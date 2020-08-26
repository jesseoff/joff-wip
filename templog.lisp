#|
Copyright 2020 Jesse Off <jesseoff@me.com>

Distributed under the MIT license (see LICENSE file)
|#

;;  Loading this file creates an executable /tmp/temp-run that:
;;  1. Joins a zyre network to get the broadcasted ambient temperature to the group ambient-temp
;;  2. Takes CPU core temperature at 10Hz and logs it to ~/temp.csv
;;  3. Alternates the CPU loading between idle and max every 30 minutes
;;
;;  The /tmp/temp-run executable is monolithic and contains all the required .so dependency files
;;  contained in the heap image. To build the binary, when this .lisp is loaded, the first thing it
;;  does is capture a snapshot of the boilerplate .so files (libc, pthread, libm, etc..) loaded by
;;  the lisp kernel by parsing /proc/self/maps. It then does the same thing after loading all the
;;  required asdf systems and uses the set-difference to decide which .so files to "ingest" into the
;;  heap before dumping the image. Before dumping the image, all CFFI foreign libraries are closed
;;  so that the lisp kernel does not try to automatically reload the libraries. (We will do it
;;  ourselves on reloading the image)  It is important to note that /proc/self/maps must be parsed
;;  because even though the running lisp knows about its current CFFI libs, it does not know what
;;  other .so files they have brought in during their dlopen().  In this app's case, zyre is a lib
;;  that requires czmq, that requires zmq, that can require a half dozen other networking libs.
;;
;;  On image launch/restore, we create a /tmp directory to extract all our ingested .so files.
;;  After extracting, we set the LD_LIBRARY_PATH environment variable, and call the exec() syscall.
;;  exec() must be called because Linux's dynamic linker only reads LD_LIBRARY_PATH once, at
;;  startup, and then interns it for all future dlopen()
;;
;;  Once we're back into lisp after the exec() with our LD_LIBRARY_PATH pointing to our /tmp
;;  directory, we can reload the CFFI libraries.  Some systems that have hardcoded absolute
;;  pathnames for their .so file will still attempt to load their .so from the absolute path, but
;;  when they fail, a condition handler will give the LD_LIBRARY_PATH a try also.
;;
;;  Once all CFFI induced dynamic (re)linking is done from the /tmp directory, the /tmp directory is
;;  completely deleted.  Linux will not truly delete the .so's until the last process with a
;;  reference dies, so the space in /tmp is automatically cleaned up on exit.  The lisp image also
;;  sets the reference holding on to the big arrays of ingested .so files to nil for eventual GC.
;;
;;  The /tmp/temp-run executable consults argv0 to determine the name of the lisp function to call
;;  for its main.  If you symlink ambient-shouter -> temp-run, the image restore calls the
;;  ambient-shouter defun instead which will join the zyre cluster and broadcast it steady-state CPU
;;  temperature which is a constant offset off true room ambient.
;;
;;  Its worth noting that this monolithic app starts looking very much like a static linked C binary
;;  which will upset the LGPL open source community.  By setting the environment variable
;;  LGPL_WORKAROUND, the use of self-extracting /tmp for necessary .so's is skipped and the system
;;  defaults to using the Linux distribution package-managed lib.so's.

(ql:quickload :cl-ppcre)

(defun mapped-so-files ()
  "Parses Linux's /proc/self/maps to find current shared object mappings."
  (with-open-file (s #p"/proc/self/maps")
    (loop
      with files
      for l = (read-line s nil)
      while l
      for so = (cl-ppcre:scan-to-strings "[^ ]+\\.so[^ ]*$" l)
      when so do (pushnew so files :test #'equal)
        finally (return (sort files #'string<)))))

(defun so-symlinks (so-file)
  "Returns a list of super-version symlinks for given full .so filename"
  (let* ((x (cl-ppcre:split "\\.so\\." so-file))
         (vers (cadr x))
         (basename (car x)))
    (when vers
      (loop
        with vl = (cl-ppcre:split "\\." vers)
        for bl = (butlast vl) then (butlast bl)
        collect (format nil "~a.so~{.~a~}" basename bl)
        while bl))))

(defparameter *ingested-files* nil "alist, w/key being a pathname and value being an an array of
octet-vectors representing file data.  All octet vectors except the last will be 64kbytes.")

(defun ingest-file (file)
  "Saves file into heap, for later regurgitation post image restore via belch-file."
  (declare (type pathname file))
  (with-open-file (src file :element-type '(unsigned-byte 8))
    (loop
      with blks = (make-array (ceiling (file-length src) #x10000))
      for buf = (make-array #x10000 :element-type '(unsigned-byte 8))
      for i from 0
      for r = (read-sequence buf src)
      do (setf (svref blks i) buf)
      until (/= r #x10000)
      finally (setf (svref blks i) (adjust-array buf r))
              (setf *ingested-files* (acons file blks *ingested-files*)))))

(defun belch-file (file dir)
  "Copies ingested file out to filesystem, then removes it from the heap."
  (let ((d (merge-pathnames dir file))
        (file-data (cdr (assoc file *ingested-files*))))
    (with-open-file (dst d :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8))
      (loop for buf across file-data do (write-sequence buf dst)))
    (setf *ingested-files* (delete-if (lambda (x) (eql file (car x))) *ingested-files*))))

(defparameter *libs* (mapped-so-files))

(ql:quickload '(:zyre :bordeaux-threads :parse-float :trivial-features :iolib))

(defparameter *sample-rate-hz* 10)
(defparameter *soak-minutes* 30)
;;(defparameter *linux-temp* "/sys/devices/virtual/thermal/thermal_zone0/temp")
(defparameter *temperature-file* #+darwin "~/temperature" #-darwin "/temperature"
              "symlink to sysfs temperature file")
(defparameter *csv-file* "~/temp.csv")
(defparameter *internal-time-units-per-ms* (/ internal-time-units-per-second 1000))

(defun buggy-linux-temp-read (file)
  "Linux imx6ul core temperature sysfs file randomly returns -ETIMEDOUT approximately 1/20000 of
the time.  Only countermeasure is to retry."
  (loop for temp = (ignore-errors (with-open-file (tempfile file :direction :input)
                                    (/ (parse-integer (read-line tempfile nil nil)) 1000)))
        when (and temp (plusp temp)) return temp))

(defun temp-run ()
  "Logs system temperature to /tmp/temp.csv at *sample-rate-hz* hz.  Every *soak-minutes* minutes,
it alternates between high CPU load and idle."
  (format t "Logging ~a to ~a ...~%" *temperature-file* *csv-file*)
  (with-open-file (csv *csv-file* :direction :output
                                  :if-exists :overwrite
                                  :if-does-not-exist :create)
    (let (ambient)
      (handler-bind ((zyre:shout-event
                       (lambda (x) (setf ambient (/ (parse-integer (zyre:event-msg x)) 1000)))))
        (loop
          with sqrt-thread
          with start-time = (get-internal-real-time)
          with start-sqrt = (* *soak-minutes* 60 *sample-rate-hz*)
          with samples-per-test = (* start-sqrt 2)
          with end-sqrt = (1- samples-per-test)
          with zp = (zyre:zyre-pipe :group "ambient-temp")
          for i from 0
          for temp = (buggy-linux-temp-read *temperature-file*)
          for phase = (rem i samples-per-test) do
            (format csv "~f,~f~&" temp ambient)
            (cond
              ((= phase start-sqrt)
               (setf sqrt-thread (bt:make-thread (lambda () (loop (sqrt 2.0))))))
              ((= phase end-sqrt) (bt:destroy-thread sqrt-thread)))
            (let* ((next (+ start-time (* (1+ i) (/ internal-time-units-per-second 10))))
                   (delay (/ (- next (get-internal-real-time)) internal-time-units-per-second)))
              (when (plusp delay) (setf zp (zyre-sleep zp delay)))))))))

(defun ambient-shouter ()
  "Reads the system temperature at 10Hz and sends the average temperature as a shout to the zyre
group ambient-temp every 10 seconds."
  (format t "Sending ~a ambient temperature to zyre cluster ...~%" *temperature-file*)
  (let* ((zp (zyre:zyre-pipe))
         (z (zyre:pipe-first zp)))
    (loop for msg = (loop for i from 0 below 100
                          for temp = (buggy-linux-temp-read *temperature-file*)
                          sum temp into temp-acc
                          do (setf zp (zyre-sleep zp 1/10))
                          finally (return (format nil "~d" (truncate (* temp-acc 10)))))
          do (zyre:shout z "ambient-temp" msg))))

(defun osx-temp-taker ()
  "Run this function in a background thread to use the homebrew installable utility osx-cpu-temp
to populate and update a current temperature file similar to Linux's /sys file os OSX."
  (loop for temp = (uiop:run-program "osx-cpu-temp" :output :string) do
    (with-open-file (tfile *temperature-file* :direction :output :if-exists :overwrite)
      (format tfile "~d~%" (truncate (* (parse-float:parse-float temp :junk-allowed t) 1000))))
    (sleep 997/1000)))

(defparameter *cffi-libs* (mapcar #'cffi:foreign-library-name (cffi:list-foreign-libraries)))

(cffi:defcfun ("execlp" execlp) :int (file :string) &rest)
(cffi:defcfun ("symlink" symlink) :int (target :string) (linkpath :string))
(cffi:defcfun ("mkdtemp" mkdtemp) :string (template :string))

(defun belch-so-file (so-file dir)
  "Belches ingested .so file out to filesystem and creates appropriate symlinks."
  (let ((parts (multiple-value-list
                (uiop:split-unix-namestring-directory-components so-file))))
    (belch-file so-file dir)
    (loop for x in (so-symlinks so-file) do
      (symlink (caddr parts) (uiop:unix-namestring (merge-pathnames dir x))))))

(defun start ()
  "Calls the function by argv0 with the rest of the command line args as args to that function."
  (let* ((tmp (merge-pathnames (make-pathname :name ".lib-XXXXXX") (uiop:temporary-directory)))
         (args (uiop:command-line-arguments))
         (ld-path (uiop:getenv "LD_LIBRARY_PATH"))
         (exe (uiop:argv0))
         (exec-args (loop for a in args collect :string collect a))
         (exec-cmd `(execlp ,exe :string ,exe ,@exec-args :string ,(cffi:null-pointer))))
    (unless (or ld-path (uiop:getenv "LGPL_WORKAROUND"))
      (setf tmp (uiop:ensure-directory-pathname (mkdtemp (uiop:unix-namestring tmp))))
      (loop for f in *libs* do (belch-so-file f tmp))
      (setf (uiop:getenv "LD_LIBRARY_PATH") (uiop:unix-namestring tmp))
      (eval exec-cmd)
      (error "execlp() failed"))
    (setf ld-path (uiop:ensure-directory-pathname ld-path)
          *ingested-files* nil)
    (loop for lib in *cffi-libs* do
      (ignore-errors
       (handler-bind                    ;workaround foreign libs w/hardcoded .so file directories
           ((cffi:load-foreign-library-error
              (lambda (err)
                (let* ((name (pathname-name (cffi:foreign-library-pathname lib))))
                  (eval `(cffi:define-foreign-library ,lib (t (:default ,name)))))
                (cffi:load-foreign-library lib :search-path ld-path))))
         (cffi:load-foreign-library lib :search-path ld-path))))
    (uiop:delete-directory-tree ld-path :validate t)
    (apply (read-from-string (pathname-name (uiop:ensure-pathname exe))) args)))

(setf *libs* (sort (set-difference (mapped-so-files) *libs* :test #'equal) #'string<))
(loop for so in *libs* do (ingest-file so))
(mapc #'cffi:close-foreign-library *cffi-libs*)
(setf uiop:*image-entry-point* #'start)
(uiop:dump-image #p"/tmp/temp-run" :executable t)
