;;;; fpgaskel.lisp

(in-package :joff)

;; A sample PADS netlist.txt file to illustrate its grammar follows:
;;
;; *SIG* ADC_CLK
;; U8.T16                U54.4
;; *SIG* ADC_CS_N
;; U8.K16                U54.6
;; *SIG* ADC_DATA
;; U8.E18                U54.5
;; *SIG* AN_1
;; C252.1                U52.3                 R81.1                 R33.2
;; *SIG* AN_2
;; C335.1                R31.2                 R80.1                 U52.1
;; *SIG* AN_3
;; U51.1                 C288.1                R82.1                 R32.2
;; *SIG* AN_3.3V
;; U51.5                 U54.1                 U52.5                 C257.1
;; U50.5                 R165.1                R79.2
;;
;; *END*     OF ASCII OUTPUT FILE
;;
;; I get the feeling from google that there is no universal format for netlist files, but
;; the above seems to be what Mentor Graphics PADS EDA software outputs in July, 2020, at least.
;;

(defun net-name-sanitize (net)
  "This translates net names to be Verilog designator safe. (remove #'s, periods, etc..)"
  (setf net (cl-ppcre:regex-replace-all "([0-9])\\.([0-9])([^_]+)" net "\\1\\3\\2")
        net (cl-ppcre:regex-replace-all "^([0-9][^_]+)_(.*)" net "\\2_\\1"))
  (multiple-value-bind (padn match) (cl-ppcre:regex-replace-all "(.*)#(.*)" net "\\1\\2_padn")
    (if match
        (setf net padn)
        (setf net (concatenate 'string net "_pad"))))
  (string-downcase net))

(defun fpga-pinlocks (netlist-asc-pathname &key designator)
  "Reads a PADS netlist.asc file and returns an alist of ((\"net-name\" . \"pin\") ... )"
  (let ((interned (make-hash-table :test 'equalp))
        (part-pins-hash (make-hash-table))
        (designator-hash (make-hash-table))
        (fpga-best-guess "u8"))
    (labels
        ((intern-strings (&rest strings)
           (loop for s in strings
                 for v = (gethash s interned)
                 if (null v) do
                   (setf (gethash s interned) s) and collect s
                 else collect v))
         (intern-string (s) (car (intern-strings s)))
         (parts (designator part &rest others)
           (when (cl-ppcre:scan "(?i)altera|lattice" part) (setf fpga-best-guess designator))
           (setf (gethash (intern-string designator) designator-hash)
                 (intern-string part))
           (when others (apply #'parts others)))
         (sig-line (net-name &rest connections)
           (when (char-equal (aref net-name 0) #\$) (return-from sig-line)) ;skip unnamed nets
           (loop for point in connections
                 for (part pin) = (apply #'intern-strings (cl-ppcre:split "\\." point))
                 do
                    (let ((pin-net-hash (gethash part part-pins-hash)))
                      (unless pin-net-hash
                        (setf pin-net-hash (make-hash-table :test 'equal)
                              (gethash part part-pins-hash) pin-net-hash))
                      (setf (gethash pin pin-net-hash) net-name))))
         (nets-alist (part-designator)
           (let ((part (gethash (intern-string part-designator) part-pins-hash)))
             (when part (loop
                          for pin being the hash-key of part
                          collect (cons (gethash pin part) pin)))))
         (shorted-nets (alist)
           (loop for (net . pin) in alist
                 if (and (member net prev-nets) (not (member net dups)))
                   collect net into dups
                 else
                   collect net into prev-nets
                 finally (return dups)))
         (no-shorted-nets (alist)
           (let ((shorts (shorted-nets alist)))
             (remove-if (lambda (x) (member (car x) shorts)) alist))))
      (with-open-file (s netlist-asc-pathname)
        (loop
          with x
          for line = (read-line s nil)
          while line
          for words = (cl-ppcre:split " +" line)
          if (and words (char-equal (aref (car words) 0) #\*)) do
            (when x
              (when (equalp "*PART*" (car x)) (apply #'parts (cddr x)))
              (when (equalp "*SIG*" (car x)) (apply #'sig-line (cdr x))))
            (setf x words)
          else do
            (setf x (nconc x words))))
      (loop for (pad . pin) in (no-shorted-nets
                                (nets-alist (if designator designator fpga-best-guess)))
            collect (cons (net-name-sanitize pad) pin)))))


(defun verilog (pin-names-alist)
  "Generates Verilog boilerplate."
  (flet
      ((inout-pad-assign-statement (pad-name)
         (let* ((p (cl-ppcre:split "_pad" pad-name))
                (n (car p)))
           (format nil "assign ~a = ~a_oe ? ~a~a : 'bz;" pad-name n (if (cdr p) "!" "") n)))
       (core-instance-connections (pad-name)
         (let* ((p (cl-ppcre:split "_pad" pad-name))
                (n (car p)))
           (format nil "  .~a_i(~a~a),~&  .~a_o(~a),~&  .~a_oe_o(~a_oe)"
                   n (if (cdr p) "!" "") pad-name
                   n n n n)))
       (core-ports (pad-name)
         (let* ((p (cl-ppcre:split "_pad" pad-name))
                (n (car p)))
           (format nil "  input ~a_i,~&  output ~a_o, ~a_oe_o" n n n))))
    (let* ((nets (mapcar #'car pin-names-alist))
           (top-ports (mapcar (lambda (x) (concatenate 'string "inout " x)) nets))
           (assigns (mapcar #'inout-pad-assign-statement nets))
           (core-ports (mapcar #'core-ports nets))
           (core-instance (mapcar #'core-instance-connections nets)))
      (format t "module top(~&~{  ~a~^,~&~}~&);~&~{~a~&~}" top-ports assigns)
      (format t "core core_inst(~&~{~a~^,~&~});~&endmodule~&" core-instance)
      (format t "module core(~&~{~a~^,~&~});~&`include \"core.v\"~&endmodule" core-ports))))

(defun qsf (pin-names-alist)
  "Generates Quartus .qsf file boilerplate"
  (format t "set_global_assignment -name FAMILY \"Cyclone IV GX\"
set_global_assignment -name DEVICE EP4CGX30CF19C8
set_global_assignment -name TOP_LEVEL_ENTITY top
set_global_assignment -name ORIGINAL_QUARTUS_VERSION 18.1.0
set_global_assignment -name PROJECT_CREATION_TIME_DATE \"09:54:01  JULY 09, 2020\"
set_global_assignment -name LAST_QUARTUS_VERSION \"18.1.0 Lite Edition\"
set_global_assignment -name PROJECT_OUTPUT_DIRECTORY output_files
set_global_assignment -name MIN_CORE_JUNCTION_TEMP 0
set_global_assignment -name MAX_CORE_JUNCTION_TEMP 85
set_global_assignment -name DEVICE_FILTER_PACKAGE FBGA
set_global_assignment -name DEVICE_FILTER_PIN_COUNT 324
set_global_assignment -name ERROR_CHECK_FREQUENCY_DIVISOR 1
set_global_assignment -name NOMINAL_CORE_SUPPLY_VOLTAGE 1.2V
set_global_assignment -name VERILOG_FILE top.v~&")
  (loop for (name . pin) in pin-names-alist
        do (format t "set_location_assignment PIN_~@:(~a~) -to ~a~&" pin name)))

(defun qpf ()
  (format t "QUARTUS_VERSION = \"18.1\"
DATE = \"09:54:01  July 09, 2020\"
PROJECT_REVISION = \"top\""))

(defun lpf (pin-names-alist)
  "Generate Lattice Diamond .lpf file boilerplate"
  (loop for (name . pin) in pin-names-alist
        do (format t "LOCATE COMP \"~a\" SITE \"~@:(~a~)\";~&" name pin)))
