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
      (values (no-shorted-nets
               (nets-alist (if designator designator fpga-best-guess)))
              designator-hash))))

(defun verilog (pin-names-alist &key (toplevel-module-name "XXX_top"))
  "Generates Verilog boilerplate."
  (loop initially (format t "module ~a (~&  ~(~a~)_pad" toplevel-module-name (caar pin-names-alist))
        for (wire . pin) in (cdr pin-names-alist)
        do (format t ",~&  ~(~a~)_pad" wire)
        finally (format t "~&);~&~&"))
  (loop for (wire . pin) in pin-names-alist
        do (format t "inout ~(~a~)_pad; /* pin ~a */~&" wire pin)
        finally (format t "endmodule")))

(defun qsf (pin-names-alist)
  "Generates Quartus .qsf file boilerplate"
  (loop for (name . pin) in pin-names-alist
        do (format t "set_location_assignment PIN_~@:(~a~) -to ~(~a~)_pad~&" pin name)))

(defun lpf (pin-names-alist)
  "Generate Lattice Diamond .lpf file boilerplate"
  (loop for (name . pin) in pin-names-alist
        do (format t "LOCATE COMP \"~(~a~)_pad\" SITE \"~@:(~a~)\";~&" name pin)))
