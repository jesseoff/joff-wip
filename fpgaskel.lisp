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
;; this seems to be Mentor Graphics PADS EDA software does in July, 2020.
;;
(defun pin-names (netlist-txt-pathname &key (designator "u8"))
  "Reads a PADS netlist.txt file and returns an alist of ((\"net-name\" . \"pin\") ... )"
  (let ((part-pins-hash (make-hash-table :test 'equal)))
    (labels
        ((sig-line (net-name &rest connections)
           (loop for point in connections
                 for (part pin) = (cl-ppcre:split "\\." point)
                 do
                    (let ((pin-net-hash (gethash part part-pins-hash)))
                      (unless pin-net-hash
                        (setf pin-net-hash (make-hash-table :test 'equal)
                              (gethash part part-pins-hash) pin-net-hash))
                      (setf (gethash pin pin-net-hash) net-name))))
         (nets-alist (part-designator)
           "Generates an alist of (net-name . pin) given a part-designator (e.g. U8)"
           (loop
             with part = (gethash (string-downcase part-designator) part-pins-hash)
             for pin being the hash-key of part
             collect (cons (gethash pin part) pin)))
         (shorted-nets (alist)
           "Returns any net-names of pins shorted together in a list"
           (loop for (net . pin) in alist
                 if (and (member net prev-nets) (not (member net dups)))
                   collect net into dups
                 else
                   collect net into prev-nets
                 finally (return dups)))
         (no-shorted-nets (alist)
           "Returns an alist with the shorted nets removed."
           (let ((shorts (shorted-nets alist)))
             (remove-if (lambda (x) (member (car x) shorts)) alist))))
      (with-open-file (s netlist-txt-pathname)
        (loop
          with x
          for line = (read-line s nil)
          while line
          for words = (cl-ppcre:split " +" (string-downcase line))
          if (and words (char-equal (aref (car words) 0) #\*)) do
            (when (and x (equal "*sig*" (car x))) (apply #'sig-line (cdr x)))
            (setf x words)
          else do
            (setf x (nconc x words))))
      (no-shorted-nets (nets-alist designator)))))

(defun verilog (pin-names-alist &key (toplevel-module-name "XXX_top"))
  "Generates Verilog boilerplate."
  (loop initially (format t "module ~a (~&  ~a_pad" toplevel-module-name (caar pin-names-alist))
        for (wire . pin) in (cdr pin-names-alist)
        do (format t ",~&  ~a_pad" wire)
        finally (format t "~&);~&~&"))
  (loop for (wire . pin) in pin-names-alist
        do (format t "inout ~a_pad; /* pin ~a */~&" wire pin)
        finally (format t "endmodule")))

(defun qsf (pin-names-alist)
  "Generates Quartus .qsf file boilerplate"
  (loop for (name . pin) in pin-names-alist
        do (format t "set_location_assignment PIN_~@:(~a~) -to ~a_pad~&" pin name)))

(defun lpf (pin-names-alist)
  "Generate Lattice Diamond .lpf file boilerplate"
  (loop for (name . pin) in pin-names-alist
        do (format t "LOCATE COMP \"~a_pad\" SITE \"~@:(~a~)\";~&" name pin)))




