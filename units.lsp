; units.lsp              Gordon S. Novak Jr.           ; 22 Nov 22
(cl:in-package "https://www.cs.utexas.edu/~novak/units")

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun convert-unit (from to)
  (let ((*package* (find-package "https://www.cs.utexas.edu/~novak/units")))
    (glconvertunit (read-from-string (princ-to-string from))
                   (read-from-string (princ-to-string to)))))

(define-compiler-macro convert-unit (&whole whole from to)
  (if (and (constantp from)
           (constantp to))
      (let ((*package* (find-package "https://www.cs.utexas.edu/~novak/units")))
        `(glconvertunit ,(read-from-string (princ-to-string from))
                        ,(read-from-string (princ-to-string to))))
      whole))

(defun simplify-unit (unit &optional system)
  (let ((*package* (find-package "https://www.cs.utexas.edu/~novak/units")))
    (glsimplifyunit (read-from-string (princ-to-string unit))
                    (read-from-string (princ-to-string system)))))

(define-compiler-macro simplify-unit (&whole whole unit &optional system)
  (if (and (constantp unit)
           (constantp system))
      (let ((*package* (find-package "https://www.cs.utexas.edu/~novak/units")))
        `(glsimplifyunit ,(read-from-string (princ-to-string unit))
                         ,(read-from-string (princ-to-string system))))
      whole))

(defun unitp (unit)
  (glunitp (read-from-string (princ-to-string unit))))

(define-compiler-macro unitp (&whole whole unit)
  (if (constantp unit)
      (let ((*package* (find-package "https://www.cs.utexas.edu/~novak/units")))
        `(glunitp ,(read-from-string (princ-to-string unit))))
      whole))    )

; Software for conversion and simplification of Units of Measurement

; Copyright (c) 2022 Gordon S. Novak Jr. and The University of Texas at Austin.

; Paper: Conversion of Units of Measurement, IEEE Trans. Software Engr.,
; vol. 21, no. 8 (Aug. 1995), pp. 651-661. 
; http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=00403789
; http://www.cs.utexas.edu/users/novak/units95.html

; See the file gnu.license for the GNU General Public License.

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.


; Floating-point numbers that are returned as conversion factors
; by this software may appear to have more digits of accuracy than are
; justified by the accuracy of the underlying conversion factors.

; Written by: Gordon S. Novak Jr., Department of Computer Sciences,
; University of Texas at Austin  78712.    novak@cs.utexas.edu
; http://www.cs.utexas.edu/users/novak/

; Thanks to Erik Enge (erik@nittin.net) for thorough testing and suggestions
; and to him and Kevin Rosenberg for making this a Debian package.

; History:
; converted from Interlisp " 1-MAR-82 17:36:34" {DSK}MEASUREMENT.LSP;8 15862
; adapted from ./physics/measu.lsp of 29 Aug 90 for use with GLISP compiler
; modified 03 June 93 for paper on units: replaced MKS by SI.
; 05 Jun 95; 15 Dec 95; 09 Mar 99; 17 Mar 99; 20 May 99; 03 May 01; 02 Apr 02
; 17 Apr 02; 08 Apr 03; 18 Apr 03; 22 May 03; 26 Jan 04; 04 Feb 04; 13 Feb 04
; 16 Mar 04; 19 Mar 04; 26 Mar 04; 08 Apr 04; 09 Apr 04; 12 Apr 04; 13 Apr 04
; 15 Apr 04; 10 May 04; 21 Jun 04; 26 May 05; 07 Jun 05; 08 Jun 05; 10 Aug 05
; 22 Aug 05; 24 Aug 05; 25 Aug 05; 08 Sep 05; 06 Dec 05; 08 Dec 05; 19 Jan 06
; 07 Feb 06; 30 Mar 06; 04 May 06; 28 Jun 06; 12 Sep 06; 03 Jan 07; 01 Jun 07
; 02 Aug 07; 07 Jan 08; 26 Jul 08; 27 Jul 09; 30 Sep 09; 07 Jan 11; 14 Jan 11
; 25 Jan 11; 18 Oct 13; 15 Feb 19

; Use the following to import the basic functions to the package where used:
; (import '(units:glconvertunit units:glsimplifyunit units:glunitp
;           units:*glunitenable* units:glmultunits units:glsimplunit
;           units:gldivunits units:gldominantunit units:glsqrtunit))

; To make the basic conversion functions have easier names, do:
; (setf (symbol-function 'convertu)  (symbol-function 'units:glconvertunit))
; (setf (symbol-function 'simplifyu) (symbol-function 'units:glsimplifyunit))
; (setf (symbol-function 'unitp)     (symbol-function 'units:glunitp))

; A unit expression is:
;   a number
;   a unit name
;   (* unit-exp1 unit-exp2 ...)
;   (/ unit-exp1 unit-exp2)
; A simplified unit expression will have at most one (/ ...) at the top.

; (setq *glunitenable* t) to enable mass -> weight, mass -> energy conversion
; *glunitmethod* is set to w2m m2w m2e e2m if this is done.

; Examples of unit conversion:  (glconvertunit <from> <to>)
; (glconvertunit 'mile 'foot)
; (glconvertunit 'kilogram 'lb)
; (glconvertunit '(/ (* atto parsec) (* micro fortnight)) '(/ inch second))
; (glconvertunit '(* acre foot) 'teaspoon)
; (glconvertunit '(/ (* 2000 kilo calorie) day) 'watt)    ; average human power
; (glconvertunit '(* 100 kgf 4 m) '(* kilo calorie)) ; calories to climb stairs
; the following require (setq *glunitenable* t)
; (glconvertunit 'kilogram 'lbf)          ; mass to weight
; (glconvertunit 'gram 'kilowatt-hour)    ; mass to energy

; Examples of unit simplification:
; (glsimplifyunit '(/ meter foot))
; (glsimplifyunit '(/ joule watt))
; (glsimplifyunit '(/ joule horsepower))
; (glsimplifyunit '(/ (* kilogram meter) (* second second)))
; (glsimplifyunit 'atm)
; (glsimplifyunit 'atm 'english)
; (glsimplifyunit '(/ (* amp second) volt))
; (glsimplifyunit '(/ (* newton meter) (* ampere second)))
; (glsimplifyunit '(/ (* volt volt) (* lbf (/ (* atto parsec) hour))))

; Example of unit conversion by GLISP compiler:
; (gldefun test (speed\:(units real (/ (* atto parsec) (* micro fortnight))))
;   (if (speed > '(q 55 mph)) (print "speeding")))

(defmacro gldimension     (x) `(get ,x 'gldimension))
(defmacro glunittype      (x) `(get ,x 'glunittype))
(defmacro glsiconversion  (x) `(get ,x 'glsiconversion))
(defmacro glabbreviations (x) `(get ,x 'glabbreviations))
(defmacro glexpansion     (x) `(get ,x 'glexpansion))
(defmacro glactualunit    (x) `(get ,x 'glactualunit))
(defmacro glstdunits      (x) `(get ,x 'glstdunits))
(defmacro glsystemunits   (x) `(get ,x 'glsystemunits))
(defmacro glactualu       (x) `(or (glactualunit ,x) ,x))
(defmacro glunitsys       (x) `(get ,x 'glunitsys))

(defvar *gldimsizes* (make-array 8 :initial-contents
				   '(20 20 20 10 10 10 10 10)))
(defvar *gldimvals*  (make-array 8))
(defvar *gldimbias*  0)
(defvar *gldimtounittype*   nil)

; 28 Apr 94
; Cube root
; returns a negative real root for a negative argument.
#|dup(defun cbrt (x)
  (and (numberp x) (if (>= x 0) (expt x 1/3) (- (expt (- x) 1/3)))))|#

; Test X to see if it represents a compile-time constant value.
; If undefined, define a simplified version.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'glconstantp)
    (defun glconstantp (x) (constantp x)))
)

; Get the value of a compile-time constant 
; If undefined, define a simplified version.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'glconstantval)
    (defun glconstantval (x) (if (constantp x) (eval x))))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'while)
    (defmacro while (test &rest forms)
      `(loop (unless ,test (return)) ,@forms)))
)

; 03 Jun 93; 16 Jul 93; 11 Nov 94
; Initialize factors for use in computing dimensions
(defun gldiminit ()
  (let ((f 1) (del 1) (bias 0))
    (dotimes (i 8)
      (setq f (* f del))
      (setf (aref *gldimvals* i) f)
      (setq del (aref *gldimsizes* i))
      (incf bias (* f (truncate del 2))) )
    (setq *gldimbias* bias) ))

(gldiminit)
  
; 04 Nov 92; 03 Jun 93; 16 Jul 93
; Convert a dimension list, which is a list of integers representing
; the powers of (mass length time charge), to an integer.
(defun gldimtoint (l)
  (let ((n 0))
    (dotimes (i 8) (incf n (* (pop l) (aref *gldimvals* i))))
    n))

; 03 Jun 93; 16 Jul 93; 26 Jul 93
; Convert a dimension integer to a dimension list
(defun glinttodim (n)
  (let ((m (+ n *gldimbias*)) l sz mm)
    (dotimes (i 8)
      (setq sz (aref *gldimsizes* i))
      (setq mm (truncate m sz))
      (push (- (- m (* mm sz)) (truncate sz 2))
	    l)
      (setq m mm))
    (nreverse l) ))

(dolist (pair '((length          1 0 0 0 0 0 0 0)
		(time            0 1 0 0 0 0 0 0)
		(temperature     0 0 1 0 0 0 0 0)
                (mass            0 0 0 1 0 0 0 0)
		(current         0 0 0 0 1 0 0 0)
		(substance       0 0 0 0 0 1 0 0)
		(luminosity      0 0 0 0 0 0 1 0)
		(money           0 0 0 0 0 0 0 1)
		(dimensionless   0 0 0 0 0 0 0 0)) )
        (setf (gldimension (car pair)) (gldimtoint (cdr pair)))
	(pushnew (list (gldimension (car pair)) (car pair))
		 *gldimtounittype*) ) 

(defvar *glunitenable* nil)  ; t to enable mass/weight, mass/energy conversions
(defvar *glunitmethod* nil)  ; which of above was used
(defvar *gldimweighttomass* (gldimtoint '( 1 -2 0 0 0 0 0 0)))
(defvar *gldimmasstoweight* (gldimtoint '(-1  2 0 0 0 0 0 0)))
(defvar *gldimmasstoenergy* (gldimtoint '(-2  2 0 0 0 0 0 0)))
(defvar *gldimenergytomass* (gldimtoint '( 2 -2 0 0 0 0 0 0)))
(defvar *gldimunitytomolar* -799997)     ; for pH = -log10(M)
(defvar *gldimmolartounity*  799997)

(defvar *glunkunits*   nil)  ; unknown units
(defvar *glunitdimerror* nil) ; error in dimensionality

; 04 Nov 92
; Define simple measurement units.  Args are unit type and a list of units.
; Each unit is specified by a list: (unit-name glsiconversion synonyms)
(defun gldefsimpleunits (unit-type units)
  (dolist (unitlist units)
    (gldefunit (first unitlist) unit-type (second unitlist) (third unitlist))))

; 04 Nov 92; 07 Jun 05
(defun gldefunit (unit unit-type factor abbrevs)
  (if (and unit-type (symbolp unit-type))
      (setf (glunittype unit) unit-type))
  (setf (gldimension unit)
	(if (symbolp unit-type)
	    (gldimension unit-type)
	    unit-type))
  (setf (glsiconversion unit) factor)
  (if abbrevs (setf (glabbreviations unit) abbrevs))
  (dolist (abbrev abbrevs) (setf (glactualunit abbrev) unit))
  unit)

; 04 Nov 92
; Define derived measurement units.  Args are unit type and a list of units.
; Unit type may be a named constant, such as mass, or nil.
; Each unit is specified by a list of: (unit-name unit-spec synonyms)
(defun gldefderivedunits (unit-type units)
  (dolist (unitlist units)
    (let ((dim (glunitdim (second unitlist))))
      (if unit-type (unless (= (gldimension unit-type) dim)
			    (error "Bad unit spec ~S" unitlist)))
      (setf (glexpansion (first unitlist)) (second unitlist))
      (gldefunit (first unitlist)
		 (or unit-type dim)
		 (glunitfactor (second unitlist))
		 (third unitlist)) )))

; 04 Nov 92; 04 May 93; 03 Jun 93; 28 Apr 95; 03 Apr 02; 18 Apr 03; 08 Dec 05
; Convert from one unit to another.
; Returns a numeric factor or nil if conversion is improper.
; sets *glunitmethod* to w2m m2w m2e e2m if special conversion done.
(defun glconvertunit (from to)
  (let (f dim)
    (setq *glunkunits* nil)
    (setq *glunitdimerror* nil)
    (if (and (glunitp from) (glunitp to)) 
	(progn
	  (setq f (/ (glunitfactor from) (glunitfactor to)))
	  (setq dim (- (glunitdim from) (glunitdim to)))
	  (setq *glunitmethod* nil)
	  (if (= dim 0)
	      f
	    (if *glunitenable*
		(cond ((= dim *gldimweighttomass*)
		        (setq *glunitmethod* 'w2m) (/ f 9.80665))
		      ((= dim *gldimmasstoweight*)
		        (setq *glunitmethod* 'm2w) (* f 9.80665))
		      ((= dim *gldimmasstoenergy*)
		        (setq *glunitmethod* 'm2e) (* f 8.987554305625E16))
		      ((= dim *gldimenergytomass*)
		        (setq *glunitmethod* 'e2m) (/ f 8.987554305625E16))
		      ((= dim *gldimunitytomolar*)
		        (setq *glunitmethod* 'u2m) (* f 1000))
		      ((= dim *gldimmolartounity*)
		        (setq *glunitmethod* 'm2u) (/ f 1000))
		      (t (setq *glunitdimerror* t) nil))
	        (progn (setq *glunitdimerror* t) nil))))) ))

; 04 Nov 92; 30 Nov 92; 03 Apr 02; 08 Apr 04
; Test whether unit is a legitimate unit specification
(defun glunitp (unit)
  (or (numberp unit)
      (and (glconstantp unit)
	   (numberp (glconstval unit)))
      (and unit
	   (symbolp unit)
	   (or (glsiconversion unit)
	       (glactualunit unit)))
      (and (consp unit)
	   (eq (car unit) '*)
	   (every #'glunitp (cdr unit)))
      (and (consp unit)
	   (eq (car unit) '/)
	   (= (length unit) 3)
	   (every #'glunitp (cdr unit)))
      (and (consp unit)
	   (eq (car unit) 'expt)
	   (= (length unit) 3)
	   (glunitp (cadr unit))
	   (integerp (caddr unit)))
      (progn (if (atom unit) (push unit *glunkunits*))
	     nil) ) )

; 07 Jun 05
; Get the base unit for a specified unit
(defun glbaseunit (unit)
  (and (symbolp unit)
       (if (glsiconversion unit)
	   unit
	   (glactualunit unit))) )

; 04 Nov 92; 03 Jun 93; 08 Apr 04; 09 Apr 04; 25 Aug 05; 08 Sep 05
; Find dimension from a unit expression.
(defun glunitdim (unit)
  (if (atom unit)
      (if (or (numberp unit) (null unit))
	  0
	  (if (symbolp unit)
	      (or (gldimension unit)
		  (gldimension (glactunit unit))
		  (and (glexpansion (glactunit unit))
		       (glunitdim (glexpansion (glactunit unit)))))
	      (error "~A is not a unit")))
      (if (eq (car unit) '*)
	  (let ((dim 0))
	    (dolist (u (cdr unit) dim)
	      (setq dim (+ dim (glunitdim u)))))
	  (if (eq (car unit) '/)
	      (- (glunitdim (cadr unit))
		 (glunitdim (caddr unit)))
	      (if (eq (car unit) 'expt)
		  (let ((dim 0))
		    (dotimes (i (abs (caddr unit)) dim)
		      (setq dim (if (minusp (caddr unit))
				    (- dim (glunitdim (cadr unit)))
				    (+ dim (glunitdim (cadr unit)))))))
		  (error "~A has bad unit operator.~%" unit))))) )

; 04 Nov 92; 08 Apr 04; 09 Apr 04; 25 Aug 05; 08 Sep 05
; Find  conversion to SI units of a unit expression.
(defun glunitfactor (unit)
  (if (atom unit)
      (if (numberp unit)
	  unit
	  (if (symbolp unit)
	      (or (glsiconversion unit)
		  (glsiconversion (glactunit unit))
		  (and (glexpansion (glactunit unit))
		       (glunitfactor (glexpansion (glactunit unit)))))))
      (if (eq (car unit) '*)
	  (let ((f 1.0))
	    (dolist (u (cdr unit) f)
	      (setq f (* f (glunitfactor u)))))
	  (if (eq (car unit) '/)
	      (/ (glunitfactor (second unit))
		 (glunitfactor (third unit)))
	      (if (eq (car unit) 'expt)
		  (let ((f 1.0))
		    (dotimes (i (abs (caddr unit)) f)
		      (setq f (if (minusp (caddr unit))
				  (/ f (glunitfactor (cadr unit)))
				  (* f (glunitfactor (cadr unit)))))))
		  (error "~A has bad unit operator.~%" unit))))) )

; 03 Apr 02; 25 Aug 05; 08 Sep 05
; get actual unit name
(defun glactunit (u)
  (and u (symbolp u)
       (if (or (glsiconversion u) (glexpansion u))
	   u
	   (and (glactualunit u)
		(glactunit (glactualunit u))))))

; 16 July 93; 19 Jul 93; 05 Mar 99; 18 Apr 03; 19 Mar 04; 26 Mar 04; 01 Apr 04
; 09 Apr 04
; Expand a unit into a flat quotient of simple factors
(defun glunitexpand (unit) (glunitexpandb unit (list nil nil) nil))
(defun glunitexpandb (unit flat flg)
  (if (atom unit)
      (if (numberp unit)
	  (if (= unit 1)
	      flat
	      (glunitpush unit flat flg))
	  (if (symbolp unit)
	      (progn
		(if (glactualunit unit) (setq unit (glactualunit unit)))
		(if (eq unit 'unity)
		    flat
		    (if (glexpansion unit)
			(glunitexpandb (glexpansion unit) flat flg)
			(if (and (gldimension unit) (= (gldimension unit) 0))
			    (glunitpush (glsiconversion unit) flat flg)
			    (glunitpush unit flat flg)))))
	      (error "~A has bad unit contents.~%" unit)))
      (case (car unit)
	(* (dolist (x (cdr unit) flat)
	     (setq flat (glunitexpandb x flat flg))) )
	(/ (glunitexpandb (second unit)
			  (glunitexpandb (third unit) flat (not flg))
			  flg) )
	(sqrt (glunitexpand (glsqrtunit (cadr unit))) )
	(cbrt (glunitexpand (glcbrtunit (cadr unit))) )
	((+ -) (if (or (null (cddr unit))
		       (glconvertunit (cadr unit) (caddr unit)))
		   (glunitexpandb (cadr unit) flat flg)
		   (error "~A has bad unit operator.~%" unit)) )
	(expt (if (numberp (caddr unit))
		  (let (lst)
		    (dotimes (i (abs (caddr unit))) (push (cadr unit) lst))
		    (glunitexpandb (if (minusp (caddr unit))
				       (list '/ 1 (cons '* lst))
				       (cons '* lst))
				   flat flg))
		  (error "~A has bad unit operator.~%" unit) ) )
	(t (error "~A has bad unit operator.~%" unit) ) ) ))

; 19 Jul 93
; Expand a unit into a flat quotient of simple factors, removing duplicates
(defun glunitexpandc (unit)
  (let (flat num den)
    (setq flat (glunitexpand unit))
    (setq num (glunitsort (first flat)))
    (setq den (glunitsort (second flat)))
    (list (glmultiset-diff num den)
	  (glmultiset-diff den num)) ))

; 16 July 93
; Push a unit onto numerator, or denominator if flg = t.
(defun glunitpush (unit flat flg)
  (if flg (push unit (second flat))
          (push unit (first flat)))
  flat)

; 30 Nov 92
; Invert a unit speciication
(defun glinvertunit (u)
  (if (and (consp u)
	   (eq (car u) '/))
      (list '/ (third u) (second u))
      (list '/ 1 u)))

; 04 Aug 93
(defun glmultunits (ua ub) (list '* ua ub))
(defun gldivunits  (ua ub) (list '/ ua ub))

; 05 Mar 99; 09 Mar 99; 03 May 01; 18 Apr 03
; Simplify a unit expression, leaving it as is when possible
(defun glsimplunit (unit)
  (let (flat)
    (if (atom unit)
	unit
        (if (and (consp unit)
		 (eq (car unit) '*)
		 (or (eq (cadr unit) 'unity)
		     (eq (caddr unit) 'unity)))
	    (if (eq (cadr unit) 'unity)
		(caddr unit)
	        (cadr unit))
	    (if (and (consp unit)
		     (eq (car unit) '/)
		     (eq (caddr unit) 'unity))
		(cadr unit)
	        (progn (setq flat (glunitexpandc unit))
		       (glflattounit (car flat) (cadr flat) 1.0)) ) ) ) ))


; 16 Jul 93; 19 Jul 93; 03 Mar 95; 04 May 06; 25 Jan 11
; Simplify a unit expression.
(defun glsimplifyunit (unit &optional system)
  (or system (setq system (or (gldominantunit unit) 'si)))
  (if (and (symbolp unit)
           (eq (glunitsys unit) system))
      unit
      (if (and (consp unit)
               (eq (car unit) '/)
               (eq (third unit) 'unity))
          (glsimplifyunit (cadr unit) system)
          (if (and (consp unit)
                   (eq (car unit) '*)
                   (eq (third unit) 'unity)
                   (null (cdddr unit)))
              (glsimplifyunit (cadr unit) system)
              (if (and (consp unit)
                       (eq (car unit) '*)
                       (eq (cadr unit) 'unity)
                       (null (cdddr unit)))
                  (glsimplifyunit (caddr unit) system)
                  (glsimplifyunitb unit system) ) ) ) ) )

(defun glsimplifyunitb (unit system)
  (let (flat (factor 1.0) num den tmp tmpb (progress t) max lng best inv)
    (setq flat (glunitexpand unit))
    (setq tmp (glsimpsystem (first flat) system))
    (setq tmpb (glsimpsystem (second flat) system))
    (setq num (glmultiset-diff (second tmp) (second tmpb)))
    (setq den (glmultiset-diff (second tmpb) (second tmp)))
    (setq factor (/ (first tmp) (first tmpb)))
    (while progress
      (setq progress nil)
      (setq max 0)
      ; (format t "num = ~A~%den = ~A~%" num den)
      (dolist (lst (glgetsystemunits system))
	(setq flat (fourth lst))
	(setq lng (+ (length (first flat)) (length (second flat))))
	(when (and (> lng 1)
		   (or (> lng max)
		       (and (= lng max) inv)))
	  (if (and (glsubmultiset (first flat) num)
		   (glsubmultiset (second flat) den))
	      (progn (setq max lng)
		     (setq inv nil)
		     (setq best lst))
	      (if (and (glsubmultiset (first flat) den)
		       (glsubmultiset (second flat) num))
		  (progn (setq max lng)
			 (setq inv t)
			 (setq best lst))))))
      (when (> max 0)
	(setq progress t)
	; (format t "best = ~A~%" best)
	(setq flat (fourth best))
	(if inv
	    (progn (setq num (glmultiset-diff num (second flat)))
		   (setq den (glunitsort
			       (cons (second best)
				     (glmultiset-diff den (first flat)))))
		   (setq factor (* factor (third best))))
	    (progn (setq num (glunitsort
			       (cons (second best)
				     (glmultiset-diff num (first flat)))))
		   (setq den (glmultiset-diff den (second flat)))
		   (setq factor (/ factor (third best)))))))
    ; (format t "num = ~A~%den = ~A~%" num den)
    (glflattounit num den factor) ))

; 19 Jul 93; 12 May 95; 30 Mar 06
; Make a unit from two flat lists
(defun glflattounit (num den factor)
    (setq num (if (glunitapprox= factor 1.0)
		  (if (cdr num) (cons '* num) (car num))
		  (if num (cons '* (cons (or (glpowerten factor) factor) num))
		      (or (glpowerten factor) factor))))	
    (setq den (if (cdr den) (cons '* den) (car den)))
    (if (and num den)
	(if (and (consp num) (eq (car num) '*) (numberp (cadr num)))
	    (list '* (cadr num)
		     (list '/ (if (cdddr num)
				  (cons '* (cddr num))
				  (caddr num))
			      den))
	    (list '/ num den))
	(or num
	    (if den (list '/ 1 den)
		'unity))) )

; 12 May 95
(defun glunitapprox= (x y) (< (abs (- x y)) 1.0e-6))

; Test if a factor is equivalent to a standard power of ten.
(defun glpowerten (x)
  (let (logx rlogx)
    (and (numberp x)
	 (plusp x)
	 (setq logx (log x 10))
	 (setq rlogx (round logx))
	 (glunitapprox= logx rlogx)
	 (cadr (assoc rlogx
		      '((30 quetta) (27 ronna)
                        (24 yotta) (21 zetta) (18 exa) (15 peta) (12 tera)
			(9 giga) (6 mega) (3 kilo) (-3 milli) (-6 micro)
			(-9 nano) (-12 pico) (-15 femto) (-18 atto)
			(-21 zepto) (-24 yocto) (-27 ronto) (-30 quecto))))) ))

; 19 Jul 93
; Convert units list to a specified system
(defun glsimpsystem (units system)
  (let ((factor 1.0) lst f new)
    (dolist (x units)
      (if (numberp x)
	  (setq factor (* factor x))
	  (progn 
	    (if (and (setq new (glsystemunit x system))
		     (not (eq new x)))
		(if (setq f (glconvertunit x new))
		    (progn (setq factor (* factor f))
			   (push new lst))
		    (error "Failed to convert ~A to ~A~%" x new))
		(push x lst)))))
    (list factor (glunitsort lst))))

; 19 Jul 93
; Sort a list of symbols alphabetically
(defun glunitsort (lst) (sort lst #'glunitsortp))
		
; 19 Jul 93
(defun glunitsortp (x y)
  (or (numberp x)
      (and (symbolp x) (symbolp y)
	   (string< (symbol-name x) (symbol-name y)))))

; 19 Jul 93
; Test if first list is a sub-multiset of the second (both sorted)
(defun glsubmultiset (seta setb)
  (or (null seta)
      (and setb
	   (if (eq (car seta) (car setb))
	       (glsubmultiset (cdr seta) (cdr setb))
	       (if (glunitsortp (car setb) (car seta))
		   (glsubmultiset seta (cdr setb)))))))

; 19 Jul 93
; Multiset difference, seta - setb (both sorted)
(defun glmultiset-diff (seta setb)
  (if seta
      (if (null setb)
	  seta
	  (if (eq (car seta) (car setb))
	      (glmultiset-diff (cdr seta) (cdr setb))
	      (if (glunitsortp (car seta) (car setb))
		  (cons (car seta) (glmultiset-diff (cdr seta) setb))
		  (glmultiset-diff seta (cdr setb)))))))

; 07 Dec 92; 17 Dec 92; 23 Jul 93; 28 Apr 94
; Divide a units list "in half" for sqrt
(defun glsqrtunit (unit &optional system noerror)
  (let (flat (factor 1.0) num den tmp tmpb ptr uniterr)
    (or system (setq system (gldominantunit unit)))
    (setq flat (glunitexpand unit))
    (setq tmp (glsimpsystem (first flat) system))
    (setq tmpb (glsimpsystem (second flat) system))
    (setq num (glmultiset-diff (second tmp) (second tmpb)))
    (setq den (glmultiset-diff (second tmpb) (second tmp)))
    (setq factor (/ (first tmp) (first tmpb)))
    (setq ptr num)
    (while (and ptr (not uniterr))
      (if (eq (car ptr) (cadr ptr))
	  (progn (rplacd ptr (cddr ptr))
		 (setq ptr (cdr ptr)))
	  (setq uniterr t)))
    (setq ptr den)
    (while (and ptr (not uniterr))
      (if (eq (car ptr) (cadr ptr))
	  (progn (rplacd ptr (cddr ptr))
		 (setq ptr (cdr ptr)))
	  (setq uniterr t)))
    (if uniterr
	(unless noerror (glerror 'glsqrtunit "bad unit ~A" unit))
        (glflattounit num den (sqrt factor))) ))

; 29 Apr 94
; Divide a units list "in thirds" for cbrt
(defun glcbrtunit (unit &optional system noerror)
  (let (flat (factor 1.0) num den tmp tmpb ptr uniterr)
    (or system (setq system (gldominantunit unit)))
    (setq flat (glunitexpand unit))
    (setq tmp (glsimpsystem (first flat) system))
    (setq tmpb (glsimpsystem (second flat) system))
    (setq num (glmultiset-diff (second tmp) (second tmpb)))
    (setq den (glmultiset-diff (second tmpb) (second tmp)))
    (setq factor (/ (first tmp) (first tmpb)))
    (setq ptr num)
    (while (and ptr (not uniterr))
      (if (and (eq (car ptr) (cadr ptr))
	       (eq (car ptr) (caddr ptr)))
	  (progn (rplacd ptr (cdddr ptr))
		 (setq ptr (cddr ptr)))
	  (setq uniterr t)))
    (setq ptr den)
    (while (and ptr (not uniterr))
      (if (and (eq (car ptr) (cadr ptr))
	       (eq (car ptr) (caddr ptr)))
	  (progn (rplacd ptr (cdddr ptr))
		 (setq ptr (cddr ptr)))
	  (setq uniterr t)))
    (if uniterr
	(unless noerror (glerror 'glcbrtunit "bad unit ~A" unit))
        (glflattounit num den (cbrt factor))) ))

; 04 Nov 92; 03 Dec 92; 07 Dec 92; 17 Dec 92
; Simplify a unit expression.
;   (glsimplifyunit '(/ (* pound-force second second) (* slug foot))) ; = unity
; Still need to do: e.g.
;   (glsimplifyunit '(/ (* kilogram meter) (* second second)))  ; = newton
; 07 Dec 92
; Get numeric factor for a unit if it is a pure number
(defun glnumfactor (unit)
  (if (numberp unit)
      unit
      (if (glconstantp unit)
	  (glconstval unit)
	  (and (symbolp unit)
	       (= (gldimension unit)
		  (gldimension 'unity))
	       (glsiconversion unit)))) )

; 04 Nov 92
; Remove the first occurrence of an item from a list.
(defun remove-first (item lst)
  (if (consp lst)
      (if (eq item (first lst))
	  (rest lst)
	  (cons (first lst) (remove-first item (rest lst))))))

; 16 July 93; 29 Sep 94
; Find the dominant unit system used in a given unit
(defun gldominantunit (unit)
  (let (pairs system (max 0) val)
    (setq pairs (gldominantunitb unit pairs))
    (dolist (pair pairs)
      (setq val (+ (cdr pair)
		   (or (cdr (assoc (car pair)
				   '((si . 0.8) (cgs . 0.5) (english . 0.2))))
		       0)))
      (when (> val max)
	(setq max val)
	(setq system (car pair))))
    system ))

; 16 July 93; 29 Sep 94; 05 Mar 99; 17 Mar 99
(defun gldominantunitb (unit pairs)
  (let (unittype tmp aunit sys)
    (if (atom unit)
	(if (symbolp unit)
	    (if (setq sys
		      (or (and (setq unittype
				 (glunittype (setq aunit (glactualu unit))))
			       (caar (member aunit (glstdunits unittype)
					     :key #'cadr)))
			  (glunitsys unit)))
		(if (setq tmp (assoc sys pairs))
		    (progn (incf (cdr tmp)) pairs)
		    (push (cons sys 1) pairs))
	        pairs)
	    pairs)
	(progn (dolist (subunit (cdr unit))
		 (setq pairs (gldominantunitb subunit pairs)))
	       pairs)) ))

; 16 July 93
; find the system of units in which unit is used, if known.
(defun glunitsystem (unit)
  (let (unittype)
    (and (symbolp unit)
	 (setq unittype (glunittype unit))
	 (some #'(lambda (x) (if (eq unit (cadr x)) (car x)))
			 (glstdunits unittype))) ))

; 16 July 93; 15 Apr 04; 04 May 06; 12 Sep 06
; find the unit to be substituted for unit in the specified system
(defun glsystemunit (unit &optional (system 'si))
   (and (symbolp unit)
	(glstandardunit (glunittype unit) system)) )

; 15 Apr 04; 04 May 06
; get the standard unit to be used for a type of unit, e.g. power
(defun glstandardunit (unittype &optional (system 'si))
  (let (tmp)
    (and (symbolp unittype)
	 (if (consp system)
	     (cadr (assoc unittype system))
	     (and (setq tmp (assoc system (glstdunits unittype)))
		  (cadr tmp)) ) ) ))

; 04 May 06
(defun glgetsystemunits (system)
  (if (symbolp system)
      (glsystemunits system)
      (mapcar #'(lambda (pair)
		  (list (car pair) (cadr pair) 1.0
			(list (list (cadr pair)) '())))
	      system)))

; 07 June 05
; find the type of an arbitrary unit
(defun glfindunittype (unit)
  (cadr (assoc (glunitdim unit) *gldimtounittype*)))

(defun glfindunittype-old (unit)
  (let (newunit)
    (if (symbolp unit)
	(glunittype (or (glactualunit unit) unit))
        (progn (setq newunit (glsimplifyunit unit))
	       (if (symbolp newunit)
		   (glfindunittype newunit)
		   (if (numberp newunit)
		       'dimensionless
		       (if (and (consp newunit)
				(eq (car newunit) '*)
				(numberp (cadr newunit))
				(symbolp (caddr newunit)))
			   (glfindunittype (caddr newunit))))))) ))

; 12 Apr 04; 02 Aug 07
(defun basictempunit (u)
  (member u '(k kelvin degK degreeK degree-kelvin
	      c centigrade celsius degC degreeC degree-celsius degree-centigrade
              f fahrenheit degF degreeF degree-fahrenheit
              r rankine degR degreeR degree-rankine)))

; 12 Apr 04
; convert temperature units.  source is (* n unit), goal is unit
(defun tempconvert (source goal)
  (let (sourcekelvin)
    (setq sourcekelvin
	  (case (caddr source)
	    ((k kelvin degree-kelvin degK) (cadr source))
	    ((c centigrade celsius degree-celsius degree-centigrade degC)
	       (+ 273.15 (cadr source)))
	    ((f fahrenheit degree-fahrenheit degF)
	       (+ 273.15 (* (/ 5 9) (- (cadr source) 32))))
	    ((rankine degR degree-rankine) (* (/ 5 9) (cadr source)))))
    (case goal
      ((k kelvin degree-kelvin degK) sourcekelvin)
      ((c centigrade celsius degree-celsius degree-centigrade degC)
        (- sourcekelvin 273.15))
      ((f fahrenheit degree-fahrenheit degF)
        (+ (* (- sourcekelvin 273.15) (/ 9 5)) 32))
      ((rankine degR degree-rankine) (* (/ 9 5) sourcekelvin)) ) ))

; 07 Jun 05
; change a KM symbol into a plain Lisp symbol: remove initial *, upper-case
(defun glunkmify (sym)
  (let ((str (symbol-name sym)))
    (intern (string-upcase (if (char= (char str 0) #\*)
			       (subseq str 1)
			       str))) ))

(defvar *kmunittypes* nil)

; 08 Sep 05
; add a km unit to known unit types
(defun defkmunittype (kmunit unit)
  (let (unittype lst)
    (setq unittype (or (glfindunittype unit) 'misc))
    (setq lst (assoc unittype *kmunittypes*))
    (if lst (pushnew kmunit (cdr lst))
        (push (list unittype kmunit) *kmunittypes*)) ))

; 07 Jun 05; 10 Aug 05; 25 Aug 05; 08 Sep 05
; define a KM unit.
; Note: basic units such as |*meter| must be defined first
; e.g. (defkmunit '|*meter| 'meter)
; e.g. (defkmunit '|*square-meter| '(* meter meter))
(defun defkmunit (kmunit unit)
  (let (system unit-type sunit newsystem tmp un f)
    (setq system (or (gldominantunit unit) 'si))
    (setq newsystem (if (eq system 'english) 'km-english 'km-si))
    (setq unit-type (glfindunittype unit))
    (setq sunit (glsimplifyunit unit))
    (if unit-type
	(progn
	  (if (not (symbolp sunit))
	      (progn (setq sunit (glunkmify kmunit))
		     (if (not (or (glexpansion sunit)
				  (eq unit sunit)))
			 (setf (glexpansion sunit) unit)) ))
	  (setf (glactualunit kmunit) sunit)
	  (pushnew kmunit (glabbreviations sunit))
	  (if (and (setq un (cadr (assoc system (glstdunits unit-type))))
		   (numberp (setq f (glconvertunit unit un)))
		   (< (abs (- f 1.0)) 0.0001))
	      (pushnew (list newsystem kmunit) (glstdunits unit-type)
		       :test #'equal))
	  (setq tmp (glunitexpandc unit))
	  (pushnew (list unit-type kmunit 1.0
			 (list
			   (mapcar #'(lambda (x) (glsimplifyunit x newsystem))
				   (car tmp))
			   (mapcar #'(lambda (x) (glsimplifyunit x newsystem))
				   (cadr tmp))))
		   (glsystemunits newsystem) :test #'equal)
	  (defkmunittype kmunit unit))
        (format t "~A does not have known unit type~%" kmunit) ) ))

; 08 Jun 05; 08 Sep 05; 01 Jun 07
(defun kmifyunit (unit)
  (let (system unittype lst f)
    (setq system (or (gldominantunit unit) 'si))
    (setq unittype (or (glfindunittype unit) 'misc))
    (setq lst (cdr (assoc unittype *kmunittypes*)))
    (or (and (member unit '(radian radians)) '|*radian|)
        (some #'(lambda (un)
		  (and (numberp (setq f (glconvertunit unit un)))
		       (< (abs (- f 1.0)) 0.0001)
		       un))
	      lst)
	(glsimplifyunit unit (if (eq system 'english) 'km-english 'km-si)) ) ))

; 28 Jun 06
; Find assumed unit, given a unit or property name
(defun glassumedunit (x)
  (if (glunitp x)
      x
      (or (get x 'assumed-unit)
	  (cadr (assoc 'si (glstdunits x))))) )

; 28 Jun 06
; Find abstract unit for a given unit
(defun glabstractunit (unit)
  (let (sim)
    (or (and (symbolp unit) (get unit 'glunittype))
	(progn (setq sim (glsimplifyunit unit 'si))
	       (if (and (consp sim)
			(eq (car sim) '*)
			(numberp (cadr sim)))
		   (setq sim (caddr sim)))
	       (or (and (symbolp sim) (get sim 'glunittype))
		   (glcopyabstractunit sim)))) ))

; 28 Jun 06
(defun glcopyabstractunit (unit)
  (if (symbolp unit)
      (glabstractunit unit)
      (if (consp unit)
	  (cons (car unit)
		(mapcar #'glcopyabstractunit (cdr unit)))
	  unit)))


(dolist (pair '((force              (/ (* mass length) (* time time)))
		(area               (* length length))
		(volume             (* length length length))
		(power              (/ (* mass length length)
				       (* time time time)))
		(energy             (/ (* mass length length) (* time time)))
		(speed              (/ length time))
                (acceleration       (/ length (* time time)))
                (pressure           (/ force area))
                (density            (/ mass volume))
                (charge             (* current time))
		(electric-potential (/ power current))
		(electric-field     (/ force charge))
		(capacitance        (/ charge electric-potential))
		(resistance         (/ electric-potential current))
		(conductance        (/ current electric-potential))
		(magnetic-field     (/ mass (* current time time)))
		(magnetic-flux      (* magnetic-field area))
		(inductance         (/ magnetic-flux current))
		(frequency          (/ 1 time))
		(time-squared       (* time time))
		(dose               (/ (* length length) (* time time)))
		(concentration      (/ substance volume))
		(flow               (/ volume time))
                (viscosity          (/ mass (* length time)))
		) )
  (let ((dim (glunitdim (second pair))))
    (setf (gldimension (first pair)) dim)
    (pushnew (list dim (first pair)) *gldimtounittype*) ))

(gldefsimpleunits 'dimensionless
               '((radian    1.0                (radians))
		 (steradian 1.0                (sr steradians))
		 (degree    0.017453292519943295   (deg degrees))
		 (arcminute 0.0002908882086657 (arcmin arcminutes arc-minute
						       arc-minutes))
		 (arcsecond 4.848136811095e-6  (arcsec arcseconds arc-second
						       arc-seconds))
		 (grad      0.015707963267948967 (grads)) ; 1/100 of right angle
		 (pi             3.1415926535897931 ())
		 (unity          1.0       ())
		 (zero           0         ())
		 (one            1         ())
		 (two            2         ())
		 (pair           2         ())
		 (three          3         ())
		 (four           4         ())
		 (five           5         ())
		 (six            6         ())
		 (seven          7         ())
		 (eight          8         ())
		 (nine           9         ())
		 (dozen          12.0      ())
		 (gross          144.0     ())
		 (ten            10.0      ())
		 (twenty         20.0      ())
		 (score          20.0      ())
		 (thirty         30.0      ())
		 (forty          40.0      ())
		 (fifty          50.0      ())
		 (sixty          60.0      ())
		 (seventy        70.0      ())
		 (eighty         80.0      ())
		 (ninety         90.0      ())
		 (hundred        100.0     ())
		 (gross          144       ())
		 (thousand       1000.0    ())
		 (million        1.0e6     ())
		 (billion        1.0e9     ())
		 (trillion       1.0e12    ())
		 (quadrillion    1.0e15    ())
		 (quintillion    1.0e18    ())
		 (percent        0.01      (\% percent))
		 (tenth          0.1       ())
		 (hundredth      0.01      ())
		 (thousandth     0.001     ())
		 (millionth      1.0e-6    ())
		 (billionth      1.0e-9    ())
		 (trillionth     1.0e-12   ())
		 (quadrillionth  1.0e-15   ())
		 (quintillionth  1.0e-18   ())
		 (quetta         1.0e30    (quetta-))
		 (ronna          1.0e27    (ronna-))
		 (yotta          1.0e24    (yotta-))
		 (zetta          1.0e21    (zetta-))
		 (exa            1.0e18    (exa-))
		 (peta           1.0e15    (peta-))
		 (tera           1.0e12    (tera-))
		 (giga           1.0e9     (giga-))
		 (mega           1.0e6     (mega-))
		 (kilo           1000.0    (kilo-))
		 (hecto          100.0     (hecto-))
		 (deka           10.0      (deca deka- deca-))
		 (deci           0.1       (deci-))
		 (centi          0.01      (centi-))
		 (milli          0.001     (milli-))
		 (micro          1.0e-6    (micro-))
		 (nano           1.0e-9    (nano-))
		 (pico           1.0e-12   (pico-))
		 (femto          1.0e-15   (femto-))
		 (atto           1.0e-18   (atto-))
		 (zepto          1.0e-21   (zepto-))
		 (yocto          1.0e-24   (yocto-))
		 (ronto          1.0e-27   (ronto-))
		 (quecto         1.0e-30   (quecto-))
		 (bit            1         (bits))
		 (byte           8         (bytes))
		 (kibi           1024      ())
		 (mebi           1048576   ())
		 (gibi           1073741824       ())
		 (tebi           1099511627776       ())
		 (pebi           1125899906842624       ())
		 (exbi           1152921504606846976       ())
		 (zebi           1180591620717411303424       ())
		 (yobi           1208925819614629174706176       ())
		 ))

(gldefsimpleunits 'length
               '((meter         1.0       (m meters metre))
		 (foot          0.3048    (ft feet))
		 (decimeter     0.1       (dm decimeters decimetre))
		 (centimeter    0.01      (cm centimeters centimetre))
		 (millimeter    0.001     (mm millimeters millimetre))
		 (dekameter     10.0      (dam dekameters decameter
					       decameters decametre))
		 (hectometer    100.0     (hm hectometers hectometre))
		 (kilometer     1000.0    (km kilometers kilometre))
		 (micron        1.0e-6    (um micro-meter micrometer
					      micrometers micro-meters
					      microns micrometre))
		 (nanometer     1.0e-9    (nm nanometers nanometre))
		 (angstrom      1.0e-10   (ang angstroms))
		 (inch          0.0254    (in inches))
		 (mile          1609.344  (mi miles))
		 (nautical-mile 1852.0    (nm nauticalmiles
					      nauticalmile nautical-miles))
                 (astronomical-unit 
                                1.49598e11 (au))
		 (light-year    9.46e15    (ly light-years
					       lightyear lightyears))
		 (parsec        3.083e16   (parsecs))
		 (fathom        1.8054     (fathoms))
		 (yard          0.9144     (yd yards))
		 (rod           5.0292     (rods))
		 (mil           0.0000254  (mils))
		 (furlong       201.168    (furlongs)) ) )

(dolist (x '(foot inch mile nautical-mile fathom yard rod furlong
	     pound slug pound-force ounce-force pound ounce ton long-ton
	     hundredweight dram grain pennyweight scruple acre square-mile
	     cubic-inch cubic-foot cubic-yard cubic-mile acre-foot gallon
	     quart peck bushel fifth pint cup fluid-ounce gill fluidram
	     minim tablespoon teaspoon foot-pound horsepower-hour grain
	     horsepower british-thermal-unit  btu pounds-per-square-inch psi
	     miles-per-hour miles-per-second feet-per-second knot
	     square-foot square-yard square-inch))
  (setf (glunitsys x) 'english))

(gldefsimpleunits 'mass
               '((kilogram         1.0           (kg kilograms))
		 (hectogram        0.1           (hg hectograms))
		 (dekagram         0.01     (dag dekagrams decagram decagrams))
		 (gram             0.001         (gm grams))
		 (decigram         0.0001        (dg decigrams))
		 (centigram        0.00001       (cg centigrams))
		 (milligram        1.0e-6        (mg milligrams))
		 (microgram        1.0e-9        (ug micrograms))
		 (metric-ton       1000.0        (metric-tons tonne tonnes))
		 (pound            0.45359237    (lb lbs pounds))    ; exactly
		 (slug             14.593902937  (slugs))
                    ; derived 02 Jun 95 based on pound, foot, and earth-gravity
		 (atomic-mass-unit 1.66053886e-27 (amu atomic-mass-units dalton))
		 (kilodalton       1.66053886e-24 (kilodaltons))
		 (earth-mass       5.98e24       ()) ) )

(gldefsimpleunits 'time
               '((second      1.0        (s sec secs seconds)) ))

(gldefderivedunits 'dimensionless
		   '((kilobyte   (* kilo byte)  (kB))
		     (megabyte   (* mega byte)  (MB))
		     (gigabyte   (* giga byte)  (GB))
		     (terabyte   (* tera byte)  (TB))
		     (petabyte   (* peta byte)  (PB))
		     (exabyte    (* exa byte)   (EB))
		     (zettabyte  (* zetta byte) (ZB))
		     (yottabyte  (* yotta byte) (YB))
		     (ronnabyte  (* ronna byte) (RB))
		     (quettabyte (* quetta byte) (QB))
		     (kibibyte   (* kibi byte)  (KiB))
		     (mebibyte   (* mebi byte)  (MiB))
		     (gibibyte   (* gibi byte)  (GiB))
		     (tebibyte   (* tebi byte)  (TiB))
		     (pebibyte   (* pebi byte)  (PiB))
		     (exbibyte   (* exbi byte)  (EiB))
		     (zebibyte   (* zebi byte)  (ZiB))
		     (yobibyte   (* yobi byte)  (YiB))
		     (kilobit    (* kilo bit)   (kbit))
		     (megabit    (* mega bit)   (Mbit))
		     (gigabit    (* giga bit)   (Gbit))
		     (terabit    (* tera bit)   (Tbit))
		     (petabit    (* peta bit)   (Pbit))
		     (exabit     (* exa bit)    (Ebit))
		     (zettabit   (* zetta bit)  (Zbit))
		     (yottabit   (* yotta bit)  (Ybit))
		     (ronnabit   (* ronna bit)  (Rbit))
		     (quettabit  (* quetta bit)  (Qbit))
		     (kibibit    (* kibi bit)   (Kibit))
		     (mebibit    (* mebi bit)   (Mibit))
		     (gibibit    (* gibi bit)   (Gibit))
		     (tebibit    (* tebi bit)   (Tibit))
		     (pebibit    (* pebi bit)   (Pibit))
		     (exbibit    (* exbi bit)   (Eibit))
		     (zebibit    (* zebi bit)   (Zibit))
		     (yobibit    (* yobi bit)   (Yibit))
		     ))

(gldefderivedunits 'time
		   '((millisecond (* milli second)     (ms msec millisec
							   milliseconds))
		     (microsecond (* micro second)     (us usec microsec
							   microseconds))
		     (nanosecond  (* nano  second)     (ns nsec nanosec
							   nanoseconds))
		     (picosecond  (* pico  second)     (ps psec picosec
							   picoseconds))
		     (femtosecond (* femto second)     (femtoseconds femtosec))
		     (attosecond  (* atto  second)     (attoseconds attosec))
		     (minute      (* 60    second)     (min minutes))
		     (hour        (* 3600  second)     (hr hours))
		     (day         (* 86400 second)     (days))
		     (week        (* 604800 second)    (wk weeks))
		     (fortnight   (* 1209600 second)   (fortnights))
		     (month       (* 2629728 second)   (mon months))
		     (year        (* 31556736 second)  (yr years))
		     (century     (* 3155673600 second) (centuries)) ) )

(gldefderivedunits 'time-squared
		   '((second-squared (* second second) (s2 s^2)) ) )

(gldefderivedunits 'frequency '((hertz    (/ 1 second) (hz))
			        (becquerel (/ 1 second) (bq)) ) )

(gldefderivedunits 'frequency
		   '((kilohertz   (* kilo hertz)       (khz))
		     (megahertz   (* mega hertz)       (mhz))
		     (gigahertz   (* giga hertz)       (ghz))
		     (terahertz   (* tera hertz)       (thz))
		     (curie       (* 3.7e10 becquerel) (curies))
		     (kilobit/second      (/ (* kilo bit) second)  (kbit/s))
		     (megabit/second      (/ (* mega bit) second)  (Mbit/s))
		     (gigabit/second      (/ (* giga bit) second)  (Gbit/s))
		     (terabit/second      (/ (* tera bit) second)  (Tbit/s))
		     (petabit/second      (/ (* peta bit) second)  (Pbit/s))
		     (exabit/second       (/ (* exa bit) second)   ())
		     (kibibit/second      (/ (* kibi bit) second)  (Kibit/s))
		     (mebibit/second      (/ (* mebi bit) second)  (Mibit/s))
		     (gibibit/second      (/ (* gibi bit) second)  (Gibit/s))
		     (tebibit/second      (/ (* tebi bit) second)  (Tibit/s))
		     (pebibit/second      (/ (* pebi bit) second)  (Pibit/s))
		     (exbibit/second      (/ (* exbi bit) second)  (Exbit/s))

 ) )

(gldefsimpleunits 'current
               '((ampere      1.0       (A amp amps amperes)) ))

(gldefderivedunits 'acceleration
                 '((earth-gravity (* 9.80665 (/ meter (* second second))))
		   (gravity (* 9.80665 (/ meter (* second second))))
		   (feet-per-second-squared (/ foot (* second second))
		     (foot-per-second-squared ft/s/s ft/sec/sec foot/sec/sec
		      ft/s2 ft/sec2 foot/second/second))
		   (meters-per-second-squared (/ meter (* second second))
		      (meter-per-second-squared m/s/s m/sec/sec m/second/second
			m/s2 m/sec2 meter/sec/sec meter/second/second))
		   (centimeters-per-second-squared
		     (/ centimeter (* second second))
		     (centimeter-per-second-squared cm/s/s cm/sec/sec cm/s2
						      cm/sec2))
		   ))

(gldefderivedunits 'current
		   '((milliampere (* milli ampere)
				  (milliamp milliamps ma milliampere))
		     (microampere (* micro ampere)
				  (microamp microamps ua microamperes))
		     (abampere    (* 10 ampere) (abamp abamperes))
		     (statampere  (* 3.336e-10 ampere) (statamp statamperes))
		     ))

(gldefderivedunits 'electric-potential
		   '((volt      (/ (* kilogram meter meter)
				   (* ampere second second second))
				                (v volts))
		     (millivolt (* milli volt)  (mv millivolts))
		     (microvolt (* micro volt)  (uv microvolts))
		     (abvolt    (* 1.0e-8 volt) (abvolts))
		     (statvolt  (* 299.8 volt)  (statvolts)) ))

(gldefderivedunits 'resistance
		   '((ohm      (/ (* kilogram meter meter)
				  (* ampere ampere second second second))
		                 (ohms))
		     (kilohm   (* kilo ohm)     (kilohms))
		     (megohm   (* mega ohm)     (megohms))
		     (abohm    (* nano ohm)     (abohms))
		     (statohm  (* 8.987e11 ohm) (statohms)) ))

(gldefderivedunits 'conductance
		   '((siemens      (/ (* ampere ampere second second second)
				      (* kilogram meter meter)) (mho) ) ))

(gldefderivedunits 'capacitance
		   '((farad   (/ (* ampere ampere second second second second)
				 (* kilogram meter meter))
			                             (farads))
		     (microfarad (* micro farad)     (uf microfarads))
		     (picofarad  (* pico farad)      (pf picofarads))
		     (abfarad    (* giga farad)      (abfarads))
		     (statfarad  (* 1.113e-12 farad) (statfarads)) ))

(gldefderivedunits 'inductance
		   '((henry      (/ (* kilogram meter meter)
				    (* ampere ampere second second))
				                    (henrys))
		     (millihenry (* milli henry)    (mh millihenrys))
		     (microhenry (* micro henry)    (uh microhenrys))
		     (abhenry    (* nano henry)     (abhenrys))
		     (stathenry  (* 8.987e11 henry) (stathenrys)) ))

(gldefderivedunits 'magnetic-flux
		   '((weber      (/ (* kilogram meter meter)
				    (* ampere second second))
				                   (wb webers))
		     (maxwell    (* 1.0e-8 weber)  (maxwells)) ))

(gldefderivedunits 'magnetic-field
		   '((tesla      (/ kilogram (* ampere second second))
				                  (teslas T))
		     (gauss      (* 1.0e-4 tesla) ())
		     (milligauss (* milli gauss)  ()) ))

; 02 Aug 07
(gldefsimpleunits 'temperature
		 '((degree-kelvin      1.0       (k kelvin kelvins degK degreeK
                                                  degC degreeC degree-celsius
                                                  degree-centigrade))
		   (degree-rankine     5/9       (rankine degR degreeR
                                                  degree-rankine degF degreeF
                                                  degree-fahrenheit)) ))

; candela = 1/683 watt/steradian at 540E12 Hz (wavelength 550 nm)
; 100 watt incandescent bulb = 150 candela
; lumen = 1 candela * 1 steradian = 1/683 watt at wavelength 550 nm
; lux = 1 lumen / square-meter                      illuminance
; footcandle = 1 lumen / square-foot = 10.76 lux
; nit = candela / square-meter

(gldefsimpleunits 'luminosity
		 '((candela            1.0       (cd candelas)) ))

(gldefsimpleunits 'substance
		 '((mole               1.0       (mol moles)) ))

(gldefsimpleunits 'money
		 '((dollar             1.0       (dollars $))
                   (euro               1.33      (eu euros))
		   (cent               0.01      (cents penny pennies))
		   ))

(gldefderivedunits 'force
                 '((pound-force  (/ (* slug foot) (* second second)) (lbf))
		   (ounce-force  (/ pound-force 16)        ())
		   (newton (/ (* kilogram meter) (* second second))
			    (N nt newtons))
		   (dyne   (/ (* gram centimeter) (* second second))
			    (dynes))
		   (kilogram-force  (* kilogram earth-gravity)
				    (kgf kilogram-weight))
		   (gram-weight     (* gram earth-gravity) (gram-force)) ))

(gldefderivedunits 'mass
		 '((ounce  (/ pound 16)
			    (oz ounces))
		   (ton    (* 2000 pound)
			    (tons short-ton short-tons))
		   (long-ton (* 2240 pound)
			    (tons long-ton long-tons))
		   (hundredweight (* 100 pound) (hundredweights))
		   (dram   (/ ounce 16) (drams))
		   (grain  (/ dram 27.344) (grains))
		   (troy-pound (* 0.373 kilogram) (troy-pounds))
		   (troy-ounce (* 31.103 gram)
			       (troy-ounces ounce-troy ounces-troy))
		   (pennyweight (* 1.555 gram) (pennyweights))
		   (scruple (* 1.296 gram) (scruples))
		   ))

(gldefderivedunits 'area
                 '((square-meter (* meter meter)
				 (m^2 m2 meter-squared meters-squared 
				      metersquared square-meters))
		   (square-centimeter (* centimeter centimeter)
				      (cm^2 centimetersquared
					    centimeters-squared
					    centimeter-squared
					    square-centimeters))
		   (square-foot (* foot foot)
				(ft^2 foot-squared feet-squared footsquared
				      feetsquared square-feet))
		   (square-yard (* yard yard)
				(yard^2 yard-squared yardsquared yards-squared
					square-yards yd^2))
		   (square-inch (* inch inch)
				(in^2 inch-squared inchsquared inches-squared
				      square-inches))
		   (hectare (* 10000 metersquared)
			    (hectares))
		   (are     (* 100 metersquared) (ares))
		   (acre (* 43560 footsquared)
			 (acres))
		   (square-mile (* mile mile)
				(mile^2 mile-squared miles-squared milesquared
					square-miles mi^2))
		   (square-kilometer (* kilometer kilometer)
				     (km^2 kilometer-squared
					   kilometers-squared
					   kilometersquared
					   square-kilometers))
		   (square-millimeter (* millimeter millimeter)
				      (mm^2 millimeter-squared
					    millimeters-squared
					    millimetersquared
					    square-millimeters))
		   (square-micron (* micrometer micrometer)
				      (um^2 micrometer-squared
					    micrometers-squared
					    micron-squared microns-squared
					    micrometersquared
					    micronsquared square-microns))
		   (barn (* 1.0e-28 metersquared) (barns))
                   (darcy (* 9.869233e-13 metersquared) (darcies))
                   (millidarcy (* milli darcy) (millidarcies md))
		   ))

(gldefderivedunits 'volume
		 '((cubic-meter     (* meter meter meter)
			            (m^3 meter-cubed metercubed meters-cubed
					 cubic-meters kiloliter kiloliters
					 kilolitre))
		   (cubic-centimeter (* centimeter centimeter centimeter)
				    (cm^3 centimeter-cubed centimeters-cubed
					  centimetercubed centimeterscubed
					  cubic-centimeters milliliter
					  milliliters ml cc
					  cubic-centimetre millilitre))
		   (cubic-millimeter (* millimeter millimeter millimeter)
				    (mm^3 millimeter-cubed millimeters-cubed
					  millimetercubed millimeterscubed
					  cubic-millimeters cubic-millimetre))
		   (cubic-micron     (* micron micron micron)
				     (micron-cubed microns-cubed
						   cubic-microns))
		   (cubic-kilometer  (* kilometer kilometer kilometer)
				    (km^3 kilometer-cubed kilometers-cubed
					  kilometercubed kilometerscubed
					  cubic-kilometers cubic-kilometre))
		   (cubic-inch       (* inch inch inch)
			            (in^3 inch-cubed inchcubed inchescubed
					  cubic-inches))
		   (cubic-foot       (* foot foot foot)
			            (ft^3 foot-cubed footcubed feetcubed
					  cubic-feet))
		   (cubic-yard       (* yard yard yard)
			            (yd^3 yard-cubed yardcubed yardscubed
					  yards-cubed cubic-yards))
		   (cubic-mile      (* mile mile mile)
			            (mile^3 mile-cubed miles-cubed
					     cubic-miles mi^3))
		   (acre-foot        (* acre foot)
			            (acrefoot acre-feet acrefeet))
		   (liter           (* 0.001 metercubed)
			            (l liters litre cubic-decimeter
				       cubic-decimeters))
		   (deciliter       (/ liter 10)
			            (dl deciliters decilitre))
		   (centiliter      (/ liter 100) (cl centiliters centilitre))
		   (dekaliter       (* liter 10)
				    (dekaliters decaliter decaliters decalitre
						dekalitre))
		   (hectoliter      (* 100 liter) (hectoliters hectolitre))
		   (gallon          (* 3.785411784 liter) (gal gallons))
		   (quart           (/ gallon 4) (qt quarts))
                   ; dry bushel as used in measuring grain
                   ; 36.75 bushels wheat = 1 ton
		   (bushel          (* 2150.42 cubic-inch) (bushels))
		   (peck            (/ bushel 4) (pecks))  ; dry peck as in grain
		   (fifth           (/ gallon 5) (fifths))
		   (pint            (* 0.473 liter) (pt pints))
		   (cup             (/ pint 2) (cups))
		   (fluid-ounce     (* 0.029573 liter)
			            (floz fluidounce fluidounces fluid-ounces))
		   (gill            (* 4 fluid-ounce) (gills))
		   (fluidram        (* 3.5516 cubic-centimeter) (fluidrams))
		   (minim           (* 0.059194 cubic-centimeter) (minims))
		   (tablespoon      (/ fluidounce 2) (tbsp tablespoons))
		   (teaspoon        (/ tablespoon 3) (tsp teaspoons))
		   (barrel          (* 159 liter) (bbl))  ; as in oil
		   ) )

(gldefderivedunits 'power
                 '((watt       (/ (* kilogram meter meter)
				  (* second second second))
			       (w watts))
		   (milliwatt  (* milli watt)
			       (mw milliwatts milli-watt milli-watts))
		   (microwatt  (* micro watt)
			       (uw microwatts micro-watt micro-watts))
		   (kilowatt   (* kilo watt)
			       (kw kilowatts kilo-watt kilo-watts))
		   (megawatt   (* mega watt)
			       (mw megawatts mega-watt mega-watts))
		   (gigawatt   (* giga watt)
			       (gw gigawatts giga-watt giga-watts))
		   (terawatt   (* tera watt)
			       (tw terawatts tera-watt tera-watts))
		   (petawatt   (* peta watt)
			       (pw petawatts peta-watt peta-watts))
		   (horsepower (* 550 (/ (* foot pound-force) second))
			       (hp pferdstaerke cheval)) ) )

(gldefderivedunits 'energy
		 '((joule (/ (* kilogram meter meter) (* second second))
			  (j joules))
		   (kilojoule  (* kilo joule)
			       (kilojoules kilo-joule kilo-joules kj))
		   (megajoule  (* mega joule)
			       (megajoules mega-joule mega-joules mj))
		   (terajoule  (* tera joule)
			       (terajoules tera-joule tera-joules tj))
		   (petajoule  (* peta joule)
			       (petajoules peta-joule peta-joules pj))
		   (foot-pound (* foot pound-force)
			      (ftlb ft-lb footpound footpounds foot-pounds))
		   (kilowatt-hour (* kilo watt hour)
				 (kwh kilowatthour kilowatthours
				      kilowatt-hours))
		   (megawatt-hour (* mega watt hour)
				 (mwh megawatthour megawatthours
				      megawatt-hours mega-watt-hour
				      mega-watt-hours))
		   (watt-hour (* watt hour)
			     (watthour watthours watt-hours))
		   (horsepower-hour (* horsepower hour)
				    (hp-hour pferdstaerkenstunde
                                             cheval-vapeur-heure))
		   (electron-volt (* 1.602176565e-19 joule)
				  (ev electronvolt electronvolts
				      electron-volts))
		   (mev (* 1.602176565e-13 joule)
			(mega-electron-volts))
		   (gev (* 1.602176565e-10 joule)
			(giga-electron-volts))
		   (tev (* 1.602176565e-7 joule)
			(tera-electron-volts))
		   (hartree-energy (* 4.35974417e-18 joule) (E_h))
		   (calorie (* 4.184 joule)
			    (cal calorie calories))
		   (kilocalorie (* 4184.0 joule)
				(kcal kilo-calorie kilo-calories))
		   (british-thermal-unit (* 1055.056 joule)
				       (btu btus britishthermalunit
					    britishthermalunits
					    british-thermal-units))
		   (erg (* 1.0e-7 joule)
			(ergs))
		   (gallon-gasoline (* 114100 BTU) (gallon-gas gal-gas))
                   (mbtu (* mega btu) (mmbtu))
                   (quad (* quadrillion btu) (quads))
                   (therm (* 100000 btu) (therms))
                   (mcf-ch4 (* 1027000 btu) (mcf))   ; 1000 cubic feet of ch4
                   (mmcf-ch4 (* kilo mcf-ch4) (mmcf))
                   (bcf-ch4 (* mega mcf-ch4) (bcf))  ; billion cubic feet ch4
                   (ton-coal (* 29.29e9 joule) (tce tonne-coal))
                         ; metric ton of coal equivalent
                         ; http://www.maproyalty.com/conversions.html
                   (barrel-oil (* 6119e6 joule) (barrels-oil boe))
                         ; barrel of oil equivalent
                   (ton-oil (* 44.76e9 joule) (tons-oil toe))
                         ; metric ton of oil equivalent
                   (cubic-meter-ch4 (* 37.26e6 joule))
                   (bcm-ch4 (* billion cubic-meter-ch4) (bcm))
                   (mcm-ch4 (* million cubic-meter-ch4) (mcm mmcm mmcm-ch4))
                   (kcm-ch4 (* kilo cubic-meter-ch4) (kcm))
                   (terawatt-year (* tera watt year) (terawatt-years twyr))
                   (ton-tnt (* 4.185e9 joule) (tons-tnt))
                   (kiloton-tnt (* kilo ton-tnt) (kilotons-tnt))
                   (megaton-tnt (* mega ton-tnt) (megatons-tnt))
                   (aa-battery (* 9900 joule) ())
  ) )

(gldefderivedunits 'charge
  '((coulomb     (* ampere second)     (coul coulombs C))
    (microcoulomb (* micro coulomb)    (micro-coulomb uC))
    (nanocoulomb (* nano coulomb)      (nano-coulomb nC))
    (abcoulomb   (* 10.0 coulomb)      (abcoul abcoulombs))
    (statcoulomb (* 3.336e-10 coulomb) (statcoul statcoulombs))
    (amperehour  (* 3600.0  coulomb)   (amp-hour ampere-hour
				        amperehours ampere-hours))
    (electron-charge (* 1.602176565e-19 coulomb) )
 ))

(gldefderivedunits 'electric-field
  '((newton-per-coulomb (/ newton coulomb) (N/C newton/coulomb))
    (volts-per-meter    (/ volt meter)     (v/m volt/meter)) ) )

(gldefderivedunits 'pressure
  '((pounds-per-square-inch (/ (* 144 pound-force) (* foot foot)) (psi))
    (pascal     (/ newton (* meter meter)) (pa))
    (kilopascal (* 1000.0 pascal) (kilo-pascal kpa kilopascals))
    (bar        (* 1.0e5 pascal)  (bars))
    (millibar   (* milli bar)     (millibars))
    (torr       (* (/ 101325 760) pascal) ())
    (dynes-per-square-centimeter (/ dyne (* centimeter centimeter)))
    (atmosphere (* 101325 pascal) (atm)) ))

(gldefderivedunits 'viscosity
                 '((pascal-second   (* pascal second) (pascal-seconds pas))
                   (poise           (/ gram (* centimeter second)) ())
                   (centipoise      (* centi poise) ()) ) )

(gldefderivedunits 'speed
                 '((miles-per-hour (/ mile hour) (mph mile-per-hour))
		   (miles-per-second (/ mile second) (mile-per-second))
		   (kilometers-per-hour (/ kilometer hour)
					(kph kilometer-per-hour))
		   (kilometers-per-second (/ kilometer second)
					  (kps kilometer-per-second))
		   (feet-per-second (/ foot second)
				    (foot-per-second fps ft/s ft/sec foot/sec
						     ft/second foot/second))
		   (meters-per-second (/ meter second)
				      (meter-per-second m/s m/sec m/second
				       meter/sec meter/second))
		   (centimeters-per-second (/ centimeter second)
					   (centimeter-per-second cm/s cm/sec))
		   (knot              (/ nautical-mile hour) (knots))
		   (speed-of-light    (* 299792458 (/ meter second)))
		   ))

(gldefderivedunits 'dose      ; of radiation
		   '((gray    (/ joule kilogram)   (gy))
		     (sievert (/ joule kilogram)   (sv))
		     (rad     (/ gray 100)         ())
		     (rem     (/ sievert 100)      ()) ))

(gldefderivedunits 'concentration
		   '((molar (/ mole liter) ()) ) )

(gldefderivedunits 'flow
		   '((cubic-meter-per-second (/ (* meter meter meter) second)
					     (cubic-meters-per-second))
		     (gallon-per-second      (/ gallon second)
					     (gallons-per-second))
		     (sverdrup               (/ (* mega meter meter meter)
						second)
					     (sverdrups))
		     (acre-foot-per-day      (/ acre-foot day)
					     (acre-feet-per-day)) ))

(dolist (x '(si cgs english)) (setf (glsystemunits x) nil))

(dolist (pair '((length   (si meter)        (cgs centimeter) (english foot))
		(mass     (si kilogram)     (cgs gram)       (english slug))
		(time     (si second)       (cgs second)     (english second))
		(force    (si newton)       (cgs dyne)    (english pound-force))
		(area     (si square-meter) (cgs square-centimeter)
		          (english square-foot))
		(volume   (si cubic-meter)  (cgs cubic-centimeter)
		          (english cubic-foot))
		(power    (si watt)         (cgs watt)  (english horsepower))
		(energy   (si joule)        (cgs erg)   (english foot-pound))
		(pressure (si pascal)       (cgs dynes-per-square-centimeter)
			  (english pounds-per-square-inch))
		(speed               (si meters-per-second)
				     (cgs centimeters-per-second)
				     (english feet-per-second))
                (acceleration        (si meters-per-second-squared)
				     (cgs centimeters-per-second-squared)
				     (english feet-per-second-squared))
                (density             (si ) (cgs ) (english ))
                (charge              (si coulomb) (cgs ) (english ))
		(electric-potential  (si volt) (cgs ) (english ))
		(current             (si ampere) (cgs ) (english ))
		(capacitance         (si farad) (cgs ) (english ))
		(resistance          (si ohm) (cgs ) (english ))
		(conductance         (si siemens) (cgs ) (english ))
		(magnetic-field      (si tesla) (cgs ) (english ))
		(magnetic-flux       (si weber) (cgs ) (english ))
		(inductance          (si henry) (cgs ) (english ))
		(concentration       (si molar))
		(substance           (si mole))
		))
  (setf (glstdunits (car pair)) (cdr pair))
  (dolist (pr (cdr pair))
    (let (tmp)
      (when (cdr pr)
	(setq tmp (glunitexpandc (cadr pr)))
	(push (list (car pair) (cadr pr)
		    (if (numberp (caar tmp)) (caar tmp) 1.0)
		    (if (numberp (caar tmp)) (cons (cdar tmp) (cdr tmp)) tmp))
	      (glsystemunits (car pr)))) ) ))

(defconstant *speed-of-light*    '(q 2.99792458e8 (/ meter second)))
(defconstant *gravitational-constant*
             '(q 6.6720e-11  (/ (* meter meter meter)
			     (* kilogram second second))))
(defconstant *elementary-charge* '(q 1.602176565-19 coulomb))
(defconstant *electron-mass*     '(q 9.109534e-31 kilogram))
(defconstant *earth-gravity*     '(q 9.80665 (/ meter (* second second))))
