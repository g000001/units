; unitsc.lsp               Gordon S. Novak Jr.              ; 04 Sep 03
(cl:in-package cl-user)

(defpackage "https://www.cs.utexas.edu/~novak/units"
  (:use "CL")
  (:export
   "CONVERT-UNIT"
   "SIMPLIFY-UNIT"
   "UNITP"))

(cl:in-package "https://www.cs.utexas.edu/~novak/units")

; Copyright (c) 2003 Gordon S. Novak Jr. and The University of Texas at Austin.

; This file contains some functions for stand-alone use of the units package.

; See the file gnu.license for the GNU General Public License.

; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software Foundation,
; Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; gnu@gnu.org, http://www.gnu.org/, 617 542-5942, fax 617 542-2652.

; Written by: Gordon S. Novak Jr., Department of Computer Sciences,
; University of Texas at Austin  78712.    novak@cs.utexas.edu
; http://www.cs.utexas.edu/users/novak/

(defmacro glispconstantflg    (x) `(get ,x 'glispconstantflg))
(defmacro glispconstantval    (x) `(get ,x 'glispconstantval))
(defmacro quotep (x) `(and (consp ,x) (eq (car ,x) 'quote)))

; 28 Apr 94
; Cube root
; returns a negative real root for a negative argument.
(defun cbrt (x)
  (and (numberp x) (if (>= x 0) (expt x 1/3) (- (expt (- x) 1/3)))))

; modified version for stand-alone use with units
; 27 Mar 89; 06 Jun 90; 20 May 93; 03 Jan 95; 18 Apr 03
(defun glerror (fn msgstr &rest args)
  (format t "error detected by ~A~%" fn)
  (apply #'format (cons t (cons msgstr args)))
  (terpri) )

; modified version for stand-alone use with units
; 15-Feb-89; 05 Apr 90; 12 Sep 91; 18 Sep 91; 19 Sep 91; 17 Jan 92; 03 Nov 92
; 10 Nov 95; 26 Jul 96; 18 Apr 03
; Get the value of a compile-time constant 
(defun glconstval (x)
  (cond ((or (null x)
	     (eq x t)
	     (numberp x)
	     (characterp x)
	     (stringp x))
	  x)
	((and (symbolp x) (constantp x)) (eval x))
	((quotep x) (cadr x))
	((and (symbolp x)
	      (glispconstantflg x))
	  (glispconstantval x))
	(t (error "NOMSG"))))

; 18 Apr 03
(defun glunitexpansion (u)
  (let ((flat (glunitexpand u)))
    (list '/ (cons '* (car flat)) (cons '* (cadr flat))) ))
