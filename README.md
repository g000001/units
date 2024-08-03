# Unit Conversion Software
----
Author: Gordon S. Novak Jr.

This free software, written in Common Lisp, can convert a given unit of measurement into an equivalent unit. The input can be named units or expressions in terms of units. The software can also simplify unit expressions.

```common-lisp
(in-package units)

(convert-unit 'meters 'feet)
→ 3.28084 

(convert-unit '(/ (* atto parsec)
                  (* micro fortnight))
              '(/ inch sec))
→ 1.0034554 

(convert-unit '(/ (* mega lbf) acre)
              'kilopascals)

1.0991794 

(simplify-unit '(/ (* newton meter) (* ampere second)))
→ volt 


(simplify-unit '(/ (* volt volt)
                   (* lbf (/ (* atto parsec) hour))))
→ (* 26250.799 ohm) 
```

* [Unit Conversion: On-line Demonstration](https://www.cs.utexas.edu/users/novak/cgi/unitsdemoty.cgi)
* [Directory for software and documentation.](https://www.cs.utexas.edu/users/novak/units/)
* [``Conversion of Units of Measurement''](https://www.cs.utexas.edu/~novak/units95.html) paper. 
