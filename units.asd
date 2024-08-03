;;;; srfi-236.asd

(cl:in-package :asdf)


(defsystem :units
  :version "20241122"
  :description "Unit Conversion Software"
  :long-description "Unit Conversion Software
https://www.cs.utexas.edu/~novak/units.html"
  :author "Gordon S. Novak Jr."
  :maintainer "CHIBA Masaomi"
  :serial t
  :components ((:file "unitsc")
               (:file "units")))


(defmethod perform :after ((o load-op) (c (eql (find-system :units))))
  (let ((name "https://www.cs.utexas.edu/~novak/units")
        (nickname :units))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))

;;; *EOF*
