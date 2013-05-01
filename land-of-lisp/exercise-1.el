(setq lexical-binding t)
;;; In Common Lisp, lexical scope is the default and dynamic scope
;;  must be specially requested with either defvar or defparameter,
;;  the latter of which is not a standard library function in Emacs Lisp,
;;  and has been replaced globally with the former in all code in this package.

;;; exercise-1.el --- Summary: The first exercise from "Land of Lisp",
;;  but in Emacs Lisp rather than the textbook's Common Lisp.

;;; Commentary: see README.md

;;; Code:

;;(eval-when-compile (require 'cl))

(defvar *small* 1)

(defvar *big* 100)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (setq *small* 1) ;; defparameter doesn't exist, defvar doesn't work
  (setq *big* 100)
  (guess-my-number))

(defun play ()
  )

(provide 'exercise-1)

;;; exercise-1.el ends here
