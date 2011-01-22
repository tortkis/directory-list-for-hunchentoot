
(in-package :common-lisp-user)

(defpackage :html-string
  (:use :cl)
  (:export :html-string))

(in-package :html-string)

(defun prop-list-p (lst)
  (cond ((null lst) t)
        ((atom lst) nil)
        (t (and (symbolp (car lst))
                (stringp (cadr lst))
                (prop-list-p (cddr lst))))))

(defun html-property (prop-lst)
  (let ((key (car prop-lst))
        (val (cadr prop-lst)))
    (cond ((null prop-lst) "")
          ((and (symbolp key) (stringp val))
           (format nil " ~A=\"~A\"~A"
                   (symbol-name key)
                   val
                   (html-property (cddr prop-lst))))
          (t (error "peoperty list format error: ~A" prop-lst)))))

(defun html-1tag (lst)
  (cond ((null lst) "")
        ((stringp lst)
         (format nil "~A" lst))
        ((symbolp lst)
         (format nil "<~A />~%" lst))
        ((atom lst)
         (error "format error"))
        ((and (listp (car lst))
              (symbolp (caar lst))
              (prop-list-p (cdar lst)))
         (let ((tag (caar lst)))
           (format nil "<~A~A>~{~A~}</~A>"
                   tag
                   (html-property (cdar lst))
                   (mapcar #'html-1tag (cdr lst))
                   tag)))
        ((symbolp (car lst))
         (let ((tag (symbol-name (car lst))))
           (cond ((and (>= (length tag) 3)
                       (equal (subseq tag 0 3) "!--"))
                  (format nil "<!--~{ ~A~} -->~%" (cdr lst)))
                 ((eql (char tag 0) #\!)
                  (format nil "<~A~{ ~A~}>~%" tag (cdr lst)))
                 (t (format nil "<~A>~{~A~}</~A>"
                            tag
                            (mapcar #'html-1tag (cdr lst))
                            tag)))))
        (t
         (format nil "~{~A~}"
                 (mapcar #'html-1tag lst)))))

(defun html-string (&rest body)
  (apply #'concatenate 'string (mapcar #'html-1tag body)))

;; a version using lml2                            
;; (defun html-string (&rest body)
;;   (lml2::def-std-html :button t nil)
;;   (with-output-to-string (str)
;;     (mapc #'(lambda (x) (lml2:html-print x str)) body)))

                    