
(in-package :cl-user)

(defpackage :directory-list
  (:use :cl)
  (:export
   :define-directory-list))

(defmacro define-directory-list (uri base-path &key (show-path nil)
                                 (default-content-type "text/plain; charset=utf-8"))
  (let ((handler-name (intern (string-upcase (format nil "dir-list-~A" uri)))))
    `(progn
       (push (hunchentoot:create-prefix-dispatcher ,uri ',handler-name)
             hunchentoot:*dispatch-table*)
       (defun ,handler-name ()
         (let* ((rel-path (or (hunchentoot:parameter "path") "/"))
                (full-path (format nil "~A~A" ,base-path rel-path))
                (dirp (if rel-path (eql (car (last (coerce rel-path 'list))) #\/)))
                (files (if (and rel-path dirp)
                           (directory (pathname (namestring (format nil "~A*.*" full-path)))))))
           (cond (dirp
                  (html-string:html-string
                   `(:html
                     (:head (:title ,(format nil "Directory List: ~A" rel-path)))
                     (:body
                      (:b ,(format nil "Directory List: ~A" rel-path))
                      (:table
                       ,(if (and dirp (> (length (remove #\/ rel-path)) 0))
                            (let* ((p0 (position #\/ (reverse rel-path) :start 1))
                                   (p1 (if p0 (- (length rel-path) p0))))
                              `(:tr (:td ((:a :href ,(format nil "~A?path=~A"
                                                             ,uri (if p1 (subseq rel-path 0 p1) "/")))
                                          "(upper directory)"))))
                            "")
                       ,@(if files
                             (mapcar #'(lambda (file)
                                         (let* ((path (subseq (namestring file) ,(length base-path)))
                                                (name (if (pathname-name file)
                                                          (if (pathname-type file)
                                                              (format nil "~A.~A"
                                                                      (pathname-name file) (pathname-type file))
                                                              (pathname-name file))
                                                          (format nil "~A/" (car (last (pathname-directory file)))))))
                                           `(:tr (:td ((:a :href ,(format nil "~A?path=~A" ,uri path))
                                                       ,(if ,show-path path name))))))
                                     files)
                             (list "NA")))))))
                 (t (hunchentoot:handle-static-file full-path
                                                    (or (hunchentoot:mime-type full-path)
                                                        ,default-content-type)))))))))