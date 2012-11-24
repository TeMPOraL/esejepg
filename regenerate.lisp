;; regenerate.lisp - script for regenerating esejepg.pl static HTML from template files.

(ql:quickload "cl-emb")
(ql:quickload "cl-fad")
(ql:quickload "cl-ppcre")

(defun template-to-generate-file-from-p (path)
  "Returns t if given file should generate a corresponding HTML in source."
  (not (search "templates/" (namestring path))))

(defun generate-file (pathname)
  (save-file (process-template pathname)
             (make-destination-path pathname))
  (format t "Regenerated successfuly: ~A (from ~A)~%" (make-destination-path pathname) pathname))

(defun save-file (content pathname)
  (with-open-file (stream pathname :direction :output
                                   :if-exists :overwrite
                                   :if-does-not-exist :create)
    (format stream content)))

(defun process-template (pathname)
  (let ((content (cl-emb:execute-emb pathname)))
    (cl-emb:execute-emb (make-group-template-path pathname)
                        :env (list :content content))))

(defun make-group-template-path (pathname)
  (pathname (cl-ppcre:regex-replace "src/([A-Za-z]*)/.*" (namestring pathname) "src/templates/\\1\.html")))

(defun make-destination-path (pathname)
  (pathname (cl-ppcre:regex-replace "src/" (namestring pathname) "site/")))

(defun regenerate ()
  "Regenerate all static HTML from template files in src/ directory, and put them in site/ directory."
  (format t "Regenerating website...~%")
  (fad:walk-directory "src" 'generate-file :test 'template-to-generate-file-from-p)
  (format t "Done!~%"))

(defun file-test (pathname)
  (let ((in (open pathname)))
    (format t "~a~%" (read-line in))
    (close in)))
