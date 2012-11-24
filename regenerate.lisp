;; regenerate.lisp - script for regenerating esejepg.pl static HTML from template files.

(ql:quickload "cl-emb")
(ql:quickload "cl-fad")
(ql:quickload "cl-ppcre")

(defun template-to-generate-file-from-p (path)
  "Returns t if given file should generate a corresponding HTML in source."
  (not (search "templates/" (namestring path))))

(defun file-or-dir-to-delete-p (path)
  "Returns t if given file or directory should be deleted when cleaning."
  (let ((name (namestring path)))
    (not (or (search "img/" name)
             (search "js/" name)
             (search "css/" name)
             (ppcre:scan "site/$" name)))))

(defun generate-file (pathname)
  (save-file (process-template pathname)
             (make-destination-path pathname))
  (format t "Regenerated successfuly: ~A (from ~A)~%" (make-destination-path pathname) pathname))

(defun save-file (content pathname)
  (ensure-directories-exist pathname)
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

(defun del-dir-or-file-noerror (pathname)
  (format t "Deleting: ~A~%" pathname)
  (ignore-errors (fad:delete-directory-and-files pathname)))


(defun delete-old-files ()
  (ignore-errors
    (fad:walk-directory "site" 'del-dir-or-file-noerror :test 'file-or-dir-to-delete-p :if-does-not-exist :ignore :directories t)))

(defun regenerate ()
  "Regenerate all static HTML from template files in src/ directory, and put them in site/ directory."
  (format t "Deleting old files...~%")
  (delete-old-files)
  (format t "Regenerating website...~%")
  (fad:walk-directory "src" 'generate-file :test 'template-to-generate-file-from-p)
  (format t "Done!~%"))

(defun file-test (pathname)
  (let ((in (open pathname)))
    (format t "~a~%" (read-line in))
    (close in)))
