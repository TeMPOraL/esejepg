;;;; regenerate.lisp - script for regenerating esejepg.pl static HTML from template files.

(ql:quickload "cl-emb")
(ql:quickload "cl-fad")
(ql:quickload "cl-ppcre")

(defparameter *configuration* '() "plist containing config parameters passed to EMB templates.")
(defparameter *essays* '() "plist containing essay descriptors generated by `defessay'.")
(defconstant +default-properties+ '(:title nil :url nil :orig-title nil :orig-url nil :date nil :orig-date nil :alt-translations nil :translators nil :editors nil :disabled nil :additional-html nil :part-of-hnp nil))

(defun make-environment ()
  (append *configuration* *essays*))

;;; Data processing code
(defun defessay (essay-id &rest properties)
  "Defines an essay by putting it's data into global *environment* variable, which will be an environment for
EMB templates. Please define essays from oldest to newest, to ensure proper order when iterating (from newest to
oldest."
  (push (append properties +default-properties+) *essays*)
  (push essay-id *essays*))

;;; Regeneration code

(defun template-to-generate-file-from-p (path)
  "Returns t if given file should generate a corresponding HTML in source."
  (not (or (search "templates/" (namestring path))
           (search "data/" (namestring path)))))

(defun sass-file-p (path)
  "Returns t if file has .scss extension (SASS stylesheet)."
  (ppcre:scan "\.scss$" (namestring path)))

(defun file-or-dir-to-delete-p (path)
  "Returns t if given file or directory should be deleted when cleaning."
  (let ((name (namestring path)))
    (not (or (search "img/" name)
             (search "js/" name)
             (search "css/" name)
             (ppcre:scan "site/$" name)))))

(defun make-group-template-path (pathname)
  (pathname (cl-ppcre:regex-replace "src/([A-Za-z]*)/.*" (namestring pathname) "src/templates/\\1\.html")))

(defun make-destination-path (pathname)
  (pathname (cl-ppcre:regex-replace "src/" (namestring pathname) "site/")))

(defun process-template (pathname)
  (let ((content (cl-emb:execute-emb pathname
                                     :env (make-environment))))
    (cl-emb:execute-emb (make-group-template-path pathname)
                        :env (append (list :content content)
                                     (make-environment)))))

(defun save-file (content pathname)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create)
    (format stream content)))

(defun generate-file (pathname)
  (save-file (process-template pathname)
             (make-destination-path pathname))
  (format t "Regenerated successfuly: ~A (from ~A)~%" (make-destination-path pathname) pathname))

(defun del-dir-or-file-noerror (pathname)
  (format t "Deleting: ~A~%" pathname)
  (ignore-errors (fad:delete-directory-and-files pathname)))

(defun delete-old-files ()
  (ignore-errors
    (fad:walk-directory "site" 'del-dir-or-file-noerror :test 'file-or-dir-to-delete-p :if-does-not-exist :ignore :directories t)))

(defun make-css-file-name (name)
  (cl-ppcre:regex-replace "\.scss$" name ".css"))

(defun generate-css (pathname)
  (let* ((name (namestring pathname))
         (command (format nil "/home/temporal/lib/sass/bin/sass --style expanded ~A:~A" name (make-css-file-name name))))
    (format t "Running command: ~A~%" command)
    #+LINUX(asdf:run-shell-command command)))

(defun regenerate ()
  "Regenerate all static HTML from template files in src/ directory, and put them in site/ directory."
  (format t "Deleting old files...~%")
  (delete-old-files)

  (format t "Cleaning template environment...~%")
  (setf *essays* '())
  (format t "Loaded ~A essay descriptors." (length *essays*))

  (format t "Loading essay data...~%")
  (load "src/data/essays.lisp")

  (format t "Regenerating static HTML files...~%")
  (fad:walk-directory "src" 'generate-file :test 'template-to-generate-file-from-p)

  (format t "Regenerating CSS files...~%")
  (fad:walk-directory "site/css" 'generate-css :test 'sass-file-p)

  (format t "Done!~%"))

(defun file-test (pathname)
  (let ((in (open pathname)))
    (format t "~a~%" (read-line in))
    (close in)))
