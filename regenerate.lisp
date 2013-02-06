;;;; regenerate.lisp - script for regenerating esejepg.pl static HTML from template files.

(ql:quickload "cl-emb")
(ql:quickload "cl-fad")
(ql:quickload "cl-ppcre")

;;; Stuff required for EMB templates.
(defconstant +sitemap-name+ "site/sitemap.xml" "Sitemap name (with location).")
(defconstant +root-url+ "http://esejepg.pl/" "Root URL of the web page.")

;;; Regeneration code

(defparameter html-to-regenerate 
  (list
   ("templates/index.html" .  "site/index.html")
   ("templates/o-stronie.html" . "site/o-stronie.html")
   ("templates/pg.html" . "site/pomoz-z-tlumaczeniem.html")
   ("templates/pg.html" . "pytania.html")))

(defparameter css-to-regenerate
  (list
   ("cssreset-min.scss" . "cssrest-mini.css")
   ("essay.scss" . "essay.css")
   ("main.scss" . "main.css")
   ("page.scss" . "page.css")))

(defun process-template (pathname)
  "Run the template engine on selected HTML template, and return the processed code as string."
  )

(defun save-file (content pathname)
  "Save content to file at `pathname', ensuring that all the required directories exist."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (print content stream)))

(defun generate-file (pathname)
  "Generate HTML from selected template."
  (save-file (process-template pathname)
             (make-destination-path pathname))
  (format t "Regenerated successfuly: ~A (from ~A)~%" (make-destination-path pathname) pathname))

(defun del-dir-or-file-noerror (pathname)
  "Delete file, don't crash on failure."
  (format t "Deleting: ~A~%" pathname)
  (ignore-errors (fad:delete-directory-and-files pathname)))

(defun delete-old-files ()
  "Delete all generated files."
  (ignore-errors
    (fad:walk-directory "site" 'del-dir-or-file-noerror :if-does-not-exist :ignore :directories t)))


(define-condition sass-compilation-error (error)
  ((exit-code :initarg :exit-code :reader exit-code)))

(defconstant +css-root-path+ (pathname "site/css"))
(defconstant +scss-root-path+ (pathname "src/css"))

(defun generate-css (source target)
  "Generate CSS file from SASS files."
  (let* ((source-path (namestring pathname))
         (target-path (make-css-file-name name))
         (command (format nil "`which sass` --style expanded ~A:~A" name css-name)))
;;Use merge-path to get valid pathname
    (ensure-directories-exist css-name)
    (format t "Running command: ~A~%" command)
    #+LINUX(unless (= 0 (asdf:run-shell-command command))
			   (error 'sass-compilation-error))))
			  

;; sitemap
(defun get-current-date-w3c ()
  "Get current date in W3C format (YYYY-MM-DD)."
  (multiple-value-bind (s m h day month year d-o-w dst-p tz)
      (get-decoded-time)
    (declare (ignore s m h d-o-w dst-p tz))
    (format nil "~A-~2,'0d-~2,'0d" year month day)))

(defun make-sitemap-url (name)
  (ppcre:regex-replace ".*site/" (namestring name) +root-url+))


(defun html-file-p (path)
  (let ((name (namestring path)))
    (and (ppcre:scan "\.html$" name)
	 (not (ppcre:scan "index\.html$" name)))))


;; Rewrite this function
(defun generate-sitemap ()
  "Generate sitemap XML file for search engines."
  (with-open-file (stream +sitemap-name+
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-sitemap-preamble stream)
    (fad:walk-directory "site" (lambda (name) (write-sitemap-entry stream name)) :test 'html-file-p)
    (write-sitemap-postamble stream)))

(defun regenerate ()
  "Regenerate all static HTML from template files in src/ directory, and put them in site/ directory."
  (format t "Deleting old files...~%")
  (delete-old-files)

  (format t "Loading essay data...~%")
  (load "src/data/essays.lisp")
  (format t "Loaded ~A essay descriptors.~%" (length *essays*))

  (format t "Regenerating static HTML files...~%")
  (fad:walk-directory "src" 'generate-file :test 'template-to-generate-file-from-p)

  (format t "Regenerating CSS files...~%")
  (fad:walk-directory "src/css" 'generate-css :test 'sass-file-p)

  (format t "Generating sitemap...~%")
  (generate-sitemap)

  (format t "Done!~%"))
