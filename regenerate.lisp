;;;; regenerate.lisp - script for regenerating esejepg.pl static HTML from template files.

(ql:quickload "cl-emb")
(ql:quickload "cl-fad")
(ql:quickload "cl-ppcre")

;;; Stuff required for EMB templates.
(defparameter *configuration* '() "plist containing config parameters passed to EMB templates.")
(defconstant +default-properties+ '(:title nil :url nil :orig-title nil :orig-url nil :date nil :orig-date nil :alt-translations nil :translators nil :editors nil :disabled nil :additional-html nil :part-of-hnp nil :description ""))
(defconstant +sitemap-name+ "site/sitemap.xml" "Sitemap name (with location).")
(defconstant +root-url+ "http://esejepg.pl/" "Root URL of the web page.")

(defun make-environment ()
  (append (list :conf *configuration*) (list :essays *essays*)))

;;; Data processing code
;;; Used for being able to refer to essays declaratively.
(defun defessay (essay-id &rest properties)
  "Defines an essay by putting it's data into global *environment* variable, which will be an environment for
EMB templates. Please define essays from oldest to newest, to ensure proper order when iterating (from newest to
oldest."
  (append (list :id essay-id)  properties +default-properties+))

(defun create-translators (&rest translators)
  (map 'list #'(lambda (person) (list :translator person)) translators))

(defun create-editors (&rest editors)
  (map 'list #'(lambda (person) (list :editor person)) editors))

(load "src/data/essays.lisp")

;;; Regeneration code

(defun template-to-generate-file-from-p (path)
  "Returns t if given file should generate a corresponding HTML in source."
  (not (or (search "templates/" (namestring path))
           (search "data/" (namestring path))
           (search "css/" (namestring path)))))

(defun sass-file-p (path)
  "Returns t if file has .scss extension (SASS stylesheet)."
  (ppcre:scan "\.scss$" (namestring path)))

(defun file-or-dir-to-delete-p (path)
  "Returns t if given file or directory should be deleted when cleaning."
  (let ((name (namestring path)))
    (not (or (search "img/" name)
             (search "js/" name)
             (ppcre:scan ".*\.txt$" name)
             (ppcre:scan "site/$" name)))))

(defun make-group-template-path (pathname)
  "Used to select proper template based on directory processed HTML file is located in."
  (pathname (cl-ppcre:regex-replace "src/([A-Za-z]*)/.*" (namestring pathname) "src/templates/\\1\.html")))

(defun make-destination-path (pathname)
  "Where to put generated files."
  (pathname (cl-ppcre:regex-replace "src/" (namestring pathname) "site/")))

(defun process-template (pathname)
  "Run the template engine on selected HTML template, and return the processed code as string."
  (let ((content (cl-emb:execute-emb pathname
                                     :env (make-environment))))
    (cl-emb:execute-emb (make-group-template-path pathname)
                        :env (append (list :content content)
                                     (make-environment)))))

(defun save-file (content pathname)
  "Save content to file at `pathname', ensuring that all the required directories exist."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream content)))

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
    (fad:walk-directory "site" 'del-dir-or-file-noerror :test 'file-or-dir-to-delete-p :if-does-not-exist :ignore :directories t)))

(defun make-css-file-name (name)
  "Generate path for generated CSS file."
  (cl-ppcre:regex-replace "src/"
                          (cl-ppcre:regex-replace "\.scss$"
                                                  name
                                                  ".css")
                          "site/"))

(define-condition sass-compilation-error (error)
  ((exit-code :initarg :exit-code :reader exit-code)))

(defun generate-css (pathname)
  "Generate CSS file from SASS files."
  (let* ((name (namestring pathname))
         (css-name (make-css-file-name name))
         (command (format nil "`which sass` --style expanded ~A:~A" name css-name)))
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

(defun write-sitemap-preamble (stream)
  (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
  (format stream "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">~%")
  (format stream "    <url>~%")
  (format stream "        <loc>~A</loc>~%" +root-url+)
  (format stream "        <lastmod>~A</lastmod>~%" (get-current-date-w3c))
  (format stream "        <changefreq>monthly</changefreq>~%")
  (format stream "    </url>~%"))

(defun write-sitemap-entry (stream name)
  (format t "Indexing entry: ~A~%" name)
  (format stream "    <url>~%")
  (format stream "        <loc>~A</loc>~%" (make-sitemap-url name))
  (format stream "        <lastmod>~A</lastmod>~%" (get-current-date-w3c))
  (format stream "        <changefreq>monthly</changefreq>~%")
  (format stream "    </url>~%"))

(defun write-sitemap-postamble (stream)
  (format stream "</urlset>~%"))

(defun html-file-p (path)
  (let ((name (namestring path)))
    (and (ppcre:scan "\.html$" name)
	 (not (ppcre:scan "index\.html$" name)))))

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

  (format t "Cleaning template environment...~%")
  (setf *essays* '())

  (format t "Loading essay data...~%")
  (load "src/data/essays.lisp")
  (format t "Loaded ~A essay descriptors.~%" (/ (length *essays*) 2))

  (format t "Regenerating static HTML files...~%")
  (fad:walk-directory "src" 'generate-file :test 'template-to-generate-file-from-p)

  (format t "Regenerating CSS files...~%")
  (fad:walk-directory "src/css" 'generate-css :test 'sass-file-p)

  (format t "Generating sitemap...~%")
  (generate-sitemap)

  (format t "Done!~%"))
