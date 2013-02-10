;;;; regenerate.lisp - script for regenerating esejepg.pl static HTML from template files.

(ql:quickload "cl-emb")
(ql:quickload "cl-fad")
(ql:quickload "cl-ppcre")

;;; Stuff required for EMB templates.
(defconstant +sitemap-name+ "site/sitemap.xml" "Sitemap name (with location).")
(defconstant +root-url+ "http://esejepg.pl/" "Root URL of the web page.")

(format t "Loading essay data...~%")
(load "src/data/essays.lisp")
(format t "Loaded ~A essay descriptors.~%" (length *essays*))

;;; Regeneration come

(defparameter html-to-regenerate 
  `((:layout #P"src/templates/strony.html"
		   :to #P"site/index.html"
		   :env (:title "Eseje Paula Grahama w języku polskim"
						:description "Eseje Paula Grahama w języku polskim."
						:essays ,*essays*
						:template "src/templates/index.html"))
	(:layout #P"src/templates/strony.html"
		   :to #P"site/o-serwisie.html"
		   :env (:title "Informacje o serwisie"
						:description "Czym jest serwis esejepg.pl i dlaczego powstał?"
						:template "src/templates/o-serwisie.html"))
	(:layout #P"src/templates/strony.html"
		   :to #P"site/pg.html"
		   :env (:title "Paul Graham"
						:description "Kim jest Paul Graham?"
						:template "src/templates/pg.html" ))
	(:layout #P"src/templates/strony.html"
		   :to #P"site/pytania.html"
		   :env (:title "Pytania dotyczące serwisu"
						:description "Odpowiedzi na różne pytania dotyczące serwisu esejepg.pl."
						:template "src/templates/pytania.html"))))


(defparameter css-to-regenerate
  '((:source "cssreset-min.scss"
	 :target "cssrest-mini.css")
	(:source "essay.scss"
	 :target "essay.css")
	(:source "main.scss"
	 :target "main.css")
	(:source "page.scss"
	 :target "page.css")))

(defun save-file (pathname content)
  "Save content to file at `pathname', ensuring that all the required directories exist."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (princ content stream)))

(define-condition sass-compilation-error (error))

(defun generate-css-file (descriptor)
  "Generate CSS file from SASS files."
  (defconstant +css-root-path+ "site/css/")
  (defconstant +scss-root-path+  "src/css/")
  (let* ((source (concatenate 'string +scss-root-path+ (getf descriptor :source)))
         (target (concatenate 'string +css-root-path+ (getf descriptor :target)))
         (command (format nil "`which sass` --style expanded ~A:~A" source target)))
;;Use merge-path to get valid pathname
    (ensure-directories-exist target)
    (format t "Running command: ~A~%" command)
    (unless (= 0 (asdf:run-shell-command command))  
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

(cl-emb:register-emb "include-dynamic"
					 "<%= (let ((cl-emb:*escape-type* cl-emb:*escape-type*))
                           (cl-emb:execute-emb (pathname (cl-emb::getf-emb \"template\")) :env env :generator-maker generator-maker)) %>")

(defun generate-pages ()
  "Generate general pages pages from tempaltes"
  (mapcar #'(lambda (site)
			  (save-file (getf site :to)
						 (emb:execute-emb 
						  (getf site :layout)
						  :env (getf site :env))))
			  html-to-regenerate))

(defun generate-essays ()
  "Generate essay pages"
  (format T "Generate essay")
  (mapcar #'(lambda (essay)
			  (save-file (getf essay :to)
						 (emb:execute-emb 
						  (getf essay :layout)
						  :env essay)))
		  *essays*))


(defun copy-asset-to-site (source)
  (let* ((relative-name (enough-namestring source (merge-pathnames "src/assets/")))
		 (target (merge-pathnames relative-name (merge-pathnames "site/"))))
	(ensure-directories-exist target)
	(fad:copy-file source target :overwrite t)))

(defun copy-assets ()
  (fad:walk-directory #P"src/assets" #'copy-asset-to-site))

(defun regenerate ()
  "Regenerate all static HTML from template files in src/ directory, and put them in site/ directory."
  (format t "Deleting old files...~%")
  (fad:delete-directory-and-files "site" :if-does-not-exist :ignore)

  (format t "Regenerating static HTML files...~%")
  (generate-pages)
  (generate-essays)
  (format t "Regenerating CSS files...~%")
  (generate-csss)

  (format t "Generating sitemap...~%")
  ;(generate-sitemap)

  (format t "Done!~%"))
