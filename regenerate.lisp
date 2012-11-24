;; regenerate.lisp - script for regenerating esejepg.pl static HTML from template files.

(ql:quickload "cl-emb")

(defun regenerate ()
  "Regenerate all static HTML from template files in src/ directory, and put them in site/ directory."
  
  ;; TODO
  ;; 1) iterate recursively over entire src/ directory (skipping template files)
  ;; 2) use emb to generate HTML files
  ;; 3) save them to site/ directory
)
