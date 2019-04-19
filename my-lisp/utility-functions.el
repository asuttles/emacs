;;;;			       ~*- MODE: Lisp -*-


;;;; TEXT

;;; Inset current time into buffer at point
(defun acs-insert-current-time ()
  (interactive)
  (insert (current-time-string)))


;;;; COPY/PASTE

;;; Copy filename to kill-ring
;;;
;;; Copies the basename w/o file extension
;;; so that the filename can be used in
;;; NASTRAN database names.
(defun acs-copy-base-filename-as-kill ()
  "Copy the basename of the file visited by the current buffer to the kill-ring sans extension"
  (interactive)
  (let* ((this-file-with-path (buffer-file-name (current-buffer)))
	 (this-file-basename     
	  (and
	   this-file-with-path
	   (string-match "/.*/\\(.*\\)\\..*" this-file-with-path)
	   (match-string 1 this-file-with-path))))	 
    (message "Filename: %s" this-file-with-path)
    (if this-file-basename
	(kill-new this-file-basename)
      (message "Buffer %s is not visting a file." 
	       (current-buffer)))))

(defun acs-copy-filename-as-kill ()
  "Copy the filename with path to teh kill-ring with extension"
  (interactive)
  (kill-new (buffer-file-name (current-buffer))))

;;;; OTHER

;;; Buffer narrowed non-interactive predicate
;;;
(defun buffer-narrowed-p ()
  "Return t if buffer is narrowed, nil otherwise."
  (or (> (point-min) 1)
      (< (point-max) (buffer-size))))


;;; Local Variables: ***
;;; fill-column:80 ***
;;; comment-column:0 ***
;;; End: ***
