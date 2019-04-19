;;;;			       ~*- MODE: Lisp -*-


;;;; COPY/MOVE

;;; Copy a line of text into 'next-window'
(defun acs-copy-line (&optional do-not-paste)
  "Copy a line or region of text to the kill ring, or \"next window.\"

If a region is selected, copy the region; otherwise, copy the current line.

If the universal argument is NOT given, AND there is a second window visible
in the frame, paste the selected text into the buffer returned by the 
function `next-window'. 

Otherwise, the text is copied to the kill-ring."
  (interactive "P")
  (save-excursion
    (let* ((point (point))
	   (mark (and mark-active (mark)))	   
	   (beg point)
	   (end mark))
      (if (or (not mark) 
	      (equal point mark))
	  (setq beg (and (not (beginning-of-line)) (point))
		end (and (not (end-of-line)) (point))))
      (if (or do-not-paste
	      (< (length (window-list (selected-frame))) 2))
	  (kill-ring-save beg end)
	(let ((buff (current-buffer))
	      (bufftxt (buffer-substring-no-properties beg end)))
	  (set-buffer (window-buffer (next-window)))
	  (barf-if-buffer-read-only)
	  (or
	   (and 
	    (equal mode-name "Shell")
	    (not (comint-simple-send 
		  (get-buffer-process (current-buffer)) bufftxt)))
	   (insert-before-markers bufftxt "\n"))))))
  (setq deactivate-mark t))


;;; Transpose Current Line Up
;;; i.e. Transpose current line with line above it
;;;
;;; Inspired by 'move-line' by:
;;; Mathias Dahl <brakjoller@gmail.com>
;;; Wed, 03 May 2006
;;;
(defun acs-move-line-up ()
  "Move current line up 1 line in current buffer, preserving cursor column"
  (interactive)
  (let ((col (current-column))
	(end (progn
	       (end-of-line)
	       (point)))
	(beg (progn
	       (beginning-of-line)
	       (point))))
    (cond 
     ((eq
       beg (point-min))
      (progn
	(beep)
	(message "This is the top line")
	(forward-char col)))
     ((eq
       end (point-max))
       (progn
	 (kill-line 1)
	 (previous-line 1)
	 (yank)
	 (newline)))
      (t
       (progn
	 (kill-line 1)
	 (previous-line 1)
	 (yank)
	 (previous-line 1)
	 (beginning-of-line)
	 (forward-char col))))))


;;;; DELETE REGION


;;; Delete region or kill backward word
;;; Overload c-del
;;; Use control-delete to delete region (if exists)
;;; else backward-kill-word
;;;
(defun acs-delete-region-or-backward-kill-word (&optional words-to-kill)
  "Delete the current region or kill backward word (depending on `mark-active')"
  (interactive "p")
  (let ((point (point))
	(mark (and mark-active (mark))))
    (if (and mark (not (equal point mark)))
	(delete-region (min point mark) (max point mark))
      (backward-kill-word words-to-kill))))

;;;; FILL REGION

;;; Overload m-q
;;; Use Meta-q to fill-region (if region exists), 
;;; else paragraph
;;;
;;; posted at:
;;; help-gnu-emacs@gnu.org message board
;;; Fri, 16 Jun 2006 10:21:40
;;; rayz@phonon.com
;;;
;;; Rayz revision of Giorgos Keramidas original solution...
(defun rayz-fill-region-or-paragraph (&optional justify)
  "Fill the current region or paragraph (depending on `mark-active')

Fill paragraph at or after point when the mark is inactive or if the
mark and the point are the same.  Fill each of the paragraphs in the
region when the mark is active and is not equal to the current point.

The optional JUSTIFY argument specifies the paragraph justification
that should be used.  Valid values are all those descrived in the help
of the `fill-region' function."
  (interactive "P")
  (let ((point (point))
	(mark (and mark-active (mark))))
    (message (format "justify is %s" justify))
    (if (and mark (not (equal point mark)))
	(if justify
	    (fill-region (min point mark) (max point mark) 'full)
	  (fill-region (min point mark) (max point mark)))
      (if justify
	  (fill-paragraph 'full)
	(fill-paragraph nil)))))


;;;; UN-FILL REGION

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
    (defun unfill-paragraph ()
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive)
      (let ((fill-column (point-max)))
        (fill-paragraph nil)))

 (defun unfill-region ()
      (interactive)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end) nil)))


;;;; TEXT MODE

;;; Text Mode Hook
;;; NOTE: Use c-u arg c-x f to 'set-fill-column' in buffer
;;;       Use m-q to fill paragraph
(add-hook 'text-mode-hook
	  (lambda ()
	     (progn (auto-fill-mode 1)
		    (set-fill-column 80))))


;;;; MISC

;;; From Bill Clementson's Blog
;; Always re-indent the top-level sexp
(defadvice acs-indent-sexp (around indent-defun (&optional endpos))
  "Indent the enclosing defun (or top-level sexp)."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    ad-do-it))

(ad-activate 'acs-indent-sexp)


;;; 'Electric' Return
;;; From EMACS Wiki
(defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")
electrify-return-match

(defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
	  (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))


;;; Remove Windows Unicode
;;; Rid Text Files of annoying unicode characters from windows
;;; cut-n-paste

;;; This function is usually the wrong thing to use in a Lisp program.
;;; What you probably want is a loop like this:
;;;   (while (search-forward FROM-STRING nil t)
;;;     (replace-match TO-STRING nil t))
;;; 
;;; need to go to beginning of buffer

(defun acs-convert-windows-dash ()
  (interactive)
  (query-replace "—" "-"))

(defun acs-convert-windows-closed-single-quote ()
  (interactive)
  (query-replace "’" "'"))

(defun acs-convert-windows-open-double-quote ()
  (interactive)
  (query-replace "“" "\""))

(defun acs-convert-windows-closed-double-quote ()
  (interactive)
  (query-replace "”" "\""))

