;;; FONTIFY WINDOW


;;; Re-center current line and re-fontify visible window
;;;
;;; Inspired by <code-snipet> from...
;;; Gary Wessle <phddas@yahoo.com>
;;;
;;; posted at:
;;; help-gnu-emacs@gnu.org message board
;;; Wed, 15 May 2006
;;;
;;; Last significant improvement: 4 June 2006
(defun acs-recenter-refontify (arg)
  "Recenter the current window and refontify lines shown in window."
  (interactive "P")
  (progn
    (recenter arg)
    (and font-lock-mode
	 (not (or
	       ;; Ignore BUFFERNAMES list
	       (member (buffer-name) (list "*info*" 
					   "*Help*"))
	       ;; Ignore MODES list
	       (member major-mode (list 'lisp-interaction-mode
					'shell-mode
					'terminal-mode))))
	 ;;	 (font-lock-fontify-block 
	 ;;	  (count-lines (window-start) (window-end))))))
	 (font-lock-fontify-buffer))))
