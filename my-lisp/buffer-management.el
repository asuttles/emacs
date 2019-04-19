;;;;			       ~*- MODE: Lisp -*-



;;;; DELETE UNWANTED BUFFERS


;;; Kill the current buffer and it's associated frame
(defun acs-kill-buffer-and-frame ()
  "Kill the current buffer and associated frame."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-frame))


;;; Delete those annoying completions buffers
(defun acs-delete-completions-buffer ()
  (interactive)
  (if
      (member "*Completions*"
	      (mapcar (function
		       (lambda (bn)
			 (buffer-name bn)))
		      (buffer-list)))
      (kill-buffer "*Completions*")
    (message "No *Completions* buffer")))



;;; Remove completions Buffer Automatically
;;;
;;; originally posted at:
;;; help-gnu-emacs@gnu.org message board
;;; Mon, 05 Mar 2007 21:30:39
;;; Kevin Rodgers <kevin.d.rodgers@gmail.com>
;;;
;;; <Modified by acs as shown below>
;;;
;;; NOTE: This code eliminates the neccessity for 
;;;       "acs-delete-completions-buffer"
;;;
(defadvice comint-send-input (after undisplay-completions activate)
  "If the *Completions* buffer is displayed in the selected frame, 
restore the previous window configuration."
;;;  (when (and (get-buffer-window "*Completions*")
;;;	     comint-dynamic-list-completions-config)
;;;	     (set-window-configuration comint-dynamic-list-completions-config)))
  (let ((buff (get-buffer "*Completions*")))
    (if buff (kill-buffer buff)
      (message "No *Completions* buffer"))))



;;; Create a *scratch* buffer for lisp interaction
(defun acs-make-scratch-buffer ()
  (interactive)
  (if (member "*scratch*"
	      (mapcar (function
		       (lambda (bn)
			 (buffer-name bn)))
		      (buffer-list)))
      (switch-to-buffer "*scratch*")
    (progn
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (lisp-interaction-mode))))

;;; scratch buffer function to immediately go to the scratch buffer
;;; from anywhere else.
;;; From: http://www.emacswiki.org/emacs/McMahanEmacsMacros
(defun scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
  ;(lisp-interaction-mode)
  (if current-prefix-arg
      (delete-region (point-min) (point-max))
    (goto-char (point-max))))
