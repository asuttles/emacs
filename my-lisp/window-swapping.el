;;;					-*- MODE: Lisp -*-


;;; Select previous window in this frame.
(defun acs-last-window ()
  (interactive)
  (other-window -1))

;;; Swap two windows in current frame
(defun acs-swap-two-windows-this-frame ()
  "IFF there are two windows in this frame,
swap the buffers displayed in the two windows"
  (interactive)
  (let ((number-windows-this-frame (length (window-list (selected-frame)))))
    (cond 
     ((= number-windows-this-frame 2)
      (let ((buff1 (current-buffer)))
	(other-window 1)
	(switch-to-buffer-other-window (current-buffer))
	(switch-to-buffer-other-window buff1)))
     ((< number-windows-this-frame 2)
      (message "Only 1 window visible in this frame."))
     (t 
      (message "Too many windows visible in this frame.")))))


;;; Rotate Bufers
;;;
;;; Buffers are those displayed in the 
;;; windows of current frame
;;;
;;; (slightly modified)
;;;
;;; by: Chris Menzel
;;; cmenzel@tamu.edu
;;;
;;; posted at:
;;; help-gnu-emacs@gnu.org message board
;;; Tue, 5 Dec 2006 21:03:06
(defun rotate-window-buffers ()
  (interactive)
  (let* ((windows (window-list))
	 (buffers (mapcar 'window-buffer windows))
	 (wpoints (mapcar 'window-point  windows))
	 (w (pop windows)))
    (setq windows (append windows (list w)))
    ;;                            (list w)))
    (mapc (lambda (w)
	    (set-window-buffer w (pop buffers))
	    (set-window-point  w (pop wpoints)))
	  windows)))
