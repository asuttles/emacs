;;;;			       ~*- MODE: Lisp -*-

;;; Window Navigation Notes:
;;;
;;; M-r      Cursor to center of window
;;; C-- M-r  Cursor to bottom of window
;;; C-1 M-r  Cursor to top of window


;;;; SCROLLING/PAGING

;;; Page the buffer down
;;; Current line number - 5 will be the new center line
(defun acs-page-down ()
  (interactive)
  (progn
    (move-to-window-line -5)
    (recenter)))

;;; Page the buffer up
;;; Current line number + 5 will be the new center line
(defun acs-page-up ()
  (interactive)
  (progn
    (move-to-window-line 5)
    (recenter)))


;;; Define a macro for putting the current line at the top
(fset 'line-to-top
   "0")

;;; Move point up/down by PERCENT percent of buffer
(defun acs-scroll-XX-percent (&optional percent)
  "Move point by PERCENT percentage of buffer"
  (push-mark)
  (View-goto-percent
   (+ (or percent 5)
      (round 
       (* 100 
	  (/ (* 1.0 (point)) 
	     (* 1.0 (- (point-max) (point-min)))))))))


;;;; POINT

;;; Move point to larger of 80th col or EOL
;;;
(defun acs-indent-to-column-80 ()
  (interactive)
  (end-of-line)
  (indent-to-column 79))

;;; Move point past next occurence of 'char'
(defun acs-move-past-char (char)
  "Move point past the next occurance of 'char'"
  (interactive "cCharacter: ")
  (search-forward (char-to-string char) nil nil nil))




