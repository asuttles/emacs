;;;					-*- MODE: Lisp -*-

;;; Set maximum of height for mini-windows
(setq max-mini-window-height 1.0)

;;; Enlarge the current window
;;; Joseph Casadonte (northbound-train.com)
(defun joc-enlarge-by-five ()
  "enlarges window by 5 lines"
  (interactive)
  (enlarge-window 5))

;;; Shrink the current window
;;; Joseph Casadonte (northbound-train.com)
(defun joc-shrink-by-five ()
  "shrinks window 5 times"
  (interactive)
  (shrink-window 5))
