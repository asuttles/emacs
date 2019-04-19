;;;;			       ~*- MODE: Lisp -*-
;;;;
;;;;		   Andrew Suttles' EMACS initialization file


;;; Color the frame monochrome for maximum contrast
;;;Re-define the frame colors (ala: yellow monochrome)
(defun acs-recolorize-frame-vt220 ()
  (interactive)
    (progn 
      (set-foreground-color "goldenrod") 
      (set-background-color "black")))

;;; Re-colorize Initial Frame
(acs-recolorize-frame-vt220)

;;; Maximize-frame
;;;
;;; from Bill Clementson's Blog
;;; (sample .emacs)
(defun acs-maximize-frame (&optional frame)
  "Maximize the selected FRAME."
  (interactive)
  (or frame
      (setq frame (selected-frame)))
  (let ((pixels-per-col (/ (float (frame-pixel-width))
			   (frame-width)))
	(pixels-per-row (/ (float
			    (frame-pixel-height)) (frame-height))))
    (set-frame-size frame
		    (if (string= "w32" window-system)
			(+ (truncate (/ (x-display-pixel-width) pixels-per-col)) 2)
		      (truncate (/ (x-display-pixel-width) pixels-per-col)))
		    (if (string= "w32" window-system)
			(- (truncate (/ (x-display-pixel-height) pixels-per-row)) 2)
		      (- (truncate (/ (x-display-pixel-height) pixels-per-row)) 7)))
    (set-frame-position frame 0 0)))
