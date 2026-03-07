;;;; ===========================================================================
;;;;			       ~*- MODE: emacs-lisp -*-
;;;;
;;;;		   Andrew Suttles' EMACS initialization file
;;;;
;;;; ===========================================================================

(message "[reading file ~/init.el]")


;;;; --------------------------------------------------------------------------
;;;;				     MELPA
;;;; --------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/")
	     t)

;;;;(add-to-list 'package-archives
;;;;             '("melpa-stable" . "https://stable.melpa.org/packages/")
;;;;	     t)

;;;; (package-refresh-contents)
;;;; (package-initialize)

;;;; --------------------------------------------------------------------------
;;;;				    GENERAL
;;;; --------------------------------------------------------------------------

;;; Load Path

;;; Customize EMACS with aditional personal customizations
(setq my-customizations-directory "~/.emacs.d/my-lisp")

;;; Safely load ELISP extensions
(defun acs-safe-customization-load (filename)
  "Check to see if 'filename' exists before trying to load it."
  (let ((customization-file (concat my-customizations-directory 
				    "/" filename))) 
    (if (file-readable-p customization-file)
	(load-file customization-file)
      (message (concat "Cannot Load: " customization-file)))))
      
;;; Start Emacs server
;;; Started in .xinitrc
;;(server-start)

;;; Allow text files to define local variables
(setq enable-local-variables t)

;;; Stop emacs from beeping
(setq visible-bell t)

;;; Start IELM - Inferior Emacs Lisp Mode (REPL)
;;(ielm)

;;;(add-hook 'after-init-hook 'global-company-mode)

;;; Define where backups are stored
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))


;;;; --------------------------------------------------------------------------
;;;;				FRAME PROPERTIES
;;;; --------------------------------------------------------------------------

;; Force steady block cursor in terminal
(unless (display-graphic-p)
  (setq cursor-type 'box)
    (send-string-to-terminal "\e[2 q"))

;;; Show matching parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq blink-matching-paren t)

;;; Search highlighting
(setq search-highlight t)
(setq query-replace-highlight t)

;;; Use faces to show meaning in text
(font-lock-mode 1)
(setq font-lock-maximum-decoration t) ; Max decoration

;;; Highlight active region in current buffer
(transient-mark-mode 1)

;;; Mouse cursor avoids point
(mouse-avoidance-mode 'exile) ;cat-and-mouse) 

;;; Set mouse color
(set-mouse-color "black")


;;;; --------------------------------------------------------------------------
;;;;				    MODELINE
;;;; --------------------------------------------------------------------------

;;; Display current line number /column in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; Format time/date in mode line
;;  -- ignore `display-time-day-and-date' and
;;     `display-time-24hr-format' when this is set
(setq display-time-format "   %a %b %e %I:%M%p (%j)")
;;; Show time/date in mode line
;;(setq display-time-day-and-date t)	; Show time AND date

(display-time)				; Display the Day, Date, Time, Load


;;;; --------------------------------------------------------------------------
;;;;				     DIRED
;;;; --------------------------------------------------------------------------

(setq ls-lisp-dirs-first t)             ;display dirs first in dired

;;; Limit `dired' to one buffer
;;;
;;; Prevents dired from cluttering up the buffer list with directories.
;;;
;;; by:
;;; Francois Bourgneuf <francois.bourgneuf@groupe-mma.fr>
;;;
;;; posted at:
;;; help-gnu-emacs@gnu.org message board
;;; Wed, 16 Aug 2006
(defun dired-follow-file ()
  "In `dired', visit the file or directory on this line.
If a directory is on the current line, replace the current dired buffer
with one containing the contents of the directory.  Otherwise, invoke
`dired-find-file' on the file."  (interactive)
  (let ((filename (dired-get-file-for-visit)))
    (if (file-directory-p filename)
	(find-alternate-file filename)
      (dired-find-file))))

(defun dired-setup-follow-file ()
  (substitute-key-definition
   'dired-file-file 'dired-follow-file dired-mode-map)
  (substitute-key-definition
   'dired-advertised-find-file 'dired-follow-file dired-mode-map))


;;;; --------------------------------------------------------------------------
;;;;				  PROGRAMMING
;;;; --------------------------------------------------------------------------

;;(require 'auto-complete)
;;(global-auto-complete-mode t)

;; Start/Stop completion
;;(setq ac-auto-start nil)
;;(global-set-key "\M-/" 'ac-start)
;;(define-key ac-complete-mode-map "\M-/" 'ac-stop)

;; Use C-n/C-p to select candidates
;;(define-key ac-complete-mode-map "\C-n" 'ac-next)
;;(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Do What I Mean mode
(setq ac-dwim t)


;;; ---------------
;;;        C
;;; ---------------
;; (require 'compile)

(setq c-default-style "stroustrup"
      c-basic-offset 2)

;;; C Completion
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-c-headers))

;;; Default C Compile Command
(add-hook 'c-mode-hook
	  (lambda ()
	    (unless (or
		     (file-exists-p "Makefile")
		     (file-exists-p "makefile"))
	      (set (make-local-variable 'compile-command)
		   (let ((filename (file-name-nondirectory buffer-file-name)))
		     (format "gcc -std=c99 -Wall -g %s -o %s.exe"
			     filename
			     (file-name-sans-extension filename)))))))


;;; ---------------
;;;   COMMON LISP
;;; ---------------

;; Specify modes for Lisp file extensions
(setq auto-mode-alist
      (append '(
		("\\.emacs$" . emacs-lisp-mode)
		("\\.lisp$" . lisp-mode)
		("\\.cl$" . lisp-mode)
		("\\.scm$" . scheme-mode)
		) auto-mode-alist))

(use-package slime
  :ensure t
  :defer t
  :init
  (setq slime-lisp-implementations '((sbcl ("sbcl"))))
  :config
  (slime-setup '(slime-fancy slime-company)))

(use-package slime-company
  :after (slime company)
  :ensure t
  :defer t
  :config
  (setq slime-company-completion 'fuzzy))

(use-package slime-fancy
  :after slime
  :defer t
  :ensure nil)

(use-package slime-repl-ansi-color
  :ensure t
  :defer t
  :hook (slime-repl-mode . slime-repl-ansi-color-mode))


;;;; --------------------------------------------------------------------------
;;;;			    UTILITY FUNCTIONS/TOOLS
;;;; --------------------------------------------------------------------------

;;; Set decimal precision for calculator
(setq calculator-number-digits 6)


;;;; --------------------------------------------------------------------------
;;;;			       GLOBAL KEYBINDINGS
;;;; --------------------------------------------------------------------------

;;;; FUNCTION KEY Summary

;;;; [F1]	- Help
;;;; [F2]	- Find Files
;;;; [F3]	- Revert Buffer
;;;; [F4]	- [Dis|E]nable Speedbar
;;;; [F5]	- Reserved (Was: Undo)
;;;; [F6]	- Reserved (Was: TODO)
;;;; [F7]	- Spell Check Buffer
;;;; [F8]	- Reserved (Was: Print Buffer)
;;;; [F9]	- Reserved (Was: Scheme/Quack)
;;;; [F10]	- Reserved (Was: Follow Mode)
;;;; [F11]      - Reserved
;;;; [F12]      - Reserved


;;; [F1] (already bound to help)

;;; [F2]
;;; Find Files - See FFAP Section Above
(global-set-key [(f2)] 'find-name-dired)
(global-set-key [(control f2)] (lambda () (interactive) (find-file-at-point)))
(global-set-key [(meta f2)] 'find-grep-dired)

;;; [F3]
;;; Revert Buffer
(global-set-key [(f3)] 'revert-buffer)
(global-set-key [(meta f3)] 'auto-revert-mode)

;;; [F4]
;;; Speedbar
(global-set-key [f4] 'speedbar-get-focus)

;;; [F5]
;;; Undo
(global-set-key [(f5)] 'undo)

;;; [F7]
;;; Spelling
(global-set-key [(control f7)] 'ispell-buffer)

;;; [F8]
;;; Print
;;;(global-set-key [(f8)] 'acs-print-buffer-auto-font-size)
;;;(global-set-key [(control f8)] 'acs-print-buffer-query-font-size)

;;; [F9]
;;; (global-set-key [(f9)]
;;; 		'(lambda ()
;;; 		   (interactive)
;;; 		   (require 'quack)
;;; 		   (run-scheme mzscheme-program)))

;;; [F10]
;;; Follow Mode
(global-set-key [f10] 'follow-delete-other-windows-and-split)


;;;; [HOME] and [END]

;;; (global-set-key [home] 'beginning-of-buffer)
;;; (global-set-key [end] 'end-of-buffer)


;;;; [UP], [DOWN], [PgUp], [PgDwn]

;;; ACS Page up and down
(global-set-key [(meta down)] 'acs-page-down)
(global-set-key [(meta up)] 'acs-page-up)

;;; Scroll 'Other' Window UNLESS only 1 window
(global-set-key [(control next)] (lambda () 
				   (interactive)
				   (acs-scroll-XX-percent +5)))

(global-set-key [(control prior)] (lambda () 
				    (interactive)
				    (acs-scroll-XX-percent -5)))
				     
;;; Move point up/down by 10%
(global-set-key [(control meta next)]  (lambda () 
					 (interactive) 
					 (acs-scroll-XX-percent +5)))
(global-set-key [(control meta prior)] (lambda () 
					 (interactive) 
					 (acs-scroll-XX-percent -5)))

;;; Horizontal scroll left/right
;;(global-set-key [(control right)] 'scroll-left)
;;(global-set-key [(control left)]  'scroll-right)

;;; Horizontal scroll all windows in frame
;;(global-set-key [(control meta right)] 'acs-hscroll-all-right)
;;(global-set-key [(control meta left)] 'acs-hscroll-all-left)


;;;; [DELETE] and [BACKSPACE]

(global-set-key [\d] 'backward-delete-char)

;;; Delete to beginning of line
(global-set-key [(meta backspace)] 
		(lambda ()
		  (interactive)
		  (let ((end-point (point)))
		    (beginning-of-line)
		    (kill-region (point) end-point))))

;;;; C-C

;;; Bind copy line
(global-set-key "w" 'acs-copy-line)

;;; Bind rotate buffers
(global-set-key [(control c) (s)] 'rotate-window-buffers)

;;; Edit/Repeat last lisp command
;;; NOTE: Already bound to c-x ESC ESC
(global-set-key [(control c) (r)] 'repeat-complex-command)

;;; Turn on truncate lines in this buffer
(global-set-key "t" 'toggle-truncate-lines)

;;;; [+] and [-]

;;; Add/Subtract prefix argument to each integer in the region
;;;(global-set-key [(control meta kp-add)] 'acs-increase-integers-rectangle)
;;;(global-set-key [(control meta kp-subtract)] 'acs-decrease-integers-rectangle)

;;; Bury Buffer
(global-set-key [(control right)] 'bs-cycle-next)
(global-set-key [(control left)] 'bs-cycle-previous)

;;; Shrink/Enlarge current window
(global-set-key [(control kp-add)] 'joc-enlarge-by-five)
(global-set-key [(control kp-subtract)] 'joc-shrink-by-five)

;;;; [.] and [,]

;;; Fast keys to switch windows in this frame
(global-set-key "," 'other-window)
(global-set-key "." 'acs-last-window)


;;; BACKSPACE with CAPS Lock
(global-set-key "" 'backward-delete-char-untabify)

;;;; OVERLOAD

;;; Overload the meta-q to fill region or paragraph
(global-set-key [(meta q)] 'rayz-fill-region-or-paragraph)
(global-set-key [(meta Q)] 'unfill-region)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; Local Variables: ***
;;; fill-column:80 ***
;;; comment-column:0 ***
;;; End: ***


