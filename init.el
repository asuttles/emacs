;;;; ===========================================================================
;;;;			       ~*- MODE: emacs-lisp -*-
;;;;
;;;;		   Andrew Suttles' EMACS initialization file
;;;;
;;;; ===========================================================================

(message "[reading file ~/init.el]")

(if (equal system-type 'windows-nt)
    (setq load-path (cons "~/.emacs.d/site-lisp" load-path)))


;;;; --------------------------------------------------------------------------
;;;;				      GPG
;;;; --------------------------------------------------------------------------

(if (equal system-type 'windows-nt)
    (setq package-gnupghome-dir (expand-file-name "~/.gnupg")))

;;;; --------------------------------------------------------------------------
;;;;				    PACKAGES
;;;; --------------------------------------------------------------------------

;;; Package Management
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

;;; Install use-package - lazy load packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;; --------------------------------------------------------------------------
;;;;			  MY CUSTOMIZATION
;;;; --------------------------------------------------------------------------

;;; Customize EMACS with aditional personal customizations
(setq my-customizations-directory "~/.emacs.d/my-lisp")

;;; Safely load ELISP extensions
(defun acs-safe-customization-load (filename)
  "Safely load a customization file from `my-customizations-directory`."
  (let ((customization-file (file-name-concat my-customizations-directory 
					      filename)))
    (if (file-readable-p customization-file)
	(load-file customization-file)
      (message "Cannot Load: " customization-file))))

;;;; --------------------------------------------------------------------------
;;;;			   SERVER and REPL
;;;; --------------------------------------------------------------------------

;;; Start Emacs server 
;;(server-start)

;;; Start IELM - Inferior Emacs Lisp Mode (REPL)
;;(ielm)
;;; Turn on Company Mode in IELM Buffers
;;;(add-hook 'ielm-mode-hook 'company-mode)

;;;; --------------------------------------------------------------------------
;;;;                                UNICODE
;;;; --------------------------------------------------------------------------

;;; Default Font
(set-face-attribute 'default nil :font "Cascadia Mono-11")

;;; Configure Unicode Icon Support
(use-package all-the-icons
  :ensure t
  :config
  (when (display-graphic-p)
    ;; Set Cascadia Mono as the primary font for unicode glyphs
    (set-fontset-font t 'unicode (font-spec :family "Cascadia Mono") nil 'prepend)
    ;; Also add all-the-icons font for icon-specific glyphs
    (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)))

;;;; --------------------------------------------------------------------------
;;;;				     DIRED
;;;; --------------------------------------------------------------------------

;;; Config directory editor
(use-package dired
  :ensure nil  ;; built-in, so no install
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  ;; Kill old dired buffers when opening new ones
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; Define function to open files with default Windows program
  (defun acs-dired-do-operate-on-file ()
    "Open the current file with its default Windows program."
    (interactive)
    (w32-shell-execute "open"
                       (convert-standard-filename (dired-get-filename))))

  ;; You can bind your custom function here, e.g.:
  (define-key dired-mode-map (kbd "C-c o") #'acs-dired-do-operate-on-file))

;;; Add extra dired functionality
(use-package dired-x
  :ensure nil
  :after dired
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\.DS_Store$"))
  ;; Uncomment to enable omit mode by default
  ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
  )

;;; Show git status in dired
(use-package diff-hl
  :ensure t
  :hook ((dired-mode . diff-hl-dired-mode)
         (after-init . global-diff-hl-mode)))

;;;; --------------------------------------------------------------------------
;;;;				FRAME PROPERTIES
;;;; --------------------------------------------------------------------------

;;; Turn off toolbar/menubar/scroll bar to maximize real estate
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; Frame Transparency
(set-frame-parameter (selected-frame) 'alpha '(85 75))
(add-to-list 'default-frame-alist '(alpha . (85 . 75)))

;;; Enable scroll-bars in Console only
(if (display-graphic-p) 
    (scroll-bar-mode -1))

;;; Stop cursor from blinking
(blink-cursor-mode 0)

;;; Mouse cursor avoids point
(mouse-avoidance-mode 'exile) ;cat-and-mouse) 

;;; Set mouse color
(set-mouse-color "black")

;;; Frame Title
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

;;; Size the Main Frame and Position is ROUGHLY in center of screen
(set-frame-width (selected-frame) 100)

;;; DELETE THIS
;;; (or modernize using:
;;;(add-to-list 'display-buffer-alist
;;;             '("^\\*Help\\*" . (display-buffer-same-window)))
;;; Set dedicated frames
;;;(setq same-window-buffer-names '("*inferior-lisp*"
;;;				 "*scheme*"
;;;				 "*Apropos*"
;;;				 "*Help*"
;;;				 ;;"*Completions*"
;;;				 ))

;;; Custom Configurations
(acs-safe-customization-load "frame-properties.el")

;;;; --------------------------------------------------------------------------
;;;;			       WINDOW PROPERTIES
;;;; --------------------------------------------------------------------------

;;; Always split windows vertically
(setq split-width-threshold nil)

(acs-safe-customization-load "window-sizing.el")
(acs-safe-customization-load "window-swapping.el")
(acs-safe-customization-load "window-properties.el")

;;;; --------------------------------------------------------------------------
;;;;                        CUSTOMIZED COMMAND MENUS
;;;; --------------------------------------------------------------------------

;;; UI toolkit for interactive command menus
(use-package transient ; Needed for magit
  :ensure t)

;;;; --------------------------------------------------------------------------
;;;;				    MODELINE
;;;; --------------------------------------------------------------------------

;;; Display current line number /column in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; Format time/date in mode line
(setq display-time-format "   %a %b %e %I:%M %p")
(setq display-time-interval 30)
(display-time-mode 1)

;;;; --------------------------------------------------------------------------
;;;;				FILE MANAGEMENT
;;;; --------------------------------------------------------------------------

;;; Bind find-file-at-point default keybindings
(ffap-bindings)

;;;; --------------------------------------------------------------------------
;;;;			       BUFFER NAVIGATION
;;;; --------------------------------------------------------------------------

;;; Allow the use of the mouse wheel
(if (display-graphic-p)
    (mwheel-install))

;;; Preserve screen position when scrolling...
(setq scroll-preserve-screen-position 1)

;;; Precise Scrolling
(setq pixel-scroll-precision-mode t)

;;; Stop at the end of the file, do not add lines
(setq next-line-add-newlines nil)

;;; Custom Configurations
(acs-safe-customization-load "buffer-navigation.el")

;;;; --------------------------------------------------------------------------
;;;;			       BUFFER MANAGEMENT
;;;; --------------------------------------------------------------------------

;;; Uniquify buffers (avoid name clashes)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-seperator "/")
(setq uniquify-ignore-buffers-re "^\\*")    ;; Don’t rename special buffers
(setq uniquify-after-kill-buffer-p nil)

(acs-safe-customization-load "buffer-management.el")

;;;; --------------------------------------------------------------------------
;;;;				   MINIBUFFER
;;;; --------------------------------------------------------------------------

;;; Use vertico for minibuffer selection
(use-package vertico
  :init
  (vertico-mode 1))

;;; Adds helpful annotations to minibuffer candidates
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode 1))

;;; Match parts of words in any order
;;;(use-package orderless
;;;  :init
;;;  (setq completion-styles '(orderless)
;;;        completion-category-defaults nil))

;;; To Add Later:
;;; consult: drop-in replacements for switch-to-buffer, find-file, etc.
;;; embark: context-sensitive actions (e.g., open file, run command, etc.)

;;;; --------------------------------------------------------------------------
;;;;				  MAN and INFO
;;;; --------------------------------------------------------------------------

;; Set the INFO path
;; Cannot change env var in MS Windows
;;(setq Info-default-directory-list 
;;      (append (list "c:/acs/info") Info-default-directory-list))
(if (string= system-type "windows-nt") 
    (setq Info-default-directory-list (list "c:/acs/info")))

;;; Man pages open in "this" frame, "this" window
(setq Man-notify-method 'pushy)

(defun my-info-mode-hook ()
  (local-set-key "j" 'next-line)
  (local-set-key "k" 'previous-line)
  (local-set-key "l" 'recenter-top-bottom)
  (local-set-key ";" 'Info-history-back))

(add-hook 'Info-mode-hook 'my-info-mode-hook)

;;;; --------------------------------------------------------------------------
;;;;				 IMAGE VIEWING
;;;; --------------------------------------------------------------------------

;;; Enable image viewing
(auto-image-file-mode t)
;;; (setq image-file-name-extensions 
;;;      (append image-file-name-extensions (list "eps" "jpg" "png" )))

;;; ----------------------------------------------------------------------------
;;;				      EWW
;;; ----------------------------------------------------------------------------

;;; Scroll up by half-pages in EWW-mode
(defun acs-half-page-scroll-up ()
  (interactive)
  (progn
    (move-to-window-line nil)
    (recenter-top-bottom 1)))

;;; Define a function to load when eww-mode is invoked
(add-hook 'eww-mode-hook
          (lambda ()
	    (define-key eww-mode-map [? ] #'acs-half-page-scroll-up)
            (visual-line-mode)))

;;;; --------------------------------------------------------------------------
;;;;				 AUTOCOMPLETION
;;;; --------------------------------------------------------------------------

;;; Use global company-mode
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  :hook
  ((slime-mode . company-mode)
   (slime-repl-mode . company-mode)))


;;;; --------------------------------------------------------------------------
;;;;			      EDITING
;;;; --------------------------------------------------------------------------

;;; Visual Editing and Behavior
(setq visible-bell t)           ;; Flash instead of beep
(setq enable-local-variables t) ;; Allow file-local variables
(setq tab-width 4)              ;; Set default tab width

;;; Define where backups are stored
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))

;;; Track file locations
(save-place-mode 1)

;;; Show matching parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq blink-matching-paren t)

;;; Search highlighting
(setq search-highlight t)
(setq query-replace-highlight t)

;;; Use Maximum decoration to show meaning in text
(setq font-lock-maximum-decoration t)

;;; Highlight active region in current buffer
(transient-mark-mode 1)

;;; Set the page delimiter
(setq page-delimiter "^")

;;; Yank text at point instead of at click.
(setq mouse-yank-at-point t)

;;; Allow user to narrow to region in buffer
(put 'narrow-to-region 'disabled nil)

;;; Custom Configurations
(acs-safe-customization-load "text-editing.el")

;;; Ignore whitespace when diffing
(setq ediff-diff-options " -b ")

;;; Ediff control panel in same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Split horizontally instead of vertically
;;;(setq ediff-split-window-function 'split-window-horizontally)


;;;; --------------------------------------------------------------------------
;;;;                             VERSION CONTROL
;;;; --------------------------------------------------------------------------

;;; Git Management
(use-package magit
 :ensure t
 :after transient)

;;;; --------------------------------------------------------------------------
;;;;				    PRINTING
;;;; --------------------------------------------------------------------------

(if (string= system-type "windows-nt") 
    (acs-safe-customization-load "printing.el"))

;;;; --------------------------------------------------------------------------
;;;;				    ORG MODE
;;;; --------------------------------------------------------------------------

;;; Enable and configure the org-mode package
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-switchb))
  :hook ((org-mode . acs-enable-org-keybindings))
  :config
  ;; Startup in overview instead of expanded
  (setq org-startup-folded t)

  ;; Define task states
  (setq org-todo-keywords
        '((sequence "TODO" "WORK" "|" "DONE")))

  ;; Task states color coding
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("WORK" :foreground "yellow" :weight bold)
          ("DONE" :foreground "forest green" :weight normal)
          ("CANCELED" :foreground "gray" :weight normal)))

  ;; LaTeX preview settings
  (setq org-format-latex-options
        (plist-put (copy-sequence org-format-latex-options) :scale 2.5)
        org-startup-with-latex-preview t))

;;; Customized org support functions...
(defun acs-enable-org-keybindings ()
  "Add custom keybindings for org-mode."
  (define-key org-mode-map (kbd "C-c C-o") #'acs-org-open-at-point))

(defun acs-org-open-at-point (&optional arg)
  "Open org links using `eww` if prefix arg is provided."
  (interactive "P")
  (if (not arg)
      (org-open-at-point)
    (let ((browse-url-browser-function #'eww-browse-url))
      (org-open-at-point))))

(defun acs-sort-priorities ()
  "Sort org Tasks by status, then priority"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (re-search-backward "^\\* Tasks" nil t)
    (org-sort-entries t ?p)
    (org-sort-entries t ?o)))

;;; org-bullets for pretty headlines
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;;;; --------------------------------------------------------------------------
;;;;				  PROGRAMMING
;;;; --------------------------------------------------------------------------

;;;; ----------------
;;;;   TREE-SITTER
;;;; ----------------

;;; Use treesit-install-language-grammar to install grammar
(setq treesit-language-source-alist
   '((css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")))

;;; ---------------
;;;        C
;;; ---------------
;; (require 'compile)

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
;;;      NASM
;;; ---------------

(use-package nasm-mode
  :ensure t
  :config
  (add-hook 'asm-mode-hook 'nasm-mode))

(add-hook 'nasm-mode-hook
	  (lambda ()
	    (set-fill-column 80)
	    (setq comment-column 40)))

(use-package x86-lookup
  :ensure t
  :config
  (setq  x86-lookup-pdf "~/programming/asm/x86ref/intelVol2.pdf"))

;;; ---------------
;;;    GOLANG
;;; ---------------

;;; Setup Go backend for Company
(require 'company-go)

;;; Format code before saving
(defun my-go-mode-hook ()
  "Stuff to do before loading go-mode"
  (message "Go mode hook")
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq tab-width 4)				  ; reasonable tab width
  ;; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump)
  ;; Yasnippet Minor Mode
  (yas-minor-mode 1)
  ;; eldoc minor mode
  (eldoc-mode 1)
  ;; Use Go company backend
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;;; enable flycheck
;;;(add-hook 'go-mode-hook 'flycheck-mode)

;;; ---------------
;;;   COMMON LISP
;;; ---------------

;; Specify modes for Lisp file extensions
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

;;; Enable slime for code editing and repl
(use-package slime
  :ensure t
  :init
  ;; CCL on MS Windows
  (setq inferior-lisp-program
	(if (eq system-type 'windows-nt)
            "~/install/ccl/wx86cl64.exe --load ~/.ccl/ccl-init.lisp"
          "sbcl"))
  :hook
  (lisp-mode . (lambda () (unless (slime-connected-p) (slime))))
  :config
  (slime-setup '(slime-fancy slime-company))
  (setq slime-net-coding-system 'utf-8-unix)
  ;; Windows: show REPL in other window
  (add-to-list 'display-buffer-alist
               '("\\*slime-repl\\*"
		 (display-buffer-reuse-window display-buffer-at-bottom)
		 (window-height . 0.33))))

(use-package slime-company
  :after (slime company)
  :ensure t
  :config
  (setq slime-company-completion 'fuzzy))

(use-package slime-fancy
  :after slime
  :ensure nil)

(use-package slime-repl-ansi-color
  :ensure t
  :hook (slime-repl-mode . slime-repl-ansi-color-mode))

;;; Configure hyperspec lookups
(require 'hyperspec)

(defun hyperspec-lookup--hyperspec-lookup-w3m (orig-fun &rest args)
 (let ((browse-url-browser-function 'eww-browse-url))
   (apply orig-fun args)))

(advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-w3m)

(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "C-c C-d h") #'hyperspec-lookup))

;;; Note:
;;; Possible packages to add: rainbow-delimiters, paredit, smartparens


;;;; NOTE:
;;;; Delete all of the following old configuration...

;; Define CCL as inferior LISP
;;(if (string= system-type "windows-nt") 
;;    (setq inferior-lisp-program "~/install/ccl/wx86cl64.exe --load ~/.ccl/ccl-init.lisp"))

;;;(load "C:/Users/asuttles/quicklisp/slime-helper.el")

;;(slime-setup '(slime-fancy slime-company))

;;(defun my-start-slime-if-needed ()
;;  "Start SLIME if it's not already running."
;;  (unless (slime-connected-p)
;;    (save-excursion
;;      (slime))))
;;
;;(add-hook 'lisp-mode-hook #'my-start-slime-if-needed)

;;; LISP documentation
;;(acs-safe-customization-load "cltl2.el")
;;(setq cltl2-root-url "c:/msys64/home/asuttles/doc/lisp/cltl")

;;; Look up CLHS in Info.
;;;(require 'info-look)

;;; Keymap
;;(defun my-slime-mode-hook ()
;; "define keys for my functions to slime mode"
;; (interactive)
;; (define-key slime-mode-map (kbd "C-c C-d l") 'cltl2-lookup)
;; (define-key slime-mode-map (kbd "C-c h") 'info-lookup-symbol))
;;
;;
;;(add-hook 'slime-mode-hook 'my-slime-mode-hook)

;;; Open slime in 'other' window
;;;(setq display-buffer-alist
;;;      '(("\\*slime-repl\\*"
;;;         (display-buffer-in-other-window)
;;;         (reusable-frames . t))))

;;;; --------------------------------------------------------------------------
;;;;			    UTILITY FUNCTIONS/TOOLS
;;;; --------------------------------------------------------------------------

;;; Set decimal precision for calculator
(setq calculator-number-digits 6)

;;; Custom Configurations
(acs-safe-customization-load "utility-functions.el")

;;;; --------------------------------------------------------------------------
;;;;			       DOCUMENT TEMPLATES
;;;; --------------------------------------------------------------------------

;;; Document templates

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

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)


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

;;; Backspace a char (Note: F1 is help key)
(global-set-key [(control h)] 'delete-backward-char)

;;; Delete region w/o adding to kill ring
(global-set-key [(control delete)] 'acs-delete-region-or-backward-kill-word)

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


;;;; OVERLOAD

;;; Overload the meta-q to fill region or paragraph
(global-set-key [(meta q)] 'rayz-fill-region-or-paragraph)
(global-set-key [(meta Q)] 'unfill-region)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ac-geiser company-anaconda company-go company-jedi counsel diff-hl dired-git
	       diredfl dirvish elpy find-file-in-project flycheck geiser-chicken
	       geiser-guile go-eldoc go-snippets gptel lsp-scheme lsp-ui magit
	       marginalia nasm-mode org-bullets paredit py-autopep8
	       scheme-complete sicp slime-company slime-repl-ansi-color sml-mode
	       vertico votd vterm-toggle web-mode x86-lookup yasnippet-snippets)))

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
