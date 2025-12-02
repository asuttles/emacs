;;; init.el --- My Emacs Configuration -*- lexical-binding: t; -*-

;;;; ===========================================================================
;;;;			       ~*- MODE: emacs-lisp -*-
;;;;
;;;;		      Andrew Suttles' EMACS initialization file
;;;;
;;;; ===========================================================================

(message "[reading file ~/init.el]")

(if (equal system-type 'windows-nt)
    (setq load-path (cons "~/.emacs.d/site-lisp" load-path)))

;;; Disable lockfiles and notify support
(setq create-lockfiles nil
      file-notify-support nil)

;;;(toggle-debug-on-error 1)

;;;; --------------------------------------------------------------------------
;;;;				    PACKAGES
;;;; --------------------------------------------------------------------------

;;; Package Management
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	;;("nongnu" . "https://elpa.nongnu.org/nongnu/")
	))


(unless package-archive-contents
  (package-refresh-contents))

;;; Install use-package - lazy load packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(use-package benchmark-init
  :ensure t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;;;; --------------------------------------------------------------------------
;;;;				      GPG
;;;; --------------------------------------------------------------------------

(setq package-gnupghome-dir "/c/msys64/home/asuttles/.emacs.d/elpa/gnupg")
(setq epg-gpg-home-directory package-gnupghome-dir)
(setq epg-gpg-program "/usr/bin/gpg")


(setq epg-debug t)  ;; optional, logs EPG I/O in *epg-debug*

;;;; --------------------------------------------------------------------------
;;;;			  MY CUSTOMIZATION
;;;; --------------------------------------------------------------------------

;;; Customize EMACS with aditional personal customizations
(defvar my-customizations-directory "~/.emacs.d/my-lisp")

;;; Safely load ELISP extensions
(defun acs-safe-customization-load (filename)
  "Safely load a customization file from `my-customizations-directory`."
  (let ((customization-file (file-name-concat my-customizations-directory 
					      filename)))
    (if (file-readable-p customization-file)
	(load-file customization-file)
      (message "Cannot Load: %s" customization-file))))

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
  :defer t
  :config
  (when (display-graphic-p)
    ;; Set Cascadia Mono as the primary font for unicode glyphs
    (set-fontset-font t 'unicode (font-spec :family "Cascadia Mono") nil 'prepend)
    ;; Also add all-the-icons font for icon-specific glyphs
    (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)))

;;; Highlight non-ascii chars in buffer
;;; Useful for finding copy/paste errors into ascii buffers
(defun highlight-non-ascii ()
  (interactive)
  (highlight-regexp "[^\x00-\x7F]" 'hi-yellow))

;;;; --------------------------------------------------------------------------
;;;;				     DIRED
;;;; --------------------------------------------------------------------------

;;; Define function to open files with default Windows program
(defun acs-dired-do-operate-on-file ()
  "Open the current file with its default Windows program."
  (interactive)
  (w32-shell-execute "open"
                     (convert-standard-filename (dired-get-filename))))

;;; Config directory editor
(use-package dired
  :ensure nil  ;; built-in, so no install
  :defer t  
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  ;; Kill old dired buffers when opening new ones
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; You can bind your custom function here, e.g.:
  (define-key dired-mode-map (kbd "C-c o") #'acs-dired-do-operate-on-file))

;;; Add extra dired functionality
(use-package dired-x
  :ensure nil
  :defer t
  :after dired
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\.DS_Store$"))
  ;; Uncomment to enable omit mode by default
  ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
  )

;;; Show git status in dired
(use-package diff-hl
  :ensure t
  :defer t
  :hook ((dired-mode . diff-hl-dired-mode)
         (after-init . global-diff-hl-mode)))

;;; Open Subtrees in dired
(use-package dired-subtree
  :defer t
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))

;;;; --------------------------------------------------------------------------
;;;;				     SHELL
;;;; --------------------------------------------------------------------------

(setq explicit-shell-file-name "bash")
(setq shell-file-name explicit-shell-file-name)
(setq explicit-bash.exe-args '("--login" "-i"))

;; If using use-package
(use-package fish-mode
  :ensure t
  :mode ("\\.fish\\'" . fish-mode))

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
  :defer t
  :ensure t)

;;;; --------------------------------------------------------------------------
;;;;				    MODELINE
;;;; --------------------------------------------------------------------------

;;; Display current line number /column in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; Format time/date in mode line
(use-package time
  :ensure nil ; built-in package
  :custom
  (display-time-format "   %a %b %e %I:%M %p")
  (display-time-interval 30)
  :config
  (display-time-mode 1))

;;;; --------------------------------------------------------------------------
;;;;				FILE MANAGEMENT
;;;; --------------------------------------------------------------------------

;;; Bind find-file-at-point default keybindings
(ffap-bindings)

;;; Track recently opened files
(require 'recentf)
(recentf-mode 1)


;;;; --------------------------------------------------------------------------
;;;;			       BUFFER NAVIGATION
;;;; --------------------------------------------------------------------------

;;; Allow the use of the mouse wheel
(if (display-graphic-p)
    ;(mwheel-install))
    (mouse-wheel-mode))

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
(use-package uniquify
  :ensure nil  ;; it's built-in, so no need to install
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-ignore-buffers-re "^\\*"
        uniquify-after-kill-buffer-p nil))

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
(use-package man
  :ensure nil  ; It's built-in
  :defer t  
  :custom
  (Man-notify-method 'pushy))

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

(use-package eww
  :ensure nil  ;; eww is built-in
  :defer t
  :hook ((eww-mode . visual-line-mode)
         (eww-mode . my/eww-setup))
  :config
  (defun my/eww-half-page-scroll-up ()
    "Scroll up by half a page in `eww-mode`."
    (interactive)
    (move-to-window-line nil)
    (recenter-top-bottom 1))

  (defun my/eww-setup ()
    "Custom keybindings and setup for `eww-mode`."
    (define-key eww-mode-map (kbd "SPC") #'my/eww-half-page-scroll-up)))

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
(setq backup-by-copying t) 
(setq delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)


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
(use-package ediff
  :ensure nil  ; Built-in package
  :defer t  
  :custom
  (ediff-diff-options " -b ")
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;;; Split horizontally instead of vertically
;;;(setq ediff-split-window-function 'split-window-horizontally)


;;;; --------------------------------------------------------------------------
;;;;                             VERSION CONTROL
;;;; --------------------------------------------------------------------------

;;; Git Management
(use-package magit
 :defer t
 :after transient)

;;; Use Magit, no other version control
(setq vc-handled-backends nil)

;;;; --------------------------------------------------------------------------
;;;;				    PRINTING
;;;; --------------------------------------------------------------------------

(if (string= system-type "windows-nt") 
    (acs-safe-customization-load "printing.el"))

;;;; --------------------------------------------------------------------------
;;;;				    ORG MODE
;;;; --------------------------------------------------------------------------

;;; Open the org link at point using eww, when requested
(defun acs-org-open-at-point (&optional arg)
  "Open org links using `eww` if prefix arg is provided."
  (interactive "P")
  (if (not arg)
      (org-open-at-point)
    (let ((browse-url-browser-function #'eww-browse-url))
      (org-open-at-point))))

;;; Enable and configure the org-mode package
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-switchb))
  :config
  ;; Startup in overview instead of expanded
  (setq org-startup-folded t)

  ;; Define org-agenda files
  (setq org-agenda-files '("~/org/meeting-notes.org"
			   "~/org/278COS.org"
			   "~/org/fd.org"
			   "~/org/home.org"))

  ;; Hide Tags in Agenda
  (setq org-agenda-hide-tags-regexp ".*")

  ;; Only look forward a month
  (setq org-agenda-span 30)
  
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
        org-startup-with-latex-preview t)

  (define-key org-mode-map (kbd "C-c C-o") #'acs-org-open-at-point))

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

;;; Swap Current Field with the Next Field
(defun acs-org-table-swap-cells ()
  "Swap the content of the current cell with the next one in the same row."
  (interactive)
  (when (org-at-table-p)
    (let ((field1 (org-table-get-field))
          (col1 (org-table-current-column)))
      (org-table-goto-column (1+ col1))
      (let ((field2 (org-table-get-field)))
	(org-table-blank-field)
	(insert field1)
        (org-table-goto-column col1)
        (org-table-blank-field)
	(insert field2)
        (org-table-align)))))

(defvar acs-meeting-notes-file "~/org/meeting-notes.org")

(defun acs-process-rocketbook-scan (scan-text)
  "Process Rocketbook SCAN-TEXT and append it to meeting-notes.org."
  (let ((lines (split-string scan-text "\n" t))
        (meeting "MISC")
        (date nil)
        (notes '())
        (todos '())
        (current-todo nil))

    ;; Step 1: Parse the scan
    (dolist (line lines)
      (cond

       ;; Match meeting title
       ((string-match "^##\\s-*\\(.*?\\)\\s-*##$" line)
        (setq meeting (match-string 1 line)))

       ;; Match date
       ((string-match "^#\\s-*DATE:\\s-*\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" line)
        (setq date (match-string 1 line)))

       ;; Match new TODO
       ((string-match "^@\\s-*TODO:\\s-*\\(.*\\)$" line)
        (setq current-todo `(:text ,(match-string 1 line)))
        (push current-todo todos))

       ;; DEADLINE (add to current-todo)
       ((and current-todo
             (string-match "^@\\s-*DEADLINE:\\s-*\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" line))
        (setq current-todo (plist-put current-todo :deadline (match-string 1 line)))
        (setf (car todos) current-todo)) ;; update most recent todo in list

       ;; SCHEDULED
       ((and current-todo
             (string-match "^@\\s-*SCHEDULED:\\s-*\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" line))
        (setq current-todo (plist-put current-todo :scheduled (match-string 1 line)))
        (setf (car todos) current-todo)) ;; update most recent todo in list

       ;; Other @ lines
       ((string-match "^@\\s-*.*" line)
        nil)

       ;; Notes
       ((string-match "^-\\s-*\\(.*\\)$" line)
        (push (match-string 1 line) notes))))

    ;; Step 2: Default date
    (unless date
      (setq date (format-time-string "%Y-%m-%d")))

    ;; Step 3: Format content
    (let* ((heading (concat "* " meeting))
           (date-entry (concat "** " date))
           (notes-text (when notes
                         (concat "*** NOTES\n"
                                 (mapconcat (lambda (n) (concat "- " n)) (reverse notes) "\n"))))
           (todo-entries
            (mapconcat
             (lambda (todo)
               (let ((line (concat "*** TODO [#B] " (plist-get todo :text))))
                 (when (plist-get todo :deadline)
                   (setq line (concat line "\nDEADLINE: <" (plist-get todo :deadline) ">")))
                 (when (plist-get todo :scheduled)
                   (setq line (concat line "\nSCHEDULED: <" (plist-get todo :scheduled) ">")))
                 line))
             (reverse todos) "\n")))

      ;; Step 4: Write to file
      (with-current-buffer (find-file-noselect acs-meeting-notes-file)
        (goto-char (point-min))
        (unless (re-search-forward (concat "^" (regexp-quote heading)) nil t)
          (goto-char (point-max))
          (insert "\n" heading "\n"))

        (re-search-backward (concat "^" (regexp-quote heading)) nil t)
        (org-end-of-subtree t t)

        (insert "\n" date-entry "\n")
        (when notes-text (insert notes-text "\n"))
        (when todo-entries (insert todo-entries "\n"))
        (save-buffer)))))

;;;; --------------------------------------------------------------------------
;;;;				   DASHBOARD
;;;; --------------------------------------------------------------------------

(setq inhibit-startup-screen t)

(use-package bible-gateway
  :ensure t)

;;; Create a startup dashboard
(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook)  
  (setq dashboard-startup-banner 'logo
	dashboard-startupify-list
	'(dashboard-insert-banner
	  dashboard-insert-newline
	  dashboard-insert-banner-title
	  dashboard-insert-newline
	  dashboard-insert-init-info
	  dashboard-insert-newline
	  dashboard-insert-newline	
	  dashboard-insert-navigator
	  dashboard-insert-items
	  dashboard-insert-newline
	  dashboard-insert-footer)
	dashboard-set-navigator t
	dashboard-navigator-buttons
	'(((nil "Weather" "Open wttrin Buffer" (lambda (&rest _) (wttrin nil)) nil nil nil)
	   (nil "News" "Open Hacker News" (lambda (&rest _) (hackernews)))
	   (nil "Email" "Open gnus Buffer" (lambda (&rest _) (gnus)))
	   (nil "Git" "Open magit" (lambda (&rest _) (magit)))
	   (nil "Lisp REPL" "Open CCL REPL" (lambda (&rest _) (slime)))	   
	   (nil "Shell" "Open emacs shell" (lambda (&rest _) (shell)))	   
	   ))
        dashboard-center-content nil
	dashboard-agenda-sort-strategy '(priority-up todo-state-up)
        dashboard-set-heading-icons nil
	dashboard-item-names '(("Agenda for the coming week:" . "Action Items:"))
        dashboard-set-file-icons t
	dashboard-footer-messages (list (bible-gateway-get-verse))
	dashboard-items '((recents   . 7)
                          (bookmarks . 5)
			  (agenda    . 20))))


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
  :defer t
  :config
  (add-hook 'asm-mode-hook 'nasm-mode))

(add-hook 'nasm-mode-hook
	  (lambda ()
	    (set-fill-column 80)
	    (setq comment-column 40)))

(use-package x86-lookup
  :ensure t
  :defer t
  :config
  (setq  x86-lookup-pdf "~/programming/asm/x86ref/intelVol2.pdf"))

;;; ---------------
;;;    GOLANG
;;; ---------------

(use-package go-mode
  :ensure t
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . my-go-mode-setup)
  :config
  (defun my-go-mode-setup ()
    "Custom setup for `go-mode'."
    (add-hook 'before-save-hook #'gofmt-before-save nil t)
    (setq tab-width 4)
    (local-set-key (kbd "M-.") #'godef-jump)
    (yas-minor-mode 1)
    (eldoc-mode 1)
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode)))

(use-package company-go
  :ensure t
  :defer t
  :after (company go-mode))

;;; enable flycheck
;;;(add-hook 'go-mode-hook 'flycheck-mode)

;;; ---------------
;;;   COMMON LISP
;;; ---------------

;; Specify modes for Lisp file extensions
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))

;; Quicklisp help for slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;;; Enable slime for code editing and repl
(use-package slime
  :ensure t
  :defer t
  :init
  ;; Common Lisp implementations
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"))
          (ccl ("~/install/ccl/wx86cl64.exe" "--load" "~/.ccl/ccl-init.lisp"))))
  :config
  (slime-setup '(slime-fancy slime-company))
  (setq slime-net-coding-system 'utf-8-unix)
  ;; Windows: show REPL in other window
  (add-to-list 'display-buffer-alist
               '("\\*slime-repl\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.33)))
  ;; Attach hook *after* slime is loaded, so slime-connected-p exists
  (add-hook 'lisp-mode-hook
            (lambda ()
              (unless (slime-connected-p)
                (slime)))))

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

;;; Configure hyperspec lookups
;;;(require 'hyperspec)
;;;
;;;(defun hyperspec-lookup--hyperspec-lookup-w3m (orig-fun &rest args)
;;; (let ((browse-url-browser-function 'eww-browse-url))
;;;   (apply orig-fun args)))
;;;
;;;(advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-w3m)
;;;
;;;(with-eval-after-load 'slime
;;;  (define-key slime-mode-map (kbd "C-c C-d h") #'hyperspec-lookup))

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


;;;; ---------------------------------------------------------------------------
;;;;				     WASM
;;;; ---------------------------------------------------------------------------

;;; wat
(add-to-list
 'treesit-language-source-alist
 '(wat "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wat/src"))

;;; wast
(add-to-list
 'treesit-language-source-alist
 '(wast "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wast/src"))

;;;; ---------------------------------------------------------------------------
;;;;				     EMAIL
;;;; ---------------------------------------------------------------------------

;;; Turn email body into org-mode notes
(defun acs-extract-rocketbook-body ()
  "Extract Rocketbook OCR body text from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\.pdf$" nil t)
      (let ((start (match-beginning 0)))
        (if (re-search-forward "^[-]+\\s-*\nText transcription is made possible" nil t)
            (buffer-substring-no-properties start (match-beginning 0))
          (buffer-substring-no-properties start (point-max)))))))

(defun acs-edit-ocr-text-before-processing (raw-text)
  "Let user edit RAW-TEXT in a temp buffer. Returns the edited string.
Press C-c C-c to accept, or C-c C-k to cancel."
  (let ((bufname "*OCR Review*")
        (result nil))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (insert raw-text)
      (goto-char (point-min))
      (text-mode)
      (setq-local header-line-format
                  "Fix OCR text. C-c C-c to continue, C-c C-k to cancel.")
      ;; Set up finishing commands
      (use-local-map
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map (current-local-map))
         (define-key map (kbd "C-c C-c")
           (lambda ()
             (interactive)
             (setq result (buffer-string))
             (throw 'done t)))
         (define-key map (kbd "C-c C-k")
           (lambda ()
             (interactive)
             (setq result nil)
             (throw 'done t)))
         map)))
    (pop-to-buffer bufname)
    ;; Block until user presses C-c C-c or C-c C-k
    (catch 'done
      (recursive-edit))
    (kill-buffer bufname)
    result))

(defun acs-process-gnus-rocketbook-message ()
  "Process Rocketbook OCR text from current Gnus article buffer."
  (interactive)
  (let ((ocr-text (acs-extract-rocketbook-body)))
    (if ocr-text
	(let ((fixed-text (acs-edit-ocr-text-before-processing ocr-text)))
	  (when fixed-text
	    (acs-process-rocketbook-scan fixed-text)))
      (message "Rocketbook OCR text not found in buffer: %s" (buffer-name)))))


;;; Load Gnus (built-in in Emacs)
(use-package gnus
  :commands (gnus)
  :init
  (setq user-full-name "Andrew Suttles"
        user-mail-address "acs@disroot.org")
  :config
  (setq gnus-select-method
        '(nnimap "disroot"
                 (nnimap-address "disroot.org")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl)))
  (setq gnus-permanently-visible-groups "^nnimap\\+disroot:")

  (setq gnus-secondary-select-methods
	'((nntp "news.gwene.org")
	  ;(rss "https://planet.lisp.org/atom.xml")
	  (nntp "news.gmane.io")))
  
  ;; SMTP config
  (setq smtpmail-smtp-server "smtp.disroot.org"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        message-send-mail-function 'smtpmail-send-it
        gnus-imap4-use-auth-source t
        smtpmail-auth-credentials "~/.authinfo.gpg")
  ;; UI preferences
  (setq gnus-inhibit-startup-message t
        gnus-read-active-file nil
        gnus-thread-sort-functions '(gnus-thread-sort-by-date))
  ;; Keybinding in gnus-article-mode
  (define-key gnus-article-mode-map (kbd "C-c m") #'acs-process-gnus-rocketbook-message))

;;;; ---------------------------------------------------------------------------
;;;;				    WEATHER
;;;; ---------------------------------------------------------------------------

(defun acs-wttrin-setup ()
  "Custom settings for wttrin."
  (setq truncate-lines t)
  (when (string= (buffer-name) wttrin-buffer-name)
    (setq buffer-face-mode-face nil)
    (buffer-face-mode -1)))

(use-package wttrin
  :ensure t
  :hook
  (wttrin-mode . acs-wttrin-setup)
  :config
  (setq wttrin-default-locations '("Avon Ohio")
	wttrin-unit-system "u"
	wttrin-font-name "Cascadia Mono-11"))

;;;; --------------------------------------------------------------------------
;;;;			    UTILITY FUNCTIONS/TOOLS
;;;; --------------------------------------------------------------------------

;;; Set decimal precision for calculator
(use-package calculator
  :ensure nil
  :defer t
  :custom
  (setq calculator-number-digits 6))

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

;;; Last Window
(global-set-key "o" 'acs-last-window)

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
   '(ac-geiser benchmark-init bible-gateway company-go counsel diff-hl dired-git
	       dired-subtree diredfl eat ebnf-mode find-file-in-project
	       fish-mode flycheck geiser-chicken geiser-guile gnuplot go-eldoc
	       go-snippets hackernews lsp-scheme lsp-ui magit marginalia
	       nasm-mode oberon org-bullets paredit scheme-complete sicp
	       slime-company slime-repl-ansi-color sml-mode vertico vterm-toggle
	       wat-ts-mode web-mode wttrin x86-lookup yasnippet-snippets)))

;;; TODOs
;;; Update keybindings
;;; Delete unused packages??
;;; setup elfeed for reddit or hackernews or foxnews, etc feeds

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
