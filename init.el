;;;; ===========================================================================
;;;;			       ~*- MODE: emacs-lisp -*-
;;;;
;;;;		   Andrew Suttles' EMACS initialization file
;;;;
;;;; ===========================================================================

(message "[reading file ~/init.el]")

(if (equal system-type 'windows-nt)
    (setq load-path (cons "c:/emacs/site-lisp" load-path)))

(setq inhibit-startup-buffer-menu t)

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

;;; Company Mode
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(add-hook 'after-init-hook 'global-company-mode)

;;; Define where backups are stored
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))

;;;; --------------------------------------------------------------------------
;;;;			      REMOTE FILE EDITING
;;;; --------------------------------------------------------------------------

(require 'tramp)

(if (equal system-type 'windows-nt)
    (setenv "PATH"
	    (concat "c:/Program Files (x86)/PuTTY;" (getenv "PATH"))))

(setq tramp-default-method "plink")
;;(set-default 'tramp-auto-save-directory "c:/Users/asuttles/AppData/Local/Temp")

;;;; --------------------------------------------------------------------------
;;;;				FRAME PROPERTIES
;;;; --------------------------------------------------------------------------

;;; Turn off toolbar/menubar/scroll bar to maximize real estate
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; Stop cursor from blinking
(blink-cursor-mode 0)

;;; Show matching parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq blink-matching-paren t)

;;; Search highlighting
(setq search-highlight t)
(setq query-replace-highlight t)

;;; Use faces to show meaning in text
;; (if (not (equal (global-font-lock-mode) t))
;;     (global-font-lock-mode t))
(font-lock-mode 1)
(setq font-lock-maximum-decoration t) ; Max decoration

;;; Highlight active region in current buffer
(transient-mark-mode 1)

;;; Mouse cursor avoids point
(mouse-avoidance-mode 'exile) ;cat-and-mouse) 

;;; Set mouse color
(set-mouse-color "black")


;;; Size the Main Frame and Position is ROUGHLY in center of screen
(if (not (null window-system))
    (progn

      ;; Frame Title (%b = buffer name, %f = filename)
      (setq frame-title-format '("Andrew Suttles' Emacs: %b (%m) <" 
			   default-directory ">"))

      (set-frame-width (selected-frame) 100)
      (set-frame-height (selected-frame) 50)
      (set-frame-position (selected-frame) 80 40)))

;; Set dedicated frames
(setq same-window-buffer-names '("*inferior-lisp*"
				 "*scheme*"
				 "*Apropos*"
				 "*Help*"
				 ;;"*Completions*"
				 ))

;; (setq special-display-buffer-names '(;"*shell*"
;; 				     "*Shell Command Output*"
;; 				     "*info*"
;; 				     "*terminal*"))


;;; Custom Configurations

;;; Custom frame properties when NOT in terminal

;;;(acs-safe-customization-load "frame-properties.el")


;;;; --------------------------------------------------------------------------
;;;;			       WINDOW PROPERTIES
;;;; --------------------------------------------------------------------------

;;; Always split windows vertically
(setq split-width-threshold nil)

(acs-safe-customization-load "window-sizing.el")
(acs-safe-customization-load "window-swapping.el")
(acs-safe-customization-load "window-properties.el")

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

;;; (setq display-time-interval 30)	; 30 Second Time Update Interval
;;(setq display-time-24hr-format nil)	; 12 hour format
(display-time)				; Display the Day, Date, Time, Load


;;;; --------------------------------------------------------------------------
;;;;				FILE MANAGEMENT
;;;; --------------------------------------------------------------------------

;;; *** COMPRESSIOIN/ARCHIVES ***
;;; Read and write compressed and archived files
;;;  of the form .gq, .z, .tgz, .tar
(require 'jka-compr)

;;; FFAP
;;; Bind find-file-at-point default keybindings
;;;
;;; from ffap.el...
;;; (ffap-bindings)                      ; do default key bindings
;;;
;;; ffap-bindings makes the following global key bindings:
;;;
;;; C-x C-f       find-file-at-point (abbreviated as ffap)
;;; C-x 4 f       ffap-other-window
;;; C-x 5 f       ffap-other-frame
;;; S-mouse-3     ffap-at-mouse
;;; C-S-mouse-3   ffap-menu
(ffap-bindings)
(setq ffap-require-prefix t) ; require the prefix arg
;;; (setq ffap-url-regexp nil)   ; disable URL features


;;;; --------------------------------------------------------------------------
;;;;				  TEXT EDITING
;;;; --------------------------------------------------------------------------

;;; Set the page delimiter
(setq page-delimiter "^")

;;; Yank text at point instead of at click.
(setq mouse-yank-at-point t)

;;; Allow user to narrow to region in buffer
(put 'narrow-to-region 'disabled nil)

;;; Custom Configurations
(acs-safe-customization-load "text-editing.el")

;;;; --------------------------------------------------------------------------
;;;;				   TEXT TOOLS
;;;; --------------------------------------------------------------------------

;;; Ediff customizations
;;;
;;; (From Bill Clementson's Blog)
(defconst ediff-ignore-similar-regions t)
(defconst ediff-use-last-dir t)
(defconst ediff-diff-options " -b ")

;;; Grep equivalent on Windows - Need to update for MSYS
;;; (setq grep-command "c:/cygwin/bin/grep -n -a -e ")
;;; (setq grep-command "findstr /n /s ")


;;;; --------------------------------------------------------------------------
;;;;			       BUFFER NAVIGATION
;;;; --------------------------------------------------------------------------

;;; Allow the use of the mouse wheel
(mwheel-install)

;;; Preserve screen position when scrolling...
(setq scroll-preserve-screen-position 1)

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
(setq uniquify-after-kill-buffer-p nil)
(setq uniquify-min-dir-content 2)

(acs-safe-customization-load "buffer-management.el")

;;;; --------------------------------------------------------------------------
;;;;				   MINIBUFFER
;;;; --------------------------------------------------------------------------

;;; Completions in minibuffer 
;;; (define-key minibuffer-local-map [tab] 'comint-dynamic-complete)

;;; Autocomplete buffer names after pressing c-x b
;; Obsolete
;;(require 'iswitchb)
;; (iswitchb-mode 1)

;;; Define buffer completions to ignore
;;; (setq iswitchb-buffer-ignore '("" ""))

;;;; --------------------------------------------------------------------------
;;;;				    PRINTING
;;;; --------------------------------------------------------------------------

(if (string= system-type "windows-nt") 
    (acs-safe-customization-load "printing.el"))

;;;; --------------------------------------------------------------------------
;;;;				    ORG MODE
;;;; --------------------------------------------------------------------------

;;; Locate LATEST org-mode elisp files
;;;(setq load-path (cons "~/org/org-7.8.10/lisp" load-path))
;;;(require 'org)

;;; Configure org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
;;;(global-set-key "\C-ca" 'org-agenda)
;;;(global-set-key "\C-cb" 'org-iswitchb)
; (setq-default major-mode 'org-mode)

;; flyspell mode for spell checking everywhere
;;(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; Define Task States
;; Set state with c-c t
;;;(setq org-todo-keywords
;;;      '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!)")
;;;	(sequence "OPEN(o)" "CLOSED(c)")))

;; Task States Color Code
;;;(setq org-todo-keyword-faces 
;;;      (quote (("TODO" :foreground "red" :weight bold)
;;;              ("STARTED" :foreground "yellow" :weight bold)
;;;              ("DONE" :foreground "forest green" :weight normal)
;;;              ("CANCELED" :foreground "gray" :weight normal))))

;; Commonly Used Tag List
;; (setq org-tag-alist '(("PLF_PROJECT" . ?P) ("Home" . ?H)))

;; Log completed time for task
;;;(setq org-log-done 'time)

;; Agenda files
;; Use c-c [ or c-c ] to add/remove agenda files

;;; Turn off babel execution notification
;; (setq org-confirm-babel-evaluate nil)

;;; Org Babel Languages
;;; (setq org-babel-load-languages '((scheme . t) (emacs-lisp . t)))
;;;(org-babel-do-load-languages
;;;      'org-babel-load-languages
;;;      '((emacs-lisp . t)
;;;        (scheme . t)
;;;	(sh . t)))
      

;;; Edit BABEL source in 'current' window
;;;(setq org-src-window-setup 'current-window)

;;; Set `org-babel-scheme-cmd'
;;;(setq org-babel-scheme-cmd "gsi -:d-")

;;; Set up Easy Templates
;;;(eval-after-load 'org
;;;		 '(progn
;;;		   (add-to-list 'org-structure-template-alist
;;;				'("S" "#+begin_src scheme\n?\n#+end_src" ""))))

;;; Customize org-mode for export
;;;(setq org-footnote-auto-label 'plain)	    ;Plain footnotes
;;;(setq org-export-html-postamble nil)	    ;No postamble in HTML
;;;
;;;
;;;;;; Add diary entries
;;;(setq org-agenda-include-diary t)


;;;; --------------------------------------------------------------------------
;;;;				    ORG2BLOG
;;;; --------------------------------------------------------------------------

;; ;;; RPC for calls to WordPress
;; ;;;(require 'xml-rpc)

;; ;;; Require org2blog
;; (setq load-path (cons "c:/emacs/site-lisp/org2blog-0.5/" load-path))
;; (require 'org2blog-autoloads)

;; ;;; Allow source code formatting in html
;; (setq org2blog/wp-use-sourcecode-shortcode 't)
;; (autoload 'htmlize-buffer "~/org/org-7.8.10/contrib/lisp/htmlize.el")

;; ;;; Fix-up html exports before org2blog htmlize
;; (add-hook 'org-export-html-final-hook
;; 	  (lambda ()
;; 	    (while (re-search-forward "<pre.*>" nil t)
;; 	      (replace-match
;; 	       "<pre style=\"font-size:10pt\"><span style=\"font-family:monospace\">"
;; 	       t nil))
;; 	    (while (re-search-forward "</pre>" nil t)
;; 	      (replace-match
;; 	       "</pre></span>" nil t))))

;; ;;; Set-up org2blog
;; (setq org2blog/wp-blog-alist
;;       '(
;; 	("i686os"
;;          :url "https://i686os.wordpress.com/xmlrpc.php"
;; 	 :username "suttlesa"
;; 	 :wp-code t
;; 	 :tags-as-categories nil)
;; 	("abrahamsseed"
;;          :url "https://abrahamsseed.wordpress.com/xmlrpc.php"
;;          :username "suttlesa"
;;          :tags-as-categories nil)))

;; ;; #+STARTUP: fninline fnplain 
;; ;; Set org-footnote-section to the Vocabulary Section

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


;;; Let W32 figure out what to do with files in DIRED

(defun acs-dired-do-operate-on-file ()
  "Operate on the current dired file, dependent upon file type."
  (interactive)
  (w32-shell-execute "open" (convert-standard-filename (dired-get-filename))))

(defun acs-launch-windows-explorer ()
  (w32-shell-execute "explore" default-directory))

;;; Define shell program to execute on
;;; particular file extensions in dired...
;;(setq dired-guess-shell-alist-user
;;      (list
;;       (list "\\.java$" "javac")
;;       (list "\\.class$" "java")
;;       (list "\\.scm$"   "MzScheme")))

(if (string= system-type "windows-nt") 
    (add-hook 'dired-load-hook
	      (function (lambda ()
			  (load "dired-x")
			  ;; (define-key dired-mode-map "&"
			  ;;	'dired-do-shell-command-in-background)
			  (define-key dired-mode-map "!"
			    'acs-dired-do-operate-on-file)
			  (define-key dired-mode-map "#"
			    'acs-launch-windows-explorer)))))

;;;; --------------------------------------------------------------------------
;;;;				  PROGRAMMING
;;;; --------------------------------------------------------------------------

;;(require 'auto-complete)
;;(global-auto-complete-mode t)

;; Start/Stop completion
(setq ac-auto-start nil)
(global-set-key "\M-/" 'ac-start)
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
;;;     PYTHON
;;; ---------------

(if (string= system-type "windows-nt") 
    (setq python-shell-interpreter "/mingw64/bin/python3"
	  python-shell-interpreter-args "-i"))
(setq python-shell-completion-native-enable nil) ; Disable readline
(setq python-indent-offset 4)

;;; Anaconda Mode
(add-hook 'python-mode-hook 'anaconda-mode)

;;; Flycheck
(add-hook 'python-mode-hook 'flycheck-mode)

;;; PEP 8
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;;; Send Buffer to Python Shell
(defun acs-python-send ()
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell)
  )

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c p") 'acs-python-send)
     (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)
     ))

;;; ---------------
;;;    RACKET
;;; ---------------

;;; Add Racket to execution path
;;;(setenv "PATH"
;;;	(concat "c:/Program Files/Racket;" (getenv "PATH")))
;;;
;;;;; Run Racket.exe in REPL buffer
;;;(setq racket-racket-program "c:/Program Files/Racket/Racket.exe")

;;; ---------------
;;;      NASM
;;; ---------------
;;; Add nasm to execution path
;;;(setenv "PATH"
;;;	(concat "c:/acs/bin/nasm;" (getenv "PATH")))

;;; Autoload `nasm-mode' when it is required
;;; (autoload 'nasm-mode "nasm-mode" "nasm editing mode." t)
;;; (add-to-list 'auto-mode-alist '("\\.asm$" . nasm-mode))
;;; (add-to-list 'interpreter-mode-alist '("nasm" . nasm-mode))


;;; ---------------
;;;      GO
;;; ---------------

;;; Format code before saving
;;; Define local keys for godef
;;;(defun my-go-mode-hook ()
;;;  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
;;;  ; Godef jump key binding                                                      
;;;  (local-set-key (kbd "M-.") 'godef-jump)
;;;  (local-set-key (kbd "M-*") 'pop-tag-mark)
;;;  )
;;;
;;;(add-hook 'go-mode-hook 'my-go-mode-hook)

;;; ---------------
;;;   COMMON LISP
;;; ---------------

;; Require common lisp extensions to elisp
(require 'cl)

;; Specify modes for Lisp file extensions
(setq auto-mode-alist
      (append '(
		("\\.emacs$" . emacs-lisp-mode)
		("\\.lisp$" . lisp-mode)
		("\\.cl$" . lisp-mode)
		("\\.scm$" . scheme-mode)
		)auto-mode-alist))

;; Setup slime load-path and autoloads
;;(add-to-list 'load-path "~/.emacs.d/elpa/slime-2.23/")
;;(require 'slime-autoloads)

;; Define CCL as inferior LISP
(if (string= system-type "windows-nt") 
    (setq inferior-lisp-program "c:/ccl/wx86cl64.exe"))

;; Load slime REPL
;;(setq slime-contribs '(slime-repl slime-company))


;; start slime automatically when we open a lisp file
;;(defun prelude-start-slime ()
;;  (unless (slime-connected-p)
;;    (save-excursion (slime))))

;;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;;(add-hook 'slime-mode-hook 'prelude-start-slime)

;;; LISP documentation
(acs-safe-customization-load "cltl2.el")
;;(setq cltl2-root-url "c:/msys64/home/asuttles/doc/lisp/cltl")

;;; Keymap
;;(defun my-slime-mode-hook ()
;;  "define keys for my functions to slime mode"
;;  (interactive)
;;  (define-key slime-mode-map (kbd "C-c C-d l") 'cltl2-lookup))

;;(add-hook 'slime-mode-hook 'my-slime-mode-hook)

;;; ---------------
;;;     SCHEME
;;; ---------------

;;(setq scheme-program-name "csi -:c")
;;(setq geiser-active-implementations '(chicken))
(setq scheme-program-name "guile")
(setq geiser-active-implementations '(guile))
;;(setq load-path (cons "c:/msys64/usr/local/bin/" load-path))

;; Paredit
;; (autoload 'paredit-mode "paredit"
;;   "Minor mode for pseudo-structurally editing Lisp code." t)
;; (add-hook 'geiser-mode-hook (lambda () (paredit-mode +1)))


;;;; --------------------------------------------------------------------------
;;;;				      LUA
;;;; --------------------------------------------------------------------------

;;;(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;;;(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;;;(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
;;;
;;;(defun run-lua ()
;;;  "Run the lua REPL using commint."
;;;  (interactive)
;;;  (lua-start-process))


;;;; --------------------------------------------------------------------------
;;;;				     SHELL
;;;; --------------------------------------------------------------------------

;; Cygwin Shell System and Tools

(if (string= system-type "windows-nt")
    (progn
      (let ((shell-executable "c:/msys64/usr/bin/bash.exe"))
	(if (file-executable-p shell-executable)
	    (progn
	      (setq explicit-shell-file-name shell-executable)
	      (setq shell-file-name "bash")
	      (setq explicit-bash.exe-args '("--login" "-i"))
 	      (setenv "SHELL" shell-file-name)
	      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m nil t))))
      (setq w32-quote-process-args ?\")
      (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)))


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

;;;; --------------------------------------------------------------------------
;;;;				     E-MAIL
;;;;				    SENDMAIL
;;;; --------------------------------------------------------------------------

;;; See GNUs

;; orgstruct++-mode is enabled in Gnus message buffers to aid in creating 
;;   structured email messages.

;;;(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
;;;(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
;;;(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
;;;(add-hook 'message-mode-hook 'orgtbl-mode 'append)
;;;(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
;;;(add-hook 'message-mode-hook '(lambda () (setq fill-column 72)) 'append)
;;;(add-hook 'message-mode-hook '(lambda () (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)) 'append)


;;;; --------------------------------------------------------------------------
;;;;				    SPELLING
;;;; --------------------------------------------------------------------------

;;; Note: This aspell is non-curses downloaded from: http://aspell.net/win32/

;;; Put aspell on the PATH and Execution Path
(if (string= system-type "windows-nt")
    (progn
      (cond ((and 
	      (not (string-match "aspell" (getenv "PATH")))
	      (file-exists-p "C:/aspell/bin")) 
	     (setenv "PATH" (concat (getenv "PATH") ";C:\\aspell\\bin"))))

      (setq exec-path 
	    (append exec-path
		    (cond ((member "C:/aspell/bin" exec-path) nil)
			  (t (list "C:/aspell/bin")))))

      ;; Define aspell as Spelling Program
      (setq ispell-program-name "C:\\aspell\\bin\\aspell.exe")

      ;; Flyspell 'list' Command
      (setq ispell-list-command "list")))

;;;; --------------------------------------------------------------------------
;;;;				   ESV Bible
;;;; --------------------------------------------------------------------------

(if (equal system-type 'windows-nt)
    (progn
      (require 'esv)

      ;; C-c e looks up a passage and displays it in a pop-up window
      (define-key global-map [(control c) ?e] 'esv-passage)

      ;; C-c i inserts an ESV passage in plain-text format at point
      (define-key global-map [(control c) ?i] 'esv-insert-passage)

      ;; "TEST" or "IP"
      (setq esv-key "IP")))

;;;(add-hook 'text-mode-hook 'turn-on-esv-mode)
;;;(add-hook 'org-mode-hook 'turn-on-esv-mode)


;;;; --------------------------------------------------------------------------
;;;;				 IMAGE VIEWING
;;;; --------------------------------------------------------------------------

;;; Enable image viewing
(auto-image-file-mode t)
;;; (setq image-file-name-extensions 
;;;      (append image-file-name-extensions (list "eps" "jpg" "png" )))


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


;;;; OVERLOAD

;;; Overload the meta-q to fill region or paragraph
(global-set-key [(meta q)] 'rayz-fill-region-or-paragraph)
(global-set-key [(meta Q)] 'unfill-region)


;;;; --------------------------------------------------------------------------
;;;;				 CUSTOMIZATIONS
;;;; --------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (paredit geiser slime-company company-jedi py-autopep8 flycheck company-anaconda elpy anaconda-mode go-mode nasm-mode))))

;;; Local Variables: ***
;;; fill-column:80 ***
;;; comment-column:0 ***
;;; End: ***


