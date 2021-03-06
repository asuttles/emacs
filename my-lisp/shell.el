;;;					-*- MODE: Lisp -*-
;;;
;;;				       Shell, Dired, Terminal


;;; Use cygwin `bash' as the default shell in Emacs.
;;;(setq exec-path (cons "C:/cygwin/bin" exec-path))
(setq shell-file-name "C:/cygwin/bin/bash.exe")
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name) ; Interactive shell
(setq explicit-shell-args '("--login" "-i"))

(setq binary-process-input t) 
(setq w32-quote-process-args ?\") 

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)


;; ;;; Bookmark System

;; ;;; Define a list to contain a buffer-local set of saved directories
;; (defvar acs-directory-list nil
;;   "A buffer local list of directory bookmarks to be used in shell mode.")
;; (make-variable-buffer-local 'acs-directory-list)

;; ;;; Add a directory to the list of directory bookmarks
;; (defun acs-add-bookmark ()
;;   "Add the pwd to the bookmarked list of directories"
;;   (interactive)
;;   (let ((dirstring (substring (pwd) 10 (length (pwd)))))
;;     (if (not (member dirstring acs-directory-list))
;; 	(setq acs-directory-list 
;; 	  (append (list dirstring) acs-directory-list)))))


;; ;;; Scroll through a saved buffer-local list of directories
;; (defun acs-scroll-shell-directories ()
;;   (interactive)
;;   (if (equal mode-name "Shell")
;;       (let ((newdir (pop acs-directory-list))
;; 	    (this-dir (pwd)))
;; 	;; Rotate the directory list
;; 	(setq acs-directory-list (append acs-directory-list (list newdir)))
;; 	(if newdir 
;; 	    ;; Select next 'unique' directory
;; 	    (if (string= (substring this-dir 10 (length this-dir)) newdir)
;; 		(if (> (length acs-directory-list) 1)
;; 		    (acs-scroll-shell-directories)
;; 		  (message "No unique directories exist."))
;; 	      (progn
;; 		(comint-bol)
;; 		(if (not (eq (point) (point-max)))
;; 		    (kill-line))
;; 		(insert (concat "cd " newdir))))
;; 	  (message "The directory list is empty.")))))


;; ;;; Searching

;; ;;; Make a buffer local container for the "last" search string
;; (defvar acs-last-comint-search-string "")
;; (make-variable-buffer-local 'acs-last-comint-search-string)

;; ;;; acs-comint-search-history
;; ;;;
;; ;;; Search up/down through search history using arrow keys
;; ;;;
;; ;;; When the cursor is at the beginning of the shell prompt, 
;; ;;; scroll through the command history using up/down arrow 
;; ;;; keys.
;; ;;;
;; ;;; By typing a few characters and then using the arrow keys, 
;; ;;; the comint history is searched to find a command beginning
;; ;;; with the typed characters. For successive up/down presses,
;; ;;; continue to search up/down through command history.
;; ;;;
;; ;;; NOTE: 
;; ;;; a-r can be used for searches within the comint buffer iteself
;; ;;; c-c enter copies the command at point to the shell prompt
;; ;;; c-c r repeats complex command so that multiple searches
;; ;;;       of the buffer can be performed
;; (defun acs-comint-search-history (&optional updn)
;;   "Search up/down through comint history and replace the current command-line
;; with matching strings."
;;   (let* ((updn (if updn updn 1))
;; 	 (last-command last-command)
;; 	 (pnt (point))
;; 	 (end (or (end-of-line) (point)))
;; 	 (start (comint-bol))
;; 	 (histStr (buffer-substring start pnt)))
;; 	;;
;; 	;; Clear the command line...
;; 	(if (> end start)
;; 	    (comint-delchar-or-maybe-eof (- end start)))
;;       ;;
;;       ;; IF repeating a search command...
;;       (if (or (eq last-command 'acs-comint-search-up)
;; 	      (eq last-command 'acs-comint-search-down))
;; 	  (if (> (length acs-last-comint-search-string) 0)	    
;; 	      (comint-previous-matching-input acs-last-comint-search-string updn)
;; 	    (comint-previous-input updn))
;; 	;;
;; 	;; IF NOT repeating a search command...
;; 	(if (> pnt start)
;; 	    (and
;; 	     (setq acs-last-comint-search-string (concat "^" histStr))
;; 	     (comint-previous-matching-input acs-last-comint-search-string updn))
;; 	  (or
;; 	   (setq acs-last-comint-search-string nil)
;; 	   (comint-previous-input updn))))))


;; ;;; Wrappers for searching up/down...
;; ;;; 
;; ;;; Bind these functions to up/down arrows
;; ;;; which are local to a shell mode.
;; (defun acs-comint-search-up ()
;;   (interactive)
;;   (if (comint-after-pmark-p)
;;       (acs-comint-search-history 1)
;;     (previous-line 1)))

;; (defun acs-comint-search-down ()
;;   (interactive)
;;   (if (comint-after-pmark-p)
;;       (acs-comint-search-history -1)
;;     (next-line 1)))


;; ;;; CONFIGURATION


;; ;;; Add color to shell output
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

;; ;;; Maximum number of lines in the shell
;; (setq comint-buffer-maximum-size 600)
;; (setq term-input-ring-size 100)

;; ;;; ANSI terminal colors
;; (setq term-default-fg-color 'term-yellow)
;; (setq term-default-bg-color 'term-blackbg)


;; ;;; Shell process sentinal to fix up history write
;; (defun acs-write-shell-history-on-exit (process event)
;;   "Called when shell process is stopped"
;;   (let* ((comint-input-ring-separator "\n")
;; 	 (buf (process-buffer process)))
;;     (comint-write-input-ring)
;;     (when (buffer-live-p buf)
;;       (with-current-buffer buf
;; 	(insert (format "\nProcess %s is %s and ACS wrote history file!\n" process event))))))


;; ;;; Set up/down shell history keys, history, color output, etc.
;; (add-hook 'shell-mode-hook 
;; 	  '( lambda () 
;; 	     ;; Navigation keys
;; 	     (local-set-key [(up)] 'acs-comint-search-up)
;; 	     (local-set-key [(down)] 'acs-comint-search-down)
;; 	     ;; Re-track the directory
;; 	     (local-set-key [(meta return)] 'dirs)
;; 	     ;; Directory navigation keys
;; 	     (local-set-key [(control c) (b)] 'acs-add-bookmark)
;; 	     (local-set-key [(control return)] 'acs-scroll-shell-directories)
;; 	     ;; Scroll to bottom before a yank
;; 	     (setq comint-scroll-to-bottom-on-input 'this)
;; 	     (setq comint-scroll-to-bottom-on-output 'this)
;; 	     ;; History
;; 	     (setq comint-input-ring-file-name "/leda/home/acs/.sh_history")
;; 	     (setq comint-input-ring-size 500)
;; 	     ;; Separator between shell commands
;; 	     (setq comint-input-ring-separator "\n[\000-\002]*") ;  , , 
;; 	     ;; History items to ignore
;; 	     (setq comint-input-history-ignore "\(^#\)\|\([\001-\002]\)") ; ignore ,, comments
;; 	     ;; Load input history
;; 	     (comint-read-input-ring)
;; 	     ;; Turn on color mode
;; 	     (ansi-color-for-comint-mode-on)
;; 	     ;; Truncate long lines
;; 	     (setq truncate-lines t)
;; 	     ;; Try to write history upon exit from this process
;; 	     (set-process-sentinel (get-buffer-process (current-buffer))
;; 				   'acs-write-shell-history-on-exit)))

;; ;;; Launch it
;; ;;;(shell)


;; ;;; TERMINAL

;; ;;; Require terminal-emulator functionality
;; ;;; This also loads terminal color schemes, etc...
;; (require 'term)


;; ;;; Set up the terminal mode
;; (add-hook 'term-mode-hook
;; 	  (function
;; 	   (lambda ()
;; 	     (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
;; 	     (make-local-variable 'mouse-yank-at-point)
;; 	     (make-local-variable 'transient-mark-mode)
;; 	     (setq mouse-yank-at-point t)
;; 	     (setq transient-mark-mode nil)
;; 	     (auto-fill-mode -1)
;; 	     (setq tab-width 8 ))))

;; ;;;* DIRED
;; ;;; ************************************************************************************************
;; ;;;					       DIRED
;; ;;; ************************************************************************************************

;;; Note: Use c-0 w in dired to copy path+filename to kill ring


;;; Use some artificial intel to determine where to copy files using dired
;;; i.e. default destination determined from 'next' window dired session
(setq dired-dwim-target t)


;; ;;; Highlight lines in dired
;; ;;; find hiline.el in google.com
;; ;;; (add-hook 'dired-mode-hook 'highline-on)


;;; Limit `dired' to one buffer
;;;
;;; Prevents dired from cluttering up the buffer list with directories.
;;;
;;; by:
;;; Bourgneuf Francois <francois.bourgneuf@groupe-mma.fr>
;;;
;;; posted at:
;;; help-gnu-emacs@gnu.org message board
;;; Wed, 16 Aug 2006
(defun dired-follow-file ()
  "In `dired', visit the file or directory on this line.
If a directory is on the current line, replace the current dired buffer
with one containing the contents of the directory.  Otherwise, invoke
`dired-find-file' on the file."  (interactive)
  ;;  (let ((filename (dired-get-filename)))  ;; Stopped working in Win32!
  (let ((filename (dired-get-file-for-visit)))
    (if (file-directory-p filename)
	(find-alternate-file filename)
      (dired-find-file))))

(defun dired-setup-follow-file ()
  (substitute-key-definition
   'dired-file-file 'dired-follow-file dired-mode-map)
  (substitute-key-definition
   'dired-advertised-find-file 'dired-follow-file dired-mode-map))

(add-hook 'dired-mode-hook 'dired-setup-follow-file)

(add-hook 'dired-mode-hook
	  '(lambda()
	     (define-key dired-mode-map [delete] 'dired-do-delete)
	     (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
	     (define-key dired-mode-map [C-down-mouse-1] 'mouse-buffer-menu)
	     (define-key dired-mode-map [double-down-mouse-1] 'dired-mouse-find-file)))

(setq ls-lisp-dirs-first t)             ;display dirs first in dired


;;; Shell Interaction

;;; --------------------------------------------------------------------------------------------
;;; Perform shell command in background using `dired'
;;;
;;; posted at:
;;; help-gnu-emacs@gnu.org message board
;;; Wed, 16 May 2006 13:34:38
;;; Josef.Bauer.NOSPAM@web.de
;;;
;;; <slightly revised>
;;; 
;;; dired-x must be loaded
(defun dired-do-shell-command-in-background (command)
  "In dired, do shell command in background on the file or directory named on this line."
  (interactive
   (list
    (dired-read-shell-command
     (concat "Command to execute on " "%s: ") nil
     (list (dired-get-filename)))))
  (call-process command nil 0 nil (dired-get-filename)))



;;; Automatically run a java executable, if selected in `dired'
(defun acs-dired-do-execute-java-class ()
  "If the current file selected in `dired' is a java class, execute it using the 'java' command"
  (interactive)
  (let ((filename (file-name-sans-extension 
		   (file-name-nondirectory (dired-get-filename))))
	(ext (file-name-extension (dired-get-filename))))
    (if (string= ext "class")
	(shell-command (concat "java " filename))
      nil)))


(defun acs-dired-do-operate-on-file ()
  "Operate on the current dired file, dependent upon file type."
  (interactive)
  (unless (acs-dired-do-execute-java-class)
    ;; (dired-guess-shell-command "Execute: " (dired-get-marked-files))))) 
    (w32-shell-execute "open" (convert-standard-filename (dired-get-filename)))))


(defun acs-launch-windows-explorer ()
  (w32-shell-execute "explore" default-directory))

   
;; ;;; Define shell program to execute on 
;; ;;; particular file extensions in dired...
;; ;; (setq dired-guess-shell-alist-user
;; ;;       (list 
;; ;;        (list "\\.ps$" "gv")
;; ;;        (list "\\.eps$" "gv" )
;; ;;        (list "\\.dat$" "nast2001 old=no" )))


;;; Define shell program to execute on 
;;; particular file extensions in dired...
(setq dired-guess-shell-alist-user
      (list 
       (list "\\.java$" "javac")
       (list "\\.class$" "java")
       (list "\\.scm$"   "MzScheme")))


;;; Load dired-x upon start up, then define keys to run
;;;
;;; `dired-do-shell-command-in-background'
;;; and
;;; `w32-shell-execute'
;;; and
;;; `acs-dired-do-execute-java-class' in dired
;;;
(add-hook 'dired-load-hook
	 (function (lambda ()
		     (load "dired-x")
		     (define-key dired-mode-map "&" 
		       'dired-do-shell-command-in-background)
		     (define-key dired-mode-map "!" 
		       'acs-dired-do-operate-on-file)
		     (define-key dired-mode-map "#"
		       'acs-launch-windows-explorer))))
				   

;;; Kill *all* those pesky dired buffers
(defun acs-kill-dired-buffers ()
  "Kill all dired buffers"
  (interactive)

  ; Check to see if dired buffers exist
  (if (and
       (boundp 'dired-buffers)
       (not (eq dired-buffers nil)))

      ; Remove these existing buffers
      (let ((acs-dired-alist dired-buffers) 
	    elt 
	    buf)
	(mapcar '(lambda (alist)
		   (setq elt (car alist)
			 buf (cdr alist)
			 dired-buffers (delq elt dired-buffers))
		   (kill-buffer buf))
		acs-dired-alist)))
  (message "There are no dired buffers to kill"))



;;; Local Variables: ***
;;; fill-column:100 ***
;;; comment-column:0 ***
;;; End: ***
