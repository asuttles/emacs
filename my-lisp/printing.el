(defun acs-print-file-notepad (filename)
  "Print the given filename using MS Notepad.exe"
  (interactive "f")
  (w32-shell-execute "open" "notepad.exe" (concat "/P " filename)))

(defun acs-print-this-buffer ()
  "Print the file associated with the present buffer"
  (interactive)
  (save-buffer)
  (let ((filename (if filename filename
		    (buffer-file-name (current-buffer)))))
    (acs-print-file-notepad filename)))

(defun acs-print-region (start end)
  "Print the active region in notepad."
  (interactive "r")
  (let ((filename (make-temp-file "~/tmp/print/prt" nil ".txt")))
    (write-region start end filename)
    (acs-print-file-notepad filename)))
;; Figure out how to delete temp files after print job
;;    (delete-file filename)))	     


;; If you need to customize the print job,
;; open the file in notepad and print from there 
(defun acs-open-region-in-notepad (start end)
  "Print the given filename using MS Notepad.exe"
  (interactive "r")
  (let ((filename (make-temp-file "~/tmp/print/prt" nil ".txt")))
    (write-region start end filename)
    (let ((command (concat "notepad.exe " filename)))
      (message command)
      (w32-shell-execute "open" command))))

