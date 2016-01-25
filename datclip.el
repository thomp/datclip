;;;
;;; datclip.el
;;;
(defvar *datclip-buffer-name* "*datclip*")

(defun datclip ()
  "If datclip buffer already exists, move to it. Otherwise, generate the buffer and insert current selections"
  (interactive)
  (if (datclip-buffer-exists-p)
      (datclip-clear-buffer)
    (get-buffer-create *datclip-buffer-name*))
  (datclip-insert-selections)
  (datclip-mode))

(defun datclip-insert-selections ()
  (switch-to-buffer *datclip-buffer-name*)
  (let ((selection-symbols '(PRIMARY SECONDARY CLIPBOARD))
	;; see SELECTION-CONVERTER-ALIST
	(selection-converter 'STRING))
    (dolist (selection-symbol selection-symbols)
      ;; Use IGNORE-ERRORS since GUI-GET-SELECTION can error out with "Timed out waiting for reply from selection owner"      
      (let ((sel (ignore-errors (gui-get-selection selection-symbol 'STRING))))
	(progn
	  (insert (propertize (symbol-name selection-symbol) 'face '(:foreground "green")))
	  (insert-char ?\x000A 1)
	  (if sel
	      (insert sel) 
	    ;; xclip (shell command) may succeed where GUI-GET-SELECTION and/or X-GET-SELECTION fail 
	    (call-process "xclip" nil
			  *datclip-buffer-name*	; insert content in dtk-buffer
			  t   ; redisplay buffer as output is inserted
			  ;; arguments: -b KJV k John
			  "-selection" (symbol-name selection-symbol) "-o")
	    ;;(insert (propertize " ** no selection ** " 'face '(:foreground "gray")))
	    )
	  (insert-char ?\x000A 2))))))

(defun datclip-buffer-exists-p ()
  (get-buffer *datclip-buffer-name*))

(defun datclip-clear-buffer ()
  (interactive)
  (with-current-buffer *datclip-buffer-name*
    (delete-region (progn (beginning-of-buffer) (point))
		   (progn (end-of-buffer) (point)))))

(defun datclip-refresh-buffer ()
  (interactive)
  (datclip-clear-buffer)
  (datclip-insert-selections))

(defun datclip-quit ()
  (interactive)
  (kill-buffer *datclip-buffer-name*))

;; bind 'Super-c to DATCLIP
(global-set-key (kbd "s-c") 'datclip)

;;;
;;; datclip-mode
;;;
(defvar datclip-mode-abbrev-table nil
  "Abbrev table used while in datclip mode.")

;; place where users can add stuff
(defvar datclip-mode-hook nil)

(defvar datclip-mode-map nil
  "Major mode keymap for `datclip-mode'.")
(setq datclip-mode-map 
      (let ((map (make-sparse-keymap))) 
	(define-key map "c" 'datclip-clear-buffer)
	(define-key map "q" 'datclip-quit)
	(define-key map "r" 'datclip-refresh-buffer)
	map))

(defun datclip-mode ()
  "Major mode for displaying datclip text
\\{datclip-mode-map}
Turning on datclip mode runs `text-mode-hook', then `datclip-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map datclip-mode-map)
  (setq mode-name "datclip")
  (setq major-mode 'datclip-mode)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table datclip-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (run-hooks 'text-mode-hook 'datclip-mode-hook))
