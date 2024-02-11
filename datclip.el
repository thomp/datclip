;;;
;;; datclip.el
;;;
(defvar *datclip-buffer-name* "*datclip*")

(defvar *datclip-values* nil
  "An alist holding the values.")

(defun datclip ()
  "If datclip buffer already exists, move to it. Otherwise, generate the buffer and insert current selections"
  (interactive)
  (if (datclip-buffer-exists-p)
      (datclip-clear-buffer)
    (get-buffer-create *datclip-buffer-name*))
  (switch-to-buffer *datclip-buffer-name*)
  (datclip-insert-selections)
  (datclip-mode))

(defun datclip-update-selections ()
  "Just set *datclip-values*. Don't concern user with the datclip buffer (although we do use it)."
  (get-buffer-create *datclip-buffer-name*) ; ensure buffer exists
  (with-current-buffer *datclip-buffer-name*
    (datclip-insert-selections)))

;; ;; call selection-f with each selection with args selection-value selection-symbol
;; (defun datclip-map-selections (selection-f)
;;   (let ((selection-symbols '(PRIMARY CLIPBOARD))
;; 	;; see SELECTION-CONVERTER-ALIST
;; 	(selection-converter 'STRING))
;;     (dolist (selection-symbol selection-symbols)
;;       ;; Use IGNORE-ERRORS since GUI-GET-SELECTION can error out with "Timed out waiting for reply from selection owner"
;;       (let ((sel (ignore-errors (gui-get-selection selection-symbol 'STRING))))
;; 	(if (not sel)
;; 	    ;; write me
;; 	    (progn (error "handle xclip output for setting *datclip-values*")
;; 		   ;; xclip (shell command) may succeed where GUI-GET-SELECTION and/or X-GET-SELECTION fail
;; 		   (call-process "xclip" nil
;; 				 *datclip-buffer-name*	; insert content in dtk-buffer
;; 				 t   ; redisplay buffer as output is inserted
;; 				 ;; arguments: -b KJV k John
;; 				 "-selection" (symbol-name selection-symbol) "-o"))
;; 	  )
;; 	(funcall selection-f sel selection-symbol)
;; 	)))
;;   )

(defun datclip-insert-selections ()
  ;; SECONDARY is infrequently of interest
  (let ((selection-symbols '(PRIMARY CLIPBOARD))
	;; see SELECTION-CONVERTER-ALIST
	(selection-converter 'STRING)
        ;; gitcommitmsg: convenience feature for selecting for subsequent copy/paste work
        (count 0))
    (dolist (selection-symbol selection-symbols)
      (incf count)
      ;; Use IGNORE-ERRORS since GUI-GET-SELECTION can error out with "Timed out waiting for reply from selection owner"
      (let ((sel (ignore-errors (gui-get-selection selection-symbol 'STRING))))
	(progn
          (insert (propertize
                   (concatenate 'string "[" (number-to-string count) "] ")
                   'face '(:foreground "dark grey")
                   )
                  )
	  (insert (propertize (symbol-name selection-symbol) 'face '(:foreground "green")))
	  (insert-char ?\x000A 1)
	  (let ((content-start (point)))
	    (if sel
		(insert sel)
	      ;; xclip (shell command) may "succeed" where GUI-GET-SELECTION and/or X-GET-SELECTION fail
	      (call-process "xclip" nil
			    *datclip-buffer-name*	; insert content in dtk-buffer
			    t   ; redisplay buffer as output is inserted
			    "-selection" (symbol-name selection-symbol) "-o")
	      )
	    (setf (alist-get selection-symbol *datclip-values*)
		  (buffer-substring-no-properties content-start (point))))
	  (insert-char ?\x000A 2))))))

(defun datclip-buffer-exists-p ()
  (get-buffer *datclip-buffer-name*))

(defun datclip-clear-buffer ()
  (interactive)
  (with-current-buffer *datclip-buffer-name*
    (delete-region (progn (beginning-of-buffer) (point))
		   (progn (end-of-buffer) (point)))))

(defun datclip-grab-primary ()
  (interactive)
  (kill-new (alist-get 'PRIMARY *datclip-values*))
  (message "Grabbed primary"))

(defun datclip-update-and-grab-primary ()
  (interactive)
  (datclip-update-selections)
  (datclip-grab-primary))

(defun datclip-refresh-buffer ()
  (interactive)
  (datclip-clear-buffer)
  (switch-to-buffer *datclip-buffer-name*)  ; needed?
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
	(define-key map "p" 'datclip-grab-primary)
	(define-key map "r" 'datclip-refresh-buffer)
	(define-key map "g" 'datclip-refresh-buffer)
	map))

;; FIXME: should be able to toggle on/off
(defun datclip-mode ()
  "Major mode for displaying datclip text
\\{datclip-mode-map}
Turning on datclip mode runs `text-mode-hook', then `datclip-mode-hook'."
  (interactive)
  (progn
    (kill-all-local-variables)
    (use-local-map datclip-mode-map)
    (setq mode-name "datclip")
    (setq major-mode 'datclip-mode)
    (set-syntax-table text-mode-syntax-table)
    (setq local-abbrev-table datclip-mode-abbrev-table)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (run-hooks 'text-mode-hook 'datclip-mode-hook))
  (if (not (executable-find "xclip"))
      (message "%s" "datclip functionality will be limited unless xclip is available")))
