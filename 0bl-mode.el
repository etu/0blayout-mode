;;; 0bl-mode.el --- Layout grouping with ease

(defvar 0bl-layout-alist ()
  "List of the currently defined layouts.")
(defvar 0bl-current-layout "default"
  "Currently active layout")



;;;
;;; Function to create a new layout
;;;
(defun 0bl-new-layout (layout-name)
  "0blayout creating function, default keybind for this function is C-c C-l C-c"
  (interactive "sEnter name of new layout: ")

  ;; Save the currently active layout
  (0bl-save-layout)

  ;; Then we just delete all other windows and switch to a *scratch* buffer,
  ;; then it's up to the user to set up their layout.
  (delete-other-windows)
  (find-file "*scratch*")

  ;; Save the name of the new current layout
  (setq 0bl-current-layout layout-name))



;;;
;;; Function to kill current layout
;;;
(defun 0bl-kill-layout ()
  "0blayout removal function, default keybind for this function is C-c C-l C-k"
  (interactive)

  (message "Killing layout: '%s'" 0bl-current-layout)

  ;; Remove current layout from known layouts
  (setq 0bl-layout-alist
        (assq-delete-all (intern 0bl-current-layout) 0bl-layout-alist))

  ;; Switch to next layout in the list
  (let ((new-layout (car (car 0bl-layout-alist))))
    (set-window-configuration (cdr (car 0bl-layout-alist)))
    (setq 0bl-current-layout (symbol-name new-layout))))



;;;
;;; Function to switch layout
;;;
(defun 0bl-switch-layout (layout-name)
  "0blayout switching function, default keybind for this function is C-c C-l C-b"
  (interactive "sEnter name of layout to switch to: ")

  ;; Save the currently active layout
  (0bl-save-layout)

  (let ((layout (assoc (intern layout-name) 0bl-layout-alist)))
    (if (eq layout nil)
        (message "No layout with name: '%s' is defined" layout-name)
      (progn
        ;; Load window configuration
        (set-window-configuration (cdr layout))

        ;; Save the name of the currently active layout
        (setq 0bl-current-layout layout-name)

        (message "Switch to layout: '%s'" layout-name)))))



;;;
;;; Function to save layout
;;;
(defun 0bl-save-layout ()
  "This is a helper function to save the current layout."

  ;; Remove all saves of current layout before saving
  (setq 0bl-layout-alist
        (assq-delete-all
         (intern 0bl-current-layout) 0bl-layout-alist))

  ;; Add current layout to list
  (add-to-list '0bl-layout-alist
               (cons (intern 0bl-current-layout) (current-window-configuration)))

  (message "Saved the currently active layout: %s" 0bl-current-layout))



;;;###autoload
(define-minor-mode 0bl-mode
  "Handle layouts with ease"
  :lighter " 0bl"
  :global t
  :group '0bl
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-l C-c") '0bl-new-layout)
            (define-key map (kbd "C-c C-l C-k") '0bl-kill-layout)
            (define-key map (kbd "C-c C-l C-b") '0bl-switch-layout)
            map))

(provide '0bl-mode)

;;; 0bl-mode.el ends here
