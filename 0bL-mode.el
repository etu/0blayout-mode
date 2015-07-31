;;; 0bL-mode.el --- Layout grouping with ease

(defvar 0bL-layout-alist ()
  "List of the currently defined layouts.")
(defvar 0bL-current-layout nil
  "Currently active layout")



;;;
;;; Function to create a new layout
;;;
(defun 0bL-new-layout (layout-name)
  "0bLayout creating function, default keybind for this function is C-c C-l C-c"
  (interactive "sEnter name of new layout: ")

  (unless (string= 0bL-current-layout 'nil)
    ;; If we visit a layout at the moment, we should save the state the layout
    ;; currently has before proceeding.
    (0bL-save-layout layout-name))

  ;; Then we just delete all other windows and switch to a *scratch* buffer,
  ;; then it's up to the user to set up their layout.
  (delete-other-windows)
  (find-file "*scratch*")

  ;; Save the name of the current layout
  (setq 0bL-current-layout layout-name))



;;;
;;; Function to kill current layout
;;;
(defun 0bL-kill-layout ()
  "0bLayout removal function, default keybind for this function is C-c C-l C-k"
  (interactive)

  (message "My mission is to kill %s with fire" 0bL-current-layout))



;;;
;;; Function to switch layout
;;;
(defun 0bL-switch-layout ()
  "0bLayout switching function, default keybind for this function is C-c C-l C-b"
  (interactive)

  (message "My mission is to switch layout"))



;;;
;;; Function to save layout
;;;
(defun 0bL-save-layout (layout-name)
  "This is a helper function to save the current layout."

  (message "TODO: Save the layout: %s" layout-name))



;;;###autoload
(define-minor-mode 0bL-mode
  "Handle your layouts with ease"
  :lighter " 0bL"
  :global t
  :group '0bL
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-l C-c") '0bL-new-layout)
            (define-key map (kbd "C-c C-l C-k") '0bL-kill-layout)
            (define-key map (kbd "C-c C-l C-b") '0bL-switch-layout)
            map))

(provide '0bL-mode)

;;; 0bL-mode.el ends here
