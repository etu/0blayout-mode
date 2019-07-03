;;; 0blayout.el --- Layout grouping with ease

;; Copyright (c) 2015-2016 Elis Axelsson

;; Author: Elis "etu" Axelsson
;; URL: https://github.com/etu/0blayout
;; Package-Version: 20160918.0
;; Version: 1.0.2
;; Keywords: convenience, window-management

;;; Commentary:

;; This global minor mode provides a simple way to switch between layouts and
;; the buffers you left open before you switched (unless you closed it).

;; It doesn't require any setup at all more than:
;; (0blayout-mode)

;; When you start Emacs with 0blayout loaded, you will have a default layout
;; named "default", and then you can create new layouts (<prefix> C-c), switch
;; layouts (<prefix> C-b), and kill the current layout (<prefix> C-k).
;; The default <prefix> is (C-c C-l), but you can change it using:
;; (0blayout-add-keybindings-with-prefix "<your prefix>")

;; You can also customize-variable to change the name of the default session.

;; The project is hosted at https://github.com/etu/0blayout
;; There you can leave bug-reports and suggestions.

;; Another comparable mode is eyebrowse which have been developed for longer.

;;; License:

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(defgroup 0blayout nil
  "Configuration settings for 0blayout-mode."
  :group 'convenience)

(defvar 0blayout-alist ()
  "List of the currently defined layouts.")

(defcustom 0blayout-default "default"
  "Name of default layout used."
  :type 'string
  :group '0blayout)

(defvar 0blayout-keys-map '(("C-c" . 0blayout-new)
                            ("C-k" . 0blayout-kill)
                            ("C-b" . 0blayout-switch))
  "Which keys bounded to which functions map.")

(defvar 0blayout-mode-map (make-sparse-keymap)
  "Keymap for 0blayout.")


;; Function to create a new layout
(defun 0blayout-new (layout-name)
  "0blayout creating function.
Argument LAYOUT-NAME Name of the layout."
  (interactive "sEnter name of new layout: ")

  ;; Save the currently active layout
  (0blayout-save)

  ;; Then we just delete all other windows and switch to a *scratch* buffer,
  ;; then it's up to the user to set up their layout.
  (delete-other-windows)
  (switch-to-buffer "*scratch*")

  ;; Save the name of the new current layout
  (0blayout-set-current-name layout-name))


;; Function to kill current layout
(defun 0blayout-kill ()
  "0blayout removal function."
  (interactive)

  (message "Killing layout: '%s'" (0blayout-get-current-name))

  ;; Remove current layout from known layouts
  (setq 0blayout-alist
        (assq-delete-all (intern (0blayout-get-current-name)) 0blayout-alist))

  ;; Switch to next layout in the list
  (let ((new-layout (car (car 0blayout-alist))))
    (if (eq new-layout nil)
        ;; If there's no other layout, make a new default layout
        (progn
          (0blayout-set-current-name 0blayout-default)
          (0blayout-new 0blayout-default))

      ;; Switch to some other saved layout
      (progn
        (set-window-configuration (cdr (car 0blayout-alist)))
        (0blayout-set-current-name (symbol-name new-layout))))))


;; Function to switch layout
(defun 0blayout-switch (layout-name)
  "0blayout switching function.
Argument LAYOUT-NAME Name of the layout."
  (interactive
   (list
    (completing-read "Layout to switch to: " 0blayout-alist)))

  ;; Save the currently active layout
  (0blayout-save)

  (let ((layout (assoc (intern layout-name) 0blayout-alist)))
    (if (eq layout nil)
        (message "No layout with name: '%s' is defined" layout-name)
      (progn
        ;; Load window configuration
        (set-window-configuration (cdr layout))

        ;; Save the name of the currently active layout
        (0blayout-set-current-name layout-name)

        (message "Switch to layout: '%s'" layout-name)))))


;; Function to save layout
(defun 0blayout-save ()
  "This is a helper function to save the current layout."

  ;; Remove all saves of current layout before saving
  (setq 0blayout-alist
        (assq-delete-all
         (intern (0blayout-get-current-name)) 0blayout-alist))

  ;; Add current layout to list
  (add-to-list '0blayout-alist
               (cons (intern (0blayout-get-current-name))
                     (current-window-configuration)))

  (message "Saved the currently active layout: %s" (0blayout-get-current-name)))


;; Save current layout name
(defun 0blayout-set-current-name (layout-name)
  "Helper function to store current LAYOUT-NAME for this frame."

  (set-frame-parameter nil '0blayout-current layout-name))


;; Get current layout name
(defun 0blayout-get-current-name ()
  "Helper function to get current LAYOUT-NAME for this frame."

  ;; Get variable from current frame
  (let ((current-layout (frame-parameter nil '0blayout-current)))
    ;; Check if it's nil
    (if (eq current-layout nil)
        ;; If so, return default value
        0blayout-default
      ;; else return current value
      current-layout)))


;;;###autoload
(defun 0blayout-add-keybindings-with-prefix (prefix)
  "Add 0blayout keybindings using the prefix PREFIX."
  (setf (cdr 0blayout-mode-map) nil)
  (dolist (pair 0blayout-keys-map)
    (define-key 0blayout-mode-map
      (kbd (format "%s %s" prefix (car pair)))
      (cdr pair))))

(0blayout-add-keybindings-with-prefix "C-c C-l")


;;;###autoload
(define-minor-mode 0blayout-mode
  "Handle layouts with ease"
  :lighter " 0bL"
  :global t
  :group '0blayout
  :keymap 0blayout-mode-map)


(provide '0blayout)

;;; 0blayout.el ends here
