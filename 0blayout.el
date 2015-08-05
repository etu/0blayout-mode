;;; 0blayout.el --- Layout grouping with ease

;; Copyright (c) 2015 Elis Axelsson

;; Author: Elis "etu" Axelsson
;; URL: https://github.com/etu/0blayout
;; Version: 0.1
;; Keywords: convenience, window-management

;;; Commentary:

;; This global minor mode provides a simple way to switch between layouts and
;; the buffers you left open before you switched (unless you closed it).

;; It doesn't require any setup at all more than:
;; (0blayout-mode)

;; When you start emacs with 0blayout loaded, you will have a default layout
;; named "default", and then you can create new layouts (C-c C-l C-c), switch
;; layouts (C-c C-l C-b), and kill the current layout (C-c C-l C-k).

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

(defvar 0blayout-alist ()
  "List of the currently defined layouts.")
(defvar 0blayout-current "default"
  "Currently active layout")



;; Function to create a new layout
(defun 0blayout-new (layout-name)
  "0blayout creating function, default keybind for this function is C-c C-l C-c"
  (interactive "sEnter name of new layout: ")

  ;; Save the currently active layout
  (0blayout-save)

  ;; Then we just delete all other windows and switch to a *scratch* buffer,
  ;; then it's up to the user to set up their layout.
  (delete-other-windows)
  (switch-to-buffer "*scratch*")

  ;; Save the name of the new current layout
  (setq 0blayout-current layout-name))



;; Function to kill current layout
(defun 0blayout-kill ()
  "0blayout removal function, default keybind for this function is C-c C-l C-k"
  (interactive)

  (message "Killing layout: '%s'" 0blayout-current)

  ;; Remove current layout from known layouts
  (setq 0blayout-alist
        (assq-delete-all (intern 0blayout-current) 0blayout-alist))

  ;; Switch to next layout in the list
  (let ((new-layout (car (car 0blayout-alist))))
    (if (eq new-layout nil)
        ;; If there's no other layout, make a new default layout
        (progn
          (setq 0blayout-current "default")
          (0blayout-new "default"))

      ;; Switch to some other saved layout
      (progn
        (set-window-configuration (cdr (car 0blayout-alist)))
        (setq 0blayout-current (symbol-name new-layout))))))



;; Function to switch layout
(defun 0blayout-switch (layout-name)
  "0blayout switching function, default keybind for this function is C-c C-l C-b"
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
        (setq 0blayout-current layout-name)

        (message "Switch to layout: '%s'" layout-name)))))



;; Function to save layout
(defun 0blayout-save ()
  "This is a helper function to save the current layout."

  ;; Remove all saves of current layout before saving
  (setq 0blayout-alist
        (assq-delete-all
         (intern 0blayout-current) 0blayout-alist))

  ;; Add current layout to list
  (add-to-list '0blayout-alist
               (cons (intern 0blayout-current) (current-window-configuration)))

  (message "Saved the currently active layout: %s" 0blayout-current))



;;;###autoload
(define-minor-mode 0blayout-mode
  "Handle layouts with ease"
  :lighter " 0bL"
  :global t
  :group '0blayout
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-l C-c") '0blayout-new)
            (define-key map (kbd "C-c C-l C-k") '0blayout-kill)
            (define-key map (kbd "C-c C-l C-b") '0blayout-switch)
            map))

(provide '0blayout)

;;; 0blayout.el ends here
