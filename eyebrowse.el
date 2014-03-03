;;; eyebrowse.el --- Easy window config switching -*- lexical-binding: t -*-

;; Copyright (C) 2014 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/eyebrowse
;; Version: 0.1
;; Package-Requires: ((dash "2.4.0") (s "1.4.0") (emacs "24"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This global minor mode provides a set of keybindings for switching
;; window configurations.  It tries mimicking the tab behaviour of
;; `ranger`, a file manager.

;; See the README for more info:
;; https://github.com/wasamasa/eyebrowse-mode

;;; Code:

(require 'dash)
(require 's)

;; --- variables -------------------------------------------------------------

(defgroup eyebrowse nil
  "A window configuration switcher modeled after the ranger file
manager."
  :group 'convenience)

(defcustom eyebrowse-lighter " ¬_¬"
  "Lighter for `eyebrowse-minor-mode'.")

(defface eyebrowse-mode-line-brackets
  '((t (:foreground "grey")))
  "Face for the mode line indicator brackets.")

(defface eyebrowse-mode-line-separator
  '((t (:foreground "grey")))
  "Face for the mode line indicator separator.")

(defface eyebrowse-mode-line-inactive
  '((t (:foreground "white")))
  "Face for the inactive items of the mode line indicator.")

(defface eyebrowse-mode-line-active
  '((t (:foreground "white")))
  "Face for the active items of the mode line indicator.")

(defcustom eyebrowse-mode-line-separator ", "
  "Separator of the mode line indicator.")

(defcustom eyebrowse-mode-line-left-delimiter "["
  "Left delimiter of the mode line indicator.")

(defcustom eyebrowse-mode-line-right-delimiter "]"
  "Right delimiter of the mode line indicator.")

(defcustom eyebrowse-mode-line-style 'smart
  "The mode line indicator style may be one of the following:

'hide: Don't show at all.

'smart: Hide when only one window config.

'always: Always show.")

(defcustom eyebrowse-restore-point-p t
  "Restore point, too?
If t, restore point.")

(defcustom eyebrowse-wrap-around-p nil
  "Wrap around when switching to the next/previous window config?
If t, wrap around.")

(defcustom eyebrowse-switch-back-and-forth-p nil
  "Switch to the last window automatically?
If t, switching to the same window config as
`eyebrowse-current-window-config', switches to
`eyebrowse-last-window-config'.")

(defvar eyebrowse-last-slot 1
  "Internal variable storing the last window config slot.")

(defvar eyebrowse-current-slot 1
  "Internal variable storing the current window config slot.")

(defvar eyebrowse-window-configs nil
  "Internal variable storing all window configs.")

(defvar eyebrowse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-w <") 'eyebrowse-prev-window-config)
    (define-key map (kbd "C-c C-w >") 'eyebrowse-next-window-config)
    (define-key map (kbd "C-c C-w '") 'eyebrowse-last-window-config)
    (define-key map (kbd "C-c C-w \"") 'eyebrowse-close-window-config)
    (-dotimes 10 (lambda (n)
                   (define-key map (kbd (s-concat "C-c C-w "
                                                  (number-to-string n)))
                     (lambda () (interactive)
                       (eyebrowse-switch-to-window-config n)))))
    map)
  "Current key map.  Can be set up with `eyebrowse-setup-keys'.")

;; --- internal functions ----------------------------------------------------

(defun eyebrowse-insert-in-window-config-list (element)
  "Insert ELEMENT in the list of window configs.
This function keeps the sortedness intact."
  (setq eyebrowse-window-configs
        (-sort (lambda (a b) (< (car a) (car b)))
               (cons element eyebrowse-window-configs))))

(defun eyebrowse-update-window-config-element (old-element new-element)
  "Replace OLD-ELEMENT with NEW-ELEMENT in the window config list."
  (setq eyebrowse-window-configs
        (-replace-at (-elem-index old-element eyebrowse-window-configs)
                     new-element eyebrowse-window-configs)))

(defun eyebrowse-save-window-config (slot)
  "Save the current window config to SLOT."
  (let* ((element (list slot (current-window-configuration) (point)))
         (match (assq slot eyebrowse-window-configs)))
    (if match
        (eyebrowse-update-window-config-element match element)
      (eyebrowse-insert-in-window-config-list element))))

(defun eyebrowse-load-window-config (slot)
  "Restore the window config from SLOT."
  (let ((match (assq slot eyebrowse-window-configs)))
    (when match
      (let ((window-config (cadr match))
            (point (nth 2 match)))
        (set-window-configuration window-config)
        (goto-char point)))))

(defun eyebrowse-delete-window-config (slot)
  "Remove the window config at SLOT."
  (setq eyebrowse-window-configs
        (remove (assq slot eyebrowse-window-configs)
                eyebrowse-window-configs)))

(defun eyebrowse-switch-to-window-config (slot)
  "Switch to the window config SLOT.
This will save the current window config to
`eyebrowse-current-slot' first, then switch.  If
`eyebrowse-switch-back-and-forth-p' is t and
`eyebrowse-current-slot' equals SLOT, this will switch to the
last window config."
  (when (or (and eyebrowse-switch-back-and-forth-p
                 (= eyebrowse-current-slot slot))
            (/= eyebrowse-current-slot slot))
    (eyebrowse-save-window-config eyebrowse-current-slot)
    (eyebrowse-load-window-config slot)
    (setq eyebrowse-last-slot eyebrowse-current-slot)
    (setq eyebrowse-current-slot slot)
    (eyebrowse-save-window-config eyebrowse-current-slot)
    (eyebrowse-load-window-config eyebrowse-current-slot)))

(defun eyebrowse-update-mode-line ()
  "Return a string representation of the window configurations."
  (let* ((left-delimiter (propertize eyebrowse-mode-line-left-delimiter
                                     'face 'eyebrowse-mode-line-delimiters))
         (right-delimiter (propertize eyebrowse-mode-line-right-delimiter
                                      'face 'eyebrowse-mode-line-delimiters))
         (separator (propertize eyebrowse-mode-line-separator
                                'face 'eyebrowse-mode-line-separator))
         (current-slot (number-to-string eyebrowse-current-slot))
         (active-item (propertize current-slot
                                  'face 'eyebrowse-mode-line-active))
         (window-config-slots (mapcar (lambda (item)
                                        (number-to-string (car item)))
                                      eyebrowse-window-configs)))
    (if (and (not (eq eyebrowse-mode-line-style 'hide))
             (or (eq eyebrowse-mode-line-style 'always)
                 (and (eq eyebrowse-mode-line-style 'smart)
                      (> (length eyebrowse-window-configs) 1))))
        (s-concat left-delimiter
                  (s-join separator
                          (-replace-at (-elem-index current-slot
                                                    window-config-slots)
                                       active-item window-config-slots))
                  right-delimiter)
      "")))

;; --- public functions ------------------------------------------------------

(defun eyebrowse-next-window-config ()
  "Switch to the next available window config.
If `eyebrowse-wrap-around-p' is t, this will switch from the last
to the first one."
  (interactive)
  (let* ((match (assq eyebrowse-current-slot eyebrowse-window-configs))
         (index (-elem-index match eyebrowse-window-configs))
         (next-index (car (nth (1+ index) eyebrowse-window-configs))))
    (if (< (1+ index) (length eyebrowse-window-configs))
        (eyebrowse-switch-to-window-config next-index)
      (when eyebrowse-wrap-around-p
        (eyebrowse-switch-to-window-config
         (caar eyebrowse-window-configs))))))

(defun eyebrowse-prev-window-config ()
  "Switch to the previous available window config.
If `eyebrowse-wrap-around-p' is t, this will switch from the
first to the last one."
  (interactive)
  (let* ((match (assq eyebrowse-current-slot eyebrowse-window-configs))
         (index (-elem-index match eyebrowse-window-configs))
         (prev-index (car (nth (1- index) eyebrowse-window-configs))))
    (if (> index 0)
        (eyebrowse-switch-to-window-config prev-index)
      (when eyebrowse-wrap-around-p
        (eyebrowse-switch-to-window-config
         (caar (last eyebrowse-window-configs)))))))

(defun eyebrowse-last-window-config ()
  "Switch to the last window config."
  (interactive)
  (eyebrowse-switch-to-window-config eyebrowse-last-slot))

(defun eyebrowse-close-window-config ()
  "Close the current window config.
This removes it from `eyebrowse-window-configs' and switches to
another appropriate window config."
  (interactive)
  (when (> (length eyebrowse-window-configs) 1)
    (if (equal (assq eyebrowse-current-slot eyebrowse-window-configs)
               (car (last eyebrowse-window-configs)))
        (eyebrowse-prev-window-config)
      (eyebrowse-next-window-config))
    (eyebrowse-delete-window-config eyebrowse-last-slot)))

;;;###autoload
(defun eyebrowse-setup-opinionated-keys ()
  "Set up more opinionated key bindings for using eyebrowse.

M-1..M-9, C-< / C->, C-`and C-' are used for switching.  If evil
is detected, it will bind gt, gT, gc and zx, too."
  (let ((map eyebrowse-mode-map))
    (define-key map (kbd "C-<") 'eyebrowse-prev-window-config)
    (define-key map (kbd "C->") 'eyebrowse-next-window-config)
    (define-key map (kbd "C-'") 'eyebrowse-last-window-config)
    (define-key map (kbd "C-\"") 'eyebrowse-close-window-config)
    (when (and (fboundp 'evil-mode) evil-mode)
      (define-key evil-motion-state-map (kbd "gt")
        'eyebrowse-next-window-config)
      (define-key evil-motion-state-map (kbd "gT")
        'eyebrowse-prev-window-config)
      (define-key evil-motion-state-map (kbd "gc")
        'eyebrowse-close-window-config)
      (define-key evil-motion-state-map (kbd "zx")
        'eyebrowse-last-window-config))
    (-dotimes 10 (lambda (n)
                   (define-key map (kbd (s-concat "M-" (number-to-string n)))
                     (lambda () (interactive)
                       (eyebrowse-switch-to-window-config n)))))))
;;;###autoload
(define-minor-mode eyebrowse-mode
  "Toggle `eyebrowse-mode'.
This global minor mode provides a set of keybindings for
switching window configurations.  It tries mimicking the tab
behaviour of `ranger`, a file manager."

  :lighter eyebrowse-lighter
  :keymap eyebrowse-mode-map
  :global t

  (if eyebrowse-mode
      (add-to-list 'mode-line-misc-info
                   '(:eval (eyebrowse-update-mode-line)) t)
    (setq mode-line-misc-info
          (remove '(:eval (eyebrowse-update-mode-line)) mode-line-misc-info))))

(provide 'eyebrowse)

;;; eyebrowse.el ends here
