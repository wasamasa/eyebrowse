;;; eyebrowse.el --- Easy window config switching

;; Copyright (C) 2014 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/eyebrowse
;; Version: 0.3.2
;; Package-Requires: ((dash "2.7.0"))
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
;; https://github.com/wasamasa/eyebrowse

;;; Code:

(require 'dash)

;; --- variables -------------------------------------------------------------

(defgroup eyebrowse nil
  "A window configuration switcher modeled after the ranger file
manager."
  :group 'convenience
  :prefix "eyebrowse-")

(defcustom eyebrowse-keymap-prefix (kbd "C-c C-w")
  "Prefix key for key-bindings."
  :type 'string
  :group 'eyebrowse)

(defcustom eyebrowse-lighter " ¬_¬"
  "Lighter for `eyebrowse-mode'."
  :type 'string
  :group 'eyebrowse)

(defface eyebrowse-mode-line-delimiters
  '((t (nil)))
  "Face for the mode line indicator delimiters."
  :group 'eyebrowse)

(defface eyebrowse-mode-line-separator
  '((t (nil)))
  "Face for the mode line indicator separator."
  :group 'eyebrowse)

(defface eyebrowse-mode-line-inactive
  '((t (nil)))
  "Face for the inactive items of the mode line indicator."
  :group 'eyebrowse)

(defface eyebrowse-mode-line-active
  '((t (:inherit mode-line-emphasis)))
  "Face for the active items of the mode line indicator."
  :group 'eyebrowse)

(defcustom eyebrowse-mode-line-separator ", "
  "Separator of the mode line indicator."
  :type 'string
  :group 'eyebrowse)

(defcustom eyebrowse-mode-line-left-delimiter "["
  "Left delimiter of the mode line indicator."
  :type 'string
  :group 'eyebrowse)

(defcustom eyebrowse-mode-line-right-delimiter "]"
  "Right delimiter of the mode line indicator."
  :type 'string
  :group 'eyebrowse)

(defcustom eyebrowse-mode-line-style 'smart
  "The mode line indicator style may be one of the following:

'hide: Don't show at all.

'smart: Hide when only one window config.

'always: Always show."
  :type '(choice (const :tag "Hide" hide)
                 (const :tag "Smart" smart)
                 (const :tag "Always" always))
  :group 'eyebrowse)

(defcustom eyebrowse-restore-point-p t
  "Restore point, too?
If t, restore point."
  :type 'boolean
  :group 'eyebrowse)

(defcustom eyebrowse-wrap-around-p nil
  "Wrap around when switching to the next/previous window config?
If t, wrap around."
  :type 'boolean
  :group 'eyebrowse)

(defcustom eyebrowse-switch-back-and-forth-p nil
  "Switch to the last window automatically?
If t, switching to the same window config as
`eyebrowse-current-window-config', switches to
`eyebrowse-last-window-config'."
  :type 'boolean
  :group 'eyebrowse)

(defcustom eyebrowse-pre-window-switch-hook nil
  "Hook run before switching to a window config."
  :type 'hook
  :group 'eyebrowse)

(defcustom eyebrowse-post-window-switch-hook nil
  "Hook run after switching to a window config."
  :type 'hook
  :group 'eyebrowse)

(defvar eyebrowse-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "<") 'eyebrowse-prev-window-config)
      (define-key prefix-map (kbd ">") 'eyebrowse-next-window-config)
      (define-key prefix-map (kbd "'") 'eyebrowse-last-window-config)
      (define-key prefix-map (kbd "\"") 'eyebrowse-close-window-config)
      (define-key prefix-map (kbd "0") 'eyebrowse-switch-to-window-config-0)
      (define-key prefix-map (kbd "1") 'eyebrowse-switch-to-window-config-1)
      (define-key prefix-map (kbd "2") 'eyebrowse-switch-to-window-config-2)
      (define-key prefix-map (kbd "3") 'eyebrowse-switch-to-window-config-3)
      (define-key prefix-map (kbd "4") 'eyebrowse-switch-to-window-config-4)
      (define-key prefix-map (kbd "5") 'eyebrowse-switch-to-window-config-5)
      (define-key prefix-map (kbd "6") 'eyebrowse-switch-to-window-config-6)
      (define-key prefix-map (kbd "7") 'eyebrowse-switch-to-window-config-7)
      (define-key prefix-map (kbd "8") 'eyebrowse-switch-to-window-config-8)
      (define-key prefix-map (kbd "9") 'eyebrowse-switch-to-window-config-9)
      (define-key map eyebrowse-keymap-prefix prefix-map))
    map)
  "Initial key map for `eyebrowse-mode'.")

;; --- internal functions ----------------------------------------------------

(defun eyebrowse-get (type &optional frame)
  "Retrieve frame-specific value of TYPE.
If FRAME is nil, use current frame.  TYPE can be any of
'window-configs, 'current-slot, 'last-slot."
  (cond
   ((eq type 'window-configs)
    (frame-parameter frame 'eyebrowse-window-configs))
   ((eq type 'current-slot)
    (frame-parameter frame 'eyebrowse-current-slot))
   ((eq type 'last-slot)
    (frame-parameter frame 'eyebrowse-last-slot))))

(defun eyebrowse-set (type value &optional frame)
  "Set frame-specific value of TYPE to VALUE.
If FRAME is nil, use current frame.  TYPE can be any of
'window-configs, 'current-slot, 'last-slot."
  (cond
   ((eq type 'window-configs)
    (set-frame-parameter frame 'eyebrowse-window-configs value))
   ((eq type 'current-slot)
    (set-frame-parameter frame 'eyebrowse-current-slot value))
   ((eq type 'last-slot)
    (set-frame-parameter frame 'eyebrowse-last-slot value))))
(put 'eyebrowse-set 'lisp-indent-function 1)

(defun eyebrowse-insert-in-window-config-list (element)
  "Insert ELEMENT in the list of window configs.
This function keeps the sortedness intact."
  (let* ((window-configs (eyebrowse-get 'window-configs))
         (index (--find-last-index (< (car it) (car element)) window-configs)))
    (eyebrowse-set 'window-configs
      (-insert-at (if index (1+ index) 0) element window-configs))))

(defun eyebrowse-update-window-config-element (new-element)
  "Replace the old element with NEW-ELEMENT in the window config list.
The old element is identified by the first element of NEW-ELEMENT."
  (eyebrowse-set 'window-configs
    (--replace-where (= (car it) (car new-element))
                     new-element (eyebrowse-get 'window-configs))))

;; window-configs are at the moment a list of a list containing the
;; numerical slot, window configuration and point.  To add "tagging",
;; it would be useful to save a tag as fourth component and display it
;; if present, not only in the mode line, but when renaming and
;; selecting a window configuration interactively, too.  This
;; obviously requires an interactive window switching command.

;; The display of the tag should be configurable via format string and
;; that format string be able to resemble vim tabs, i3 workspaces,
;; tmux sessions, etc.  Therefore it has to contain format codes for
;; slot, tag and buffer name.

(defun eyebrowse-save-window-config (slot)
  "Save the current window config to SLOT."
  (let* ((element (list slot (current-window-configuration) (point))))
    (if (assq slot (eyebrowse-get 'window-configs))
        (eyebrowse-update-window-config-element element)
      (eyebrowse-insert-in-window-config-list element))))

(defun eyebrowse-load-window-config (slot)
  "Restore the window config from SLOT."
  (let ((match (assq slot (eyebrowse-get 'window-configs))))
    (when match
      (let ((window-config (cadr match))
            (point (nth 2 match)))
        (set-window-configuration window-config)
        (goto-char point)))))

(defun eyebrowse-delete-window-config (slot)
  "Remove the window config at SLOT."
  (let ((window-configs (eyebrowse-get 'window-configs)))
    (eyebrowse-set 'window-configs
      (remove (assq slot window-configs) window-configs))))

(defun eyebrowse-update-mode-line ()
  "Return a string representation of the window configurations."
  (let* ((left-delimiter (propertize eyebrowse-mode-line-left-delimiter
                                     'face 'eyebrowse-mode-line-delimiters))
         (right-delimiter (propertize eyebrowse-mode-line-right-delimiter
                                      'face 'eyebrowse-mode-line-delimiters))
         (separator (propertize eyebrowse-mode-line-separator
                                'face 'eyebrowse-mode-line-separator))
         (current-slot (eyebrowse-get 'current-slot))
         (active-item (propertize (number-to-string current-slot)
                                  'face 'eyebrowse-mode-line-active))
         (window-configs (eyebrowse-get 'window-configs))
         (window-config-slots (mapcar (lambda (item)
                                        (number-to-string (car item)))
                                      window-configs)))
    (if (and (not (eq eyebrowse-mode-line-style 'hide))
             (or (eq eyebrowse-mode-line-style 'always)
                 (and (eq eyebrowse-mode-line-style 'smart)
                      (> (length window-configs) 1))))
        (concat
         left-delimiter
         (mapconcat 'identity
                    (-replace (number-to-string current-slot)
                              active-item window-config-slots)
                    separator)
         right-delimiter)
      "")))

(defun eyebrowse-read-slot ()
  (let* ((candidates (--map (number-to-string (car it))
                            (eyebrowse-get 'window-configs)))
         (last-slot (number-to-string (eyebrowse-get 'last-slot)))
         (selection (completing-read "Enter slot: " candidates
                                     nil nil last-slot))
         (slot (string-to-number selection)))
    (unless (and (= slot 0) (not (string= selection "0")))
        slot)))

;; --- public functions ------------------------------------------------------

(defun eyebrowse-init (&optional frame)
  "Initialize Eyebrowse for the current frame."
  (eyebrowse-set 'last-slot 1 frame)
  (eyebrowse-set 'current-slot 1 frame))

(defun eyebrowse-switch-to-window-config (slot)
  "Switch to the window config SLOT.
This will save the current window config to
`eyebrowse-current-slot' first, then switch.  If
`eyebrowse-switch-back-and-forth-p' is t and
`eyebrowse-current-slot' equals SLOT, this will switch to the
last window config."
  (interactive (list (eyebrowse-read-slot)))
  (when slot
    (let ((current-slot (eyebrowse-get 'current-slot))
          (last-slot (eyebrowse-get 'last-slot)))
      (when (and eyebrowse-switch-back-and-forth-p (= current-slot slot))
        (setq slot last-slot))
      (when (/= current-slot slot)
        (run-hooks 'eyebrowse-pre-window-switch-hook)
        (eyebrowse-save-window-config current-slot)
        (eyebrowse-load-window-config slot)
        (eyebrowse-set 'last-slot current-slot)
        (eyebrowse-set 'current-slot slot)
        (eyebrowse-save-window-config slot)
        (eyebrowse-load-window-config slot)
        (run-hooks 'eyebrowse-post-window-switch-hook)))))

(defun eyebrowse-next-window-config (count)
  "Switch to the next available window config.
If `eyebrowse-wrap-around-p' is t, this will switch from the last
to the first one.  When used with a numerical argument, switch to
window config COUNT."
  (interactive "P")
  (let* ((window-configs (eyebrowse-get 'window-configs))
         (match (assq (eyebrowse-get 'current-slot) window-configs))
         (index (-elem-index match window-configs)))
    (if count
        (eyebrowse-switch-to-window-config count)
      (when index
        (if (< (1+ index) (length window-configs))
            (eyebrowse-switch-to-window-config
             (car (nth (1+ index) window-configs)))
          (when eyebrowse-wrap-around-p
            (eyebrowse-switch-to-window-config
             (caar window-configs))))))))

(defun eyebrowse-prev-window-config (count)
  "Switch to the previous available window config.
If `eyebrowse-wrap-around-p' is t, this will switch from the
first to the last one.  When used with a numerical argument,
switch COUNT window configs backwards and always wrap around."
  (interactive "P")
  (let* ((window-configs (eyebrowse-get 'window-configs))
         (match (assq (eyebrowse-get 'current-slot) window-configs))
         (index (-elem-index match window-configs)))
    (if count
        (let ((eyebrowse-wrap-around-p t))
          (eyebrowse-prev-window-config
           (when (> count 1)
             (eyebrowse-prev-window-config (1- count)))))
      (when index
        (if (> index 0)
            (eyebrowse-switch-to-window-config
             (car (nth (1- index) window-configs)))
          (when eyebrowse-wrap-around-p
            (eyebrowse-switch-to-window-config
             (caar (last window-configs)))))))))

(defun eyebrowse-last-window-config ()
  "Switch to the last window config."
  (interactive)
  (eyebrowse-switch-to-window-config (eyebrowse-get 'last-slot)))

(defun eyebrowse-close-window-config ()
  "Close the current window config.
This removes it from `eyebrowse-window-configs' and switches to
another appropriate window config."
  (interactive)
  (let ((window-configs (eyebrowse-get 'window-configs)))
    (when (> (length window-configs) 1)
      (if (equal (assq (eyebrowse-get 'current-slot) window-configs)
                 (car (last window-configs)))
          (eyebrowse-prev-window-config nil)
        (eyebrowse-next-window-config nil))
      (eyebrowse-delete-window-config (eyebrowse-get 'last-slot)))))

(defun eyebrowse-switch-to-window-config-0 ()
  "Switch to window configuration 0."
  (interactive)
  (eyebrowse-switch-to-window-config 0))

(defun eyebrowse-switch-to-window-config-1 ()
  "Switch to window configuration 1."
  (interactive)
  (eyebrowse-switch-to-window-config 1))

(defun eyebrowse-switch-to-window-config-2 ()
  "Switch to window configuration 2."
  (interactive)
  (eyebrowse-switch-to-window-config 2))

(defun eyebrowse-switch-to-window-config-3 ()
  "Switch to window configuration 3."
  (interactive)
  (eyebrowse-switch-to-window-config 3))

(defun eyebrowse-switch-to-window-config-4 ()
  "Switch to window configuration 4."
  (interactive)
  (eyebrowse-switch-to-window-config 4))

(defun eyebrowse-switch-to-window-config-5 ()
  "Switch to window configuration 5."
  (interactive)
  (eyebrowse-switch-to-window-config 5))

(defun eyebrowse-switch-to-window-config-6 ()
  "Switch to window configuration 6."
  (interactive)
  (eyebrowse-switch-to-window-config 6))

(defun eyebrowse-switch-to-window-config-7 ()
  "Switch to window configuration 7."
  (interactive)
  (eyebrowse-switch-to-window-config 7))

(defun eyebrowse-switch-to-window-config-8 ()
  "Switch to window configuration 8."
  (interactive)
  (eyebrowse-switch-to-window-config 8))

(defun eyebrowse-switch-to-window-config-9 ()
  "Switch to window configuration 9."
  (interactive)
  (eyebrowse-switch-to-window-config 9))

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
    (when (and (boundp 'evil-mode) evil-mode)
      (define-key evil-motion-state-map (kbd "gt")
        'eyebrowse-next-window-config)
      (define-key evil-motion-state-map (kbd "gT")
        'eyebrowse-prev-window-config)
      (define-key evil-motion-state-map (kbd "gc")
        'eyebrowse-close-window-config)
      (define-key evil-motion-state-map (kbd "zx")
        'eyebrowse-last-window-config))
    (define-key map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
    (define-key map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (define-key map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (define-key map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (define-key map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
    (define-key map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
    (define-key map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
    (define-key map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
    (define-key map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
    (define-key map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)))

;;;###autoload
(define-minor-mode eyebrowse-mode
  "Toggle `eyebrowse-mode'.
This global minor mode provides a set of keybindings for
switching window configurations.  It tries mimicking the tab
behaviour of `ranger`, a file manager."
  :lighter eyebrowse-lighter
  :keymap eyebrowse-mode-map
  :global t
  ;; the `define-minor-mode' macro apparently sets the mode variable
  ;; first, then runs the associated code, therefore if
  ;; `eyebrowse-mode' is t, code related to initialization is run
  (if eyebrowse-mode
      (progn
        ;; for some reason it's necessary to init both after emacs
        ;; started and after frame creation to make it work for both
        ;; emacs and emacsclient
        (eyebrowse-init)
        (add-hook 'after-make-frame-functions 'eyebrowse-init)
        (add-to-list 'mode-line-misc-info
                     '(:eval (eyebrowse-update-mode-line)) t))
    (remove-hook 'after-make-frame-functions 'eyebrowse-init)
    (setq mode-line-misc-info
          (remove '(:eval (eyebrowse-update-mode-line)) mode-line-misc-info))))

(provide 'eyebrowse)

;;; eyebrowse.el ends here
