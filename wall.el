;;; wall.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/wall.el
;; Version: 0.0.1 Alpha
;; Keywords: wallpaper background image
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; This library provides functionalities and options related to the
;; management of wallpapers, i.e, options, commands and timers
;; to set the proper background image.
;;
;;; Code:

(require 'files)
(require 'cl-extra)

(defgroup wall nil
  "Wallpaper management."
  :group 'extensions
  :group 'convenience)

(defcustom wall-root-dir
  (expand-file-name "~/media/images/wallpapers/")
  "Root directory, the starting point for search functions."
  :type 'directory
  :group 'wall
  :safe t)

(defcustom wall-program "feh"
  "An Image viewer program."
  :type 'string
  :group 'wall
  :safe t)

(defcustom wall-program-args "--bg-fill"
  "Arguments that will be used by `wall-program'."
  :type 'string
  :group 'wall
  :safe t)

(defcustom wall-program-switches "-g +0-0"
  "Program switches that will be used by `wall-program'."
  :type 'string
  :group 'wall
  :safe t)

(defcustom wall-message-prefix "[Wall-e]: "
  "The `wall-mode' message prefix."
  :type 'string
  :group 'wall
  :safe t)

(defcustom wall-debug-messages-flag nil
  "Non-nil means activate debug messages."
  :type 'boolean
  :group 'wall
  :safe t)

(defcustom wall-random-flag nil
  "Non-nil means rotates wallpaper at random."
  :type 'boolean
  :group 'wall
  :safe t)

(defcustom wall-timer-flag nil
  "Non-nil means start timer facility."
  :type 'boolean
  :group 'wall
  :safe t)

(defcustom  wall-countdown 600
  "Wallpaper rotation interval in seconds.
Default 10 minutes."
  :type 'string
  :group 'wall
  :safe t)

(defcustom wall-images-list '()
  "Wallpapers images list"
  :type 'directory
  :group 'wall
  :safe t)

(defcustom wall-minor-mode-string "wall-e"
  "String to be displayed in the mode-line."
  :type 'string
  :group 'wall
  :safe t)

(defvar wall-images-ext-regexp "[.jpg|.png|.jpeg|.git]$"
  "Images extension regex expression.")

(defvar wall-timer nil "")

(defvar wall-gif-timer nil
  "Wallpaper git loop image timer.")

(defvar wall-images-index 0
  "Wallpaper (images) list current index.")

(defvar wall-mode nil
  "Non-nil means that wall-mode is enabled.
Altering this variable directly has no effect.")

(defvar wall-internal-vars
  '(wall-images-list wall-timer)
  "List of internal variables.")

(defmacro wall--debug-message (fmt &rest args)
  "Display a internal message at the bottom of the screen.
See `message' for more information about FMT and ARGS arguments."
  `(when wall-debug-messages-flag
     (message (concat wall-message-prefix ,fmt) ,@args)))

(defun wall--set-images-list (&optional dir)
  "Initialize the wallpaper images list and its length."
  ;; set wallpaper images list.
  (setq wall-images-list
        (directory-files-recursively (or dir wall-root-dir)
                                     wall-images-ext-regexp
                                     nil
                                     t)))

(defun wall--clean-internal-vars ()
  "Clean internal lists."
  ;; clean internal variables
  (dolist (var wall-internal-vars)
    (set var nil))
  ;; restore index and length initial values
  (setq  wall-images-index 0))

(defun wall-run-timer ()
  "Start/Initialize wallpaper rotate timer."
  (interactive)
  (cond
   ;; verify if idle timer was already initialized
   ((not (eq wall-timer nil))
    (wall--debug-message "timer already on"))
   ;; default: run-timer
   (t
    ;; set the idle auxiliary timer
    (setq wall-timer
          (run-with-timer wall-countdown
                          nil
                          'wall-rotate-wallpaper
                          t))
    ;; show debug message
    (wall--debug-message "timer on: countdown %s seconds"
                         wall-countdown))))

(defun wall-cancel-timer ()
  "Cancel/Disables `wall-timer'."
  (interactive)
  ;; remove time if was set
  (if (not wall-timer) nil
    ;; otherwise cancel the timer and update its value (nil)
    (cancel-timer wall-timer)
    (setq wall-timer nil))
  ;; display timer debug message
  (wall--debug-message "timer off"))

(defun wall-reload-timer ()
  "Reload (cancel/start) `wall-timer'.
Invoke this function to apply the new
value of `wall-timer.'"
  (interactive)
  ;; cancel (delete) previous timer
  (wall-cancel-timer)
  ;; run (start) timer
  (wall-run-timer)
  ;; show the current
  (wall--debug-message "current time %ds" wall-countdown))

(defun wall--reload-timer ()
  "Maybe reload the timer."
  ;; if not timer just leave
  (if (not wall-timer) nil
    ;; cancel timer
    (wall-cancel-timer)
    ;; run (start) timer
    (wall-run-timer)))

(defun wall-set-timer (seconds &optional arg)
  "Set `wall-countdown' TIME seconds.
If ARG, force the reloading of timer, otherwise
asks for it."
  (interactive
   (list (read-number "Time in seconds: ")
         current-prefix-arg))
  ;; update idle elapse time (in seconds)
  (setq wall-countdown seconds)
  ;; reload timer
  (if (or arg (y-or-n-p "Reload timer?"))
      (wall-reload-timer)))

(defun wall-set-wallpaper (wallpaper &optional args)
  "Set WALLPAPER (full path image file) as the background.
If ARGS is non-nil asks for the custom program
(`wall-program') arguments."
  (interactive
   ;; get wallpaper (file image)
   (list (read-file-name "Wallpaper: " wall-root-dir nil t)
         ;; get current prefix argument (universal argument)
         (when current-prefix-arg
           (read-string "Args: " wall-program-args))))
  (cond
   ((not (executable-find wall-program))
    (wall--debug-message "Program %s isn't an executable" wall-program))
   ;; verify if the file exists and it's a regular file
   ((or (file-directory-p wallpaper)
        (not (file-exists-p wallpaper)))
    (wall--debug-message "File %s not found" wallpaper))
   (t
    ;; TODO: Research, it is really necessary replace this for start-process?
    (async-shell-command (format "%s %s %s %s"
                                 wall-program
                                 (concat wall-program-args (or args ""))
                                 wall-program-switches
                                 wallpaper)))))

(defun wall-rotate-wallpaper (&optional random)
  "Set next/or RANDOM wallpaper.
If optional RANDOM argument isn't nil, set next wallpaper
'randomly'."
  (interactive "P")
  (let ((lim (length wall-images-list)))
    (when (> lim 0)
      (let ((n (+ wall-images-index 1)))
        ;; if not random update images current index
        (if (not random)
            (setq wall-images-index (if (> n lim) 0 n))
          ;; otherwise get a random value (and update images current index)
          (setq wall-images-index (cl-random lim)))
        ;; set the wallpaper
        (wall-set-wallpaper (nth wall-images-index wall-images-list))
        ;; reload (maybe) timer
        (wall--reload-timer)))))

(defun wall-random-wallpaper ()
  "Set random wallpaper."
  (interactive)
  (wall-rotate-wallpaper t))

(defun wall-echo-wallpapers-number ()
  "Set random wallpaper."
  (interactive)
  (message "[Wall-e]: %d wallapers"
           (length wall-images-list)))

(defun wall-echo-rotate-interval ()
  "Set random wallpaper."
  (interactive)
  (message "[Wall-e]: rotate interval %d minutes"
           (/ wall-countdown 60)))

(defun wall--minibuffer-read (prompt choices)
  "Read from the `minibuffer' using PROMPT and CHOICES as candidates.
Report an error unless a valid docset is selected."
  (let ((completion-ignore-case t))
    (completing-read (format "%s (%s): " prompt (car choices))
                     choices nil t nil nil choices)))

(defun wall-add-wallpaper (wallpaper)
  "Add a wallpaper image in the `wall-images-list.'"
  (interactive
   ;; get wallpaper (image file)
   (list (read-file-name "Wallpaper: "
                         wall-root-dir nil t)))
  ;; verify file extension
  (when (string-match-p wall-images-ext-regexp
                        wallpaper)
    (let ((wallpaper (expand-file-name wallpaper)))
      ;; add if its not already a member
      (when (not (member wallpaper wall-images-list))
        (push wallpaper wall-images-list)))))

(defun wall-del-wallpaper (wallpaper)
  "Delete a WALLPAPER image from the `wall-images-list.'"
  ;; map wallpaper argument
  ;; read the argument using the minibuffer
  (interactive
   (list (wall--minibuffer-read "Image:"
                                wall-images-list)))
  ;; delete images from wallpapers image list
  (setq wall-images-list
        (delete wallpaper wall-images-list)))

(defun wall-clean-all-wallpapers ()
  "Empty (clean) wallpaper images list, i.e, set `wall-images-list' to nil."
  (interactive)
  ;; empty wall-images-lit
  (setq wall-images-list '()))

(defun wall-toggle-debug-messages (&optional arg)
  "Toggle `wall-debug-messages-flag' bool value.
If optional ARG is non-nil, force the activation of
 debug messages."
  (interactive "P")
  ;; toggle logic
  (setq wall-debug-messages-flag
        (or arg (not wall-debug-messages-flag)))
  ;; display log message in echo area
  (message "[Wall-e]: Debug messages: %s"

           (if wall-debug-messages-flag "on" "off")))
(defun wall-echo-mode-state ()
  "Show `wall-mode' state: on or off."
  (interactive)
  (message "[Wall-e]: mode %s" (if wall-mode "on" "off")))

;;;###autoload
(define-minor-mode wall-mode
  "Define a new minor mode `wall-mode'.

This defines the toggle command `wall-mode' and (by default)
a control variable `wall-mode'.

Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise."

  :group wall
  :lighter wall-minor-mode-string
  (cond
   (wall-mode
    ;; set internal wallpaper images lists
    (wall--set-images-list)
    ;; start timer
    (when wall-timer-flag (wall-run-timer))
    ;; set mode indicator: true
    (setq wall-mode t))
   (t
    ;; cancel the timer
    (wall-cancel-timer)
    ;; clean internal variables
    (wall--clean-internal-vars)
    ;; set mode indicator: false (nil)
    (setq wall-mode nil))))

;;;###autoload
(defun turn-on-wall-mode ()
  "Enables wall-el minor-mode."
  (interactive)
  ;; turn on wall-mode mode
  (wall-mode 1)
  ;; show wall-mode mode state: on/off
  (wall-echo-mode-state))

(defun turn-off-wall-mode ()
  "Disables wall-el minor-mode."
  (interactive)
  ;; turn off wall-mode mode
  (wall-mode 0)
  ;; show wall-mode mode state
  (wall-echo-mode-state))

(provide 'wall)

;;; wall.el ends here
