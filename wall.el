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
(require 'cl-seq)
(require 'cl-extra)
(require 'ansi-color)

;;; GROUP DEFINITION

(defgroup wall nil
  "Wallpaper management."
  :group 'extensions
  :group 'convenience)

;;; CUSTOM OPTIONS

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

(defcustom wall-message-prefix "[Wall-e]: "
  "The message prefix."
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
  "Wallpapers images list."
  :type 'directory
  :group 'wall
  :safe t)

(defcustom wall-minor-mode-string "wall-e"
  "String to be displayed in the mode-line."
  :type 'string
  :group 'wall
  :safe t)

(defcustom wall-output-buffer-name "WALL-OUTPUT"
  "Wall output buffer name."
  :type 'string
  :group 'wall
  :safe t)

(defcustom wall-create-buffer-flag nil
  "Non-nil means create output buffer."
  :type 'boolean
  :group 'wall
  :safe t)

;;; GLOBAL VARIABLES

(defvar wall-executable (executable-find wall-program)
  "Executable full-path: `wall-program'.")

(defvar wall-images-ext-regexp "[.jpg|.png|.jpeg|.gif]$"
  "Images extension regex expression.")

(defvar wall-timer nil
  "Auxiliary timer.")

(defvar wall-gif-timer nil
  "Wallpaper git loop image timer.")

(defvar wall-images-index 0
  "Wallpaper (images) list current index.")

(defvar wall-current-wallpaper nil
  "Wallpaper current image.")

(defvar wall-images-index-history '()
  "Wallpaper index history.")

(defvar wall-mode nil
  "Non-nil means the mode is enabled.
Altering this variable directly has no effect.
See \\[wall-mode] for more information to turn the mode
on/off.")

(defvar wall-internal-vars
  '(wall-images-list wall-timer)
  "List of internal variables.")

;;; MACROS

(defmacro wall--debug-message (fmt &rest args)
  "Display a internal message at the bottom of the screen.
See `message' for more information about FMT and ARGS arguments."
  `(when wall-debug-messages-flag
     (message (concat wall-message-prefix ,fmt) ,@args)))

;;; UTILS (INTERNAL) FUNCTIONS

(defun wall--set-images-list (&optional dir)
  "Initialize the wallpaper images list and its length.
Use DIR (directory) if is set, otherwise the
value of `wall-root-dir'."
  ;; set wallpaper images list.
  (setq wall-images-list
        (directory-files-recursively (or dir wall-root-dir)
                                     wall-images-ext-regexp
                                     nil
                                     t)))

(defun wall--add-images-list (dir)
  "Add wallpapers images list.
Use DIR (directory) if is set, otherwise the
value of `wall-root-dir'."
  (let ((images (directory-files-recursively dir
                                wall-images-ext-regexp
                                nil
                                t)))
  ;; add wallpaper images list
    (and images
         (setq wall-images-list
               (cl-delete-duplicates
                `(,@images ,@wall-images-list))))))

(defun wall--clean-internal-vars ()
  "Clean internal lists."
  ;; clean internal variables
  (dolist (var wall-internal-vars)
    (set var nil))
  ;; restore index and length initial values
  (setq  wall-images-index 0))

;;; SYSTEM PROCESS RELATED FUNCTIONS

(defun wall--default-sentinel (process _event)
  "Wall default sentinel: PROCESS default EVENT handler function.
This is also a template for another callbacks."
  (let ((status (process-status process)))
    ;; handle process status (nothing for now)
    (or
     (eq status 'exit)
     (eq status 'stop)
     (eq status 'signal)
     (eq status 'closed)
     (eq status 'failed))
    nil))

(defun wall--process-filter (process string)
  "Filter PROCESS output STRING."
  (let ((buffer (process-buffer process)))
    (when buffer
      (with-current-buffer buffer
        (ansi-color-apply-on-region (point-min) (point-max))
        (insert string)))))

(defun wall--set-sentinel (process sentinel)
  "Set wall PROCESS SENTINEL (callback) function to handle events."
  (set-process-sentinel process sentinel))

(defun wall--start-process (&optional sentinel &rest program-args)
  "Start `wall-program' process passing its arguments PROGRAM-ARGS.
Set a SENTINEL (callback) function to handle process
signals and returns."
  ;; create a buffer, if create buffer predicate is true
  (let* ((buffer (when wall-create-buffer-flag
                   (get-buffer-create wall-output-buffer-name)))
         (process (apply 'start-process
                         wall-program
                         buffer
                         wall-executable
                         program-args)))
    ;; verify if process was correctly created
    (unless process
      (error "Was not possible to create %s process" wall-executable))
    ;; set (default or callback) sentinel
    (wall--set-sentinel process (or sentinel 'wall--default-sentinel))
    ;; set default filter
    (set-process-filter process 'wall--process-filter)))

;;; TIMER COMMANDS

(defun wall--reload-timer ()
  "Maybe reload the timer."
  (if (not wall-timer) nil
    (wall-cancel-timer)
    (wall-run-timer)))

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
                          wall-countdown
                          'wall-set-next-wallpaper
                          t)))))

(defun wall-cancel-timer ()
  "Cancel the `wall-timer'."
  (interactive)
  ;; remove time if was set
  (if (not wall-timer) nil
    ;; otherwise cancel the timer and update its value (nil)
    (cancel-timer wall-timer)
    (setq wall-timer nil))
  ;; display timer debug message
  (wall--debug-message "timer off"))

(defun wall-reload-timer ()
  "Reload the running timer."
  (interactive)
  (wall-cancel-timer)
  (wall-run-timer)
  ;; show the current count down timer
  (wall--debug-message "current time %ds" wall-countdown))

(defun wall-update-timer (seconds &optional arg)
  "Set `wall-countdown' count down SECONDS.
If ARG, force the reloading of timer, otherwise
asks for it."
  (interactive
   (list (read-number "Time in seconds: ")
         current-prefix-arg))
  ;; update idle elapse time (in seconds)
  (setq wall-countdown seconds)
  ;; reload timer
  (when (or arg (y-or-n-p "Reload timer? "))
    (wall-reload-timer)))

;; WALLPAPER MANAGEMENT COMMANDS

(defun wall--read-wallpaper ()
  "Read wallpaper file path.
If \\[universal-argument] is non-nil, read program arguments."
  ;; get wallpaper (file image)
  (list (expand-file-name
         (read-file-name "Wallpaper: " wall-root-dir nil t))
        ;; get current prefix argument (universal argument)
        (when current-prefix-arg
          (read-string "Args: " wall-program-args))))

(defun wall-set-wallpaper (wallpaper &optional args)
  "Set WALLPAPER (full path image file) as the background.
If ARGS is non-nil, asks (read using the `minibuffer' facility)
for the programs arguments."
  (interactive (wall--read-wallpaper))
  (cond
   ((not (executable-find wall-program))
    (wall--debug-message "Program %s isn't executable" wall-program))
   ;; verify if the file exists and it's a regular file
   ((or (file-directory-p wallpaper)
        (not (file-exists-p wallpaper)))
    (wall--debug-message "File %s not found" wallpaper))
   (t
    ;; save current wallpaper
    (setq wall-current-wallpaper wallpaper)
    ;; use `start-process' interface to execute the unix command
    (apply 'wall--start-process nil `(,@(split-string
                                         (or args wall-program-args))
                                      ,wallpaper)))))

(defun wall-reset-current-wallpaper ()
  "Reset current wallpaper."
  (interactive)
  ;; reset current wallpaper
  (wall-set-wallpaper (or wall-current-wallpaper
                          (nth wall-images-index
                               wall-images-list))))

(defun wall-update-current-wallpaper-pos (pos)
  "Update current wallpaper POS (position)."
  (interactive "sY-Position : ")
  ;; get current wallpaper
  (let ((wallpaper (or wall-current-wallpaper
                       (nth wall-images-index wall-images-list)))
        ;; get the number as a integer
        (num (string-to-number pos)))
    ;; set default position if none of the conditions meet
    ;; if/else-if/else equivalent
    (or (string-equal pos "-0")
        (string-equal pos "+0")
        (not (equal num 0))
        ;; set default value
        (setq pos "+0"))
    ;; update pos signal indicator (if necessary)
    (when (eq (string-match-p "^[\+\-]" pos) nil)
      (setq pos (concat "+" pos)))
    ;; set new wallpaper Y position
    (wall-set-wallpaper wallpaper (format "--bg-fill -g +0%s" pos))))

(defun wall-set--next-wallpaper (n &optional random)
  "Set next wallpaper +N.
If RANDOM is non nil, set next wallpaper at random."
  ;; first verify if have a images list
  (if (not wall-images-list) nil
    (let* ((lim (length wall-images-list))
           (n (if (= lim 1) 0 (+ wall-images-index n))))
      ;; if not random update images current index (save global index)
      (setq wall-images-index (if random
                                  (cl-random lim)
                                (if (> n lim) 0 n)))
      (let ((wallpaper (nth wall-images-index wall-images-list)))
        ;; set the wallpaper
        (wall-set-wallpaper wallpaper)
        ;; show me the wallpaper set
        (wall--debug-message "[%d] %s" wall-images-index wallpaper)))))

(defun wall-set-next-wallpaper (&optional random)
  "Set next wallpaper or RANDOM one."
  (interactive "P")
  (wall-set--next-wallpaper 1 random))

(defun wall-set-random-wallpaper ()
  "Set random wallpaper."
  (interactive)
  (wall-set--next-wallpaper 1 t))

(defun wall-set-wallpaper-forward ()
  "Set next wallpaper."
  (interactive)
  (wall-set--next-wallpaper 1))

(defun wall-set-wallpaper-backward ()
  "Set previous wallpaper."
  (interactive)
  (wall-set--next-wallpaper -1))

(defun wall-show-wallpapers-number ()
  "Set random wallpaper."
  (interactive)
  (message "[Wall-e]: %d wallapers" (length wall-images-list)))

(defun wall-show-rotate-interval ()
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
  "Add a WALLPAPER image in the `wall-images-list'."
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
  "Delete a WALLPAPER image from the `wall-images-list'."
  (interactive
   (list (wall--minibuffer-read "Image:" wall-images-list)))
  ;; delete images from wallpapers image list
  (setq wall-images-list (delete wallpaper wall-images-list)))

(defun wall-clean-all-wallpapers ()
  "Empty (clean) wallpaper images list, i.e, set `wall-images-list' to nil."
  (interactive)
  ;; empty wall-images-lit
  (setq wall-images-list '()))

(defun wall-set-wallpaper-list (dir)
  "Set wallpapers images list from the DIR."
  (interactive
   (list (expand-file-name
          (read-directory-name "Directory: "
                               wall-root-dir))))
  ;; set wallpaper images list
  (wall--set-images-list dir))

(defun wall-add-wallpaper-list (dir)
  "Set wallpapers images list from the DIR."
  (interactive
   (list (expand-file-name
          (read-directory-name "Directory: "
                               wall-root-dir))))
  ;; set wallpaper images list
  (wall--add-images-list dir))

(defun wall-toggle-debug-messages (&optional arg)
  "Toggle `wall-debug-messages-flag' bool value.
If optional ARG is non-nil, force the activation of
 debug messages."
  (interactive "P")
  ;; toggle logic
  (setq wall-debug-messages-flag (or arg (not wall-debug-messages-flag)))
  ;; display log message in echo area
  (message "[Wall-e]: Debug messages: %s"
           (if wall-debug-messages-flag "on" "off")))

(defun wall-show-mode-state ()
  "Show mode state: on or off."
  (interactive)
  (message "[Wall-e]: mode %s" (if wall-mode "on" "off")))

;;; MINOR MODE DEFINITION

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

;;; MINOR MODE ON/OFF COMMANDS

;;;###autoload
(defun turn-on-wall-mode ()
  "Turn the minor-mode on."
  (interactive)
  ;; turn on
  (wall-mode 1)
  ;; show the state
  (wall-show-mode-state))

(defun turn-off-wall-mode ()
  "Turn the minor-mode off."
  (interactive)
  ;; turn on
  (wall-mode 0)
  ;; show state
  (wall-show-mode-state))

(provide 'wall)

;;; wall.el ends here
