;;; ido-vertical-mode.el --- Makes ido-mode display vertically.

;; Copyright (C) 2013, 2014  Steven Degutis

;; Author: Steven Degutis
;; Maintainer: Daniel Gempesaw <gempesaw@gmail.com>
;; Version: 0.1.2
;; Keywords: convenience
;; URL: https://github.com/gempesaw/ido-vertical-mode.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Makes ido-mode display prospects vertically

;;; Code:

(require 'ido)
(defvar ido-show-confirm-message)
(defvar ido-directory-nonreadable)
(defvar ido-directory-too-big)

(defvar ido-vertical-decorations
  '("\n-> "                             ; left bracket around prospect list
    ""                                  ; right bracket around prospect list
    "\n   "                             ; separator between prospects, depends on `ido-separator`
    "\n   ..."                          ; inserted at the end of a truncated list of prospects
    "["                                 ; left bracket around common match string
    "]"                                 ; right bracket around common match string
    " [No match]"
    " [Matched]"
    " [Not readable]"
    " [Too big]"
    " [Confirm]"
    "\n-> "                             ; left bracket around the sole remaining completion
    ""                                  ; right bracket around the sole remaining completion
    )

  "Changing the decorations does most of the work for ido-vertical

This sets up newlines and arrows before, between, and after the
prospects. For additional information, see `ido-decorations'.")

(defvar ido-vertical-old-decorations nil
  "The original `ido-decorations' variable

We need to keep track of the original value so we can restore it
when turning `ido-vertical-mode' off")

(defvar ido-vertical-old-completions nil
  "The original `ido-completions' function

We need to keep track of the original value of `ido-completions'
so we can restore it when turning `ido-vertical-mode' off")

(defgroup ido-vertical-mode nil
  "Make ido behave vertically."
  :group 'ido)

(defcustom ido-vertical-define-keys 'C-n-and-C-p-only
  "Defines which keys that `ido-vertical-mode' redefines."
  :type '(choice
          (const :tag "Keep default ido keys." nil)
          (const :tag "C-p and C-n are up & down in match" C-n-and-C-p-only)
          (const :tag "C-p/up and C-n/down are up and down in match." C-n-C-p-up-and-down)
          (const :tag "C-p/up, C-n/down are up/down in match. left or right cycle history or directory." C-n-C-p-up-down-left-right))
  :group 'ido-vertical-mode)

;; borrowed from ido.el and modified to work better when vertical
(defun ido-vertical-completions (name)
  ;; Return the string that is displayed after the user's text.
  ;; Modified from `icomplete-completions'.

  (let* ((comps ido-matches)
         (ind (and (consp (car comps)) (> (length (cdr (car comps))) 1)
                   ido-merged-indicator))
         first)

    (if (and ind ido-use-faces)
        (put-text-property 0 1 'face 'ido-indicator ind))

    (if (and ido-use-faces comps)
        (let* ((fn (ido-name (car comps)))
               (ln (length fn)))
          (setq first (format "%s" fn))
          (put-text-property 0 ln 'face
                             (if (= (length comps) 1)
                                 (if ido-incomplete-regexp
                                     'ido-incomplete-regexp
                                   'ido-only-match)
                               'ido-first-match)
                             first)
          (if ind (setq first (concat first ind)))
          (setq comps (cons first (cdr comps)))))

    (cond ((null comps)
           (cond
            (ido-show-confirm-message
             (or (nth 10 ido-decorations) " [Confirm]"))
            (ido-directory-nonreadable
             (or (nth 8 ido-decorations) " [Not readable]"))
            (ido-directory-too-big
             (or (nth 9 ido-decorations) " [Too big]"))
            (ido-report-no-match
             (nth 6 ido-decorations))  ;; [No match]
            (t "")))
          (ido-incomplete-regexp
           (concat " " (car comps)))
          ((null (cdr comps))           ;one match
           (concat (concat (nth 11 ido-decorations)  ;; [ ... ]
                           (ido-name (car comps))
                           (nth 12 ido-decorations))
                   (if (not ido-use-faces) (nth 7 ido-decorations))))  ;; [Matched]
          (t                            ;multiple matches
           (let* ((items (if (> ido-max-prospects 0) (1+ ido-max-prospects) 999))
                  (alternatives
                   (apply
                    #'concat
                    (cdr (apply
                          #'nconc
                          (mapcar
                           (lambda (com)
                             (setq com (ido-name com))
                             (setq items (1- items))
                             (cond
                              ((< items 0) ())
                              ((= items 0) (list (nth 3 ido-decorations))) ; " | ..."
                              (t
                               (list (or ido-separator (nth 2 ido-decorations)) ; " | "
                                     (let ((str (substring com 0)))
                                       (if (and ido-use-faces
                                                (not (string= str first))
                                                (ido-final-slash str))
                                           (put-text-property 0 (length str) 'face 'ido-subdir str))
                                       str)))))
                           comps))))))

             (concat
              ;; put in common completion item -- what you get by pressing tab
              (if (and (stringp ido-common-match-string)
                       (> (length ido-common-match-string) (length name)))
                  (concat (nth 4 ido-decorations)   ;; [ ... ]
                          (substring ido-common-match-string (length name))
                          (nth 5 ido-decorations)))
              ;; list all alternatives
              (nth 0 ido-decorations)  ;; { ... }
              alternatives
              (nth 1 ido-decorations)))))))

(defun ido-vertical-disable-line-truncation ()
  "Prevent the newlines in the minibuffer from being truncated"
  (set (make-local-variable 'truncate-lines) nil))

(defun turn-on-ido-vertical ()
  (if (and (eq nil ido-vertical-old-decorations)
         (eq nil ido-vertical-old-completions))
      (progn
        (setq ido-vertical-old-decorations ido-decorations)
        (setq ido-vertical-old-completions (symbol-function 'ido-completions))))

  (setq ido-decorations ido-vertical-decorations)
  (fset 'ido-completions 'ido-vertical-completions)

  (add-hook 'ido-minibuffer-setup-hook 'ido-vertical-disable-line-truncation)
  (add-hook 'ido-setup-hook 'ido-vertical-define-keys))

(defun turn-off-ido-vertical ()
  (setq ido-decorations ido-vertical-old-decorations)
  (fset 'ido-completions ido-vertical-old-completions)

  (remove-hook 'ido-minibuffer-setup-hook 'ido-vertical-disable-line-truncation)
  (remove-hook 'ido-setup-hook 'ido-vertical-define-keys))

(defun ido-vertical-next-match ()
  "Call the correct next-match function for right key.

This is based on:
- Different functions for completing directories and prior history.
"
  (interactive)
  (cond
   ((and (boundp 'item) item (eq item 'file))
    (ido-next-match-dir))
   (t
    (next-history-element 1))))

(defun ido-vertical-prev-match ()
  "Call the correct prev-match function for left key.

This is based on:
- Different functions for completing directories and prior history.
"
  (interactive)
  (cond
   ((and (boundp 'item) item (eq item 'file))
    (ido-prev-match-dir))
   (t
    (previous-history-element 1))))

(defun ido-vertical-define-keys () ;; C-n/p is more intuitive in vertical layout
  (when ido-vertical-define-keys
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
    (define-key ido-completion-map (kbd "M-p") 'ido-toggle-prefix))
  (when (memq ido-vertical-define-keys '(C-n-C-p-up-and-down C-n-C-p-up-down-left-right))
    (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
    (define-key ido-completion-map (kbd "<down>") 'ido-next-match))
  (when (eq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (define-key ido-completion-map (kbd "<left>") 'ido-vertical-prev-match)
    (define-key ido-completion-map (kbd "<right>") 'ido-vertical-next-match)))

;;;###autoload
(define-minor-mode ido-vertical-mode
  "Makes ido-mode display vertically."
  :global t
  (if ido-vertical-mode
      (turn-on-ido-vertical)
    (turn-off-ido-vertical))
  (message "ido-vertical-mode has been %s"
           (if ido-vertical-mode "enabled" "disabled")))

(provide 'ido-vertical-mode)
;;; ido-vertical-mode.el ends here
