;;; ido-vertical-mode.el --- Makes ido-mode display vertically.

;; Copyright (C) 2013, 2014  Steven Degutis

;; Author: Steven Degutis
;; Maintainer: Christopher Reichert <creichert07@gmail.com>
;; Version: 1.0.0
;; Keywords: convenience
;; URL: https://github.com/creichert/ido-vertical-mode.el

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

;;; The following three variables and their comments are lifted
;;; directly from `ido.el'; they are defined here to avoid compile-log
;;; warnings. See `ido.el' for more information.

;; Non-nil if we should add [confirm] to prompt
(defvar ido-show-confirm-message)

;; Remember if current directory is non-readable (so we cannot do completion).
(defvar ido-directory-nonreadable)

;; Remember if current directory is 'huge' (so we don't want to do completion).
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

(defcustom ido-vertical-show-count nil
  "Non nil means show the count of candidates while completing."
  :type 'boolean
  :group 'ido-vertical-mode)

(defcustom ido-vertical-define-keys nil
  "Defines which keys that `ido-vertical-mode' redefines."
  :type '(choice
          (const :tag "Keep default ido keys." nil)
          (const :tag "C-p and C-n are up & down in match" C-n-and-C-p-only)
          (const :tag "C-p/up and C-n/down are up and down in match." C-n-C-p-up-and-down)
          (const :tag "C-p/up, C-n/down are up/down in match. left or right cycle history or directory." C-n-C-p-up-down-left-right))
  :group 'ido-vertical-mode)

(defcustom ido-vertical-keep-static nil
  "Non nil means to keep the completion list static while moving prev/next elements."
  :type 'boolean
  :group 'ido-vertical-mode)

(defface ido-vertical-first-match-face
  '((t (:inherit ido-first-match)))
  "Face used by Ido Vertical for highlighting first match."
  :group 'ido-vertical-mode)

(defface ido-vertical-only-match-face
  '((t (:inherit ido-only-match)))
  "Face used by Ido Vertical for highlighting only match."
  :group 'ido-vertical-mode)

(defface ido-vertical-match-face
  '((t (:inherit font-lock-variable-name-face :bold t :underline t)))
  "Face used by Ido Vertical for the matched part."
  :group 'ido-vertical-mode)

;; borrowed from ido.el and modified to work better when vertical
(defun ido-vertical-completions (name)
  ;; Return the string that is displayed after the user's text.
  ;; Modified from `icomplete-completions'.

  (let* ((completions ido-matches)
         (completions-length (length completions))
         (completions-empty (null completions))
         (minibuffer-available-lines (1- (min ido-max-prospects (ido-vertical-minibuffer-max-height))))
         (additional-items-indicator (nth 3 ido-decorations)))

    ;; Keep the height of the suggestions list constant by padding
    ;; when completions-length is too small. Also, if completions-length is too short, we
    ;; should not indicate that there are additional prospects.
    (when (< completions-length minibuffer-available-lines)
      (setq additional-items-indicator "\n")
      (setq completions (append completions (make-list (- (1+ ido-max-prospects) completions-length) ""))))

    (when ido-use-faces
      ;; Make a copy of [ido-matches], otherwise the selected string
      ;; could contain text properties which could lead to weird
      ;; artifacts, e.g. buffer-file-name having text properties.
      (when (eq completions ido-matches)
        (setq completions (copy-sequence ido-matches)))

      (dotimes (i minibuffer-available-lines)
        (setf (nth i completions) (substring (if (listp (nth i completions))
                                                 (car (nth i completions))
                                               (nth i completions))
                                             0))
        (when (string-match name (nth i completions))
          (ignore-errors
            (add-face-text-property (match-beginning 0)
                                    (match-end 0)
                                    'ido-vertical-match-face
                                    nil (nth i completions))))))

    ;; Previously we'd check null completions to see if the list was
    ;; empty. We pad the list with empty items to keep the list at a
    ;; constant height, so we have to check if the entire list is
    ;; empty, instead of (null completions)
    (cond (completions-empty
           (cond
            (ido-show-confirm-message
             (or (nth 10 ido-decorations) " [Confirm]"))
            (ido-directory-nonreadable
             (or (nth 8 ido-decorations) " [Not readable]"))
            (ido-directory-too-big
             (or (nth 9 ido-decorations) " [Too big]"))
            (ido-report-no-match
             (nth 6 ido-decorations)) ;; [No match]
            (t "")))
          (ido-incomplete-regexp
           (concat " " (car completions)))
          ((null (cdr completions))                       ;one match
           (concat (concat (nth 11 ido-decorations) ;; [ ... ]
                           (ido-name (car completions))
                           (nth 12 ido-decorations))
                   (when (not ido-use-faces) (nth 7 ido-decorations)))) ;; [Matched]
          (t                            ;multiple matches
           (let* ((items minibuffer-available-lines)
                  (items-count items)
                  (alternatives
                   (apply
                    #'concat
                    (cdr (apply
                          #'nconc
                          (mapcar
                           (lambda (completion)
                             (setq completion (ido-name completion))
                             (setq items (1- items))
                             (cond
                              ((< items 0) ())
                              ((= items 0) (list additional-items-indicator)) ; " | ..."
                              (t
                               (list (nth (if (= (- items-count items 1) ido-vertical-selected-offset) 0 2) ido-decorations)
                                     (let ((str (substring completion 0)))
                                       (when ido-use-faces
                                         (ido-vertical-propertize-text 0
                                                                       (length str)
                                                                       (cond ((= completions-length 1)
                                                                              'ido-vertical-only-match-face)
                                                                             ((= (- items-count items 1) ido-vertical-selected-offset)
                                                                              'ido-vertical-first-match-face)
                                                                             (ido-incomplete-regexp
                                                                              'ido-incomplete-regexp)
                                                                             ((ido-final-slash str)
                                                                              'ido-subdir))
                                                                       str))
                                       str)))))
                           completions))))))

             (concat
              ;; put in common completion item -- what you get by pressing tab
              (when (and (stringp ido-common-match-string)
                         (> (length ido-common-match-string) (length name)))
                (concat (nth 4 ido-decorations) ;; [ ... ]
                        (substring ido-common-match-string (length name))
                        (nth 5 ido-decorations)))
              ;; list all alternatives
              (let ((decoration (nth (if (= 0 ido-vertical-selected-offset) 0 2) ido-decorations)))
                (when ido-vertical-show-count
                  (setq decoration (format " [%d]%s" completions-length decoration)))
                decoration)
              alternatives
              (nth 1 ido-decorations)))))))

(defun ido-vertical-propertize-text (start stop face str)
  "Wrapper for `add-face-text-property' or `put-text-property'."
  (if (fboundp 'add-face-text-property)
      (add-face-text-property start stop face nil str)
    (put-text-property start stop 'face face str)))

(defun ido-vertical-minibuffer-max-height ()
  "Compute the maximum theoric size of the minibuffer."
  (floor (* max-mini-window-height (/ (frame-pixel-height) (frame-char-height)))))

(defun ido-vertical-disable-line-truncation ()
  "Prevent the newlines in the minibuffer from being truncated"
  (set (make-local-variable 'truncate-lines) nil))

(defun turn-on-ido-vertical ()
  (when (and (eq nil ido-vertical-old-decorations)
             (eq nil ido-vertical-old-completions))
    (setq ido-vertical-old-decorations ido-decorations)
    (setq ido-vertical-old-completions (symbol-function 'ido-completions)))

  (setq ido-decorations ido-vertical-decorations)
  (fset 'ido-completions 'ido-vertical-completions)

  (add-hook 'ido-minibuffer-setup-hook 'ido-vertical-disable-line-truncation)
  (add-hook 'ido-setup-hook 'ido-vertical-define-keys)
  (add-hook 'ido-setup-hook 'ido-vertical-reset-counters))

(defun turn-off-ido-vertical ()
  (setq ido-decorations ido-vertical-old-decorations)
  (fset 'ido-completions ido-vertical-old-completions)

  (remove-hook 'ido-minibuffer-setup-hook 'ido-vertical-disable-line-truncation)
  (remove-hook 'ido-setup-hook 'ido-vertical-define-keys)
  (remove-hook 'ido-setup-hook 'ido-vertical-reset-counters))

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

(defvar ido-vertical-selected-offset 0
  "Offset of selected item from the top. ")

(defvar ido-vertical-down-count 0
  "How many times `ido-next-match' was called.")

(defun ido-vertical-reset-counters ()
  (setq ido-vertical-selected-offset 0)
  (setq ido-vertical-down-count 0))

(defun ido-vertical-max-visible ()
  (1- (window-body-height)))

(defun ido-vertical-maximum-offset ()
  (1- (min (length ido-matches) (ido-vertical-max-visible))))

(defun ido-vertical-invisible-count ()
  (- (length ido-matches) (1+ (ido-vertical-maximum-offset))))

(defadvice ido-prev-match (around ido-vertical activate)
  "Select the prev element if needed."
  (if (not ido-vertical-keep-static)
      ad-do-it
    (if (> ido-vertical-selected-offset 0)
        (setq ido-vertical-selected-offset (1- ido-vertical-selected-offset))
      (when (> ido-vertical-down-count 0)
        (setq ido-vertical-down-count (1- ido-vertical-down-count))
        ad-do-it))))

(defadvice ido-next-match (around ido-vertical activate)
  "Select the next element if needed."
  (if (not ido-vertical-keep-static)
      ad-do-it
    (if (< ido-vertical-selected-offset (ido-vertical-maximum-offset))
        (setq ido-vertical-selected-offset (1+ ido-vertical-selected-offset))
      (when (< ido-vertical-down-count (ido-vertical-invisible-count))
        (setq ido-vertical-down-count (1+ ido-vertical-down-count))
        ad-do-it))))

(defadvice ido-exit-minibuffer (around ido-vertical activate)
  "Ensure the correct element is selected."
  (if (not ido-vertical-keep-static)
      ad-do-it
    (dotimes (n ido-vertical-selected-offset)
      (let ((ido-vertical-keep-static nil))
        (ido-next-match)))
    ad-do-it))

(defadvice exit-minibuffer (before ido-vertical activate)
  "Ensure counters are reset before entering previous/next directory."
  (ido-vertical-reset-counters))

(defun ido-vertical-define-keys () ;; C-n/p is more intuitive in vertical layout
  (when ido-vertical-define-keys
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
    (define-key ido-completion-map (kbd "C-c C-t") 'ido-toggle-prefix))
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
  :group ido-vertical-mode
  (if ido-vertical-mode
      (turn-on-ido-vertical)
    (turn-off-ido-vertical)))

(provide 'ido-vertical-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ido-vertical-mode.el ends here
