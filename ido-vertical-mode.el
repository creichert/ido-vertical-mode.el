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

(defvar ido-vertical-count-active nil
  "Used internally to track whether we're already showing the count")

(defcustom ido-vertical-define-keys nil
  "Defines which keys that `ido-vertical-mode' redefines."
  :type '(choice
          (const :tag "Keep default ido keys." nil)
          (const :tag "C-p and C-n are up & down in match" C-n-and-C-p-only)
          (const :tag "C-p/up and C-n/down are up and down in match." C-n-C-p-up-and-down)
          (const :tag "C-p/up, C-n/down are up/down in match. left or right cycle history or directory." C-n-C-p-up-down-left-right)
          (const :tag "Left, right, up and down move around in the grid" grid-movement))
  :group 'ido-vertical-mode)

(defcustom ido-vertical-pad-list t
  "Non nil means to pad the list of candidates to ensure the minibuffer area is always tall"
  :type 'boolean
  :group 'ido-vertical-mode)

(defcustom ido-vertical-columns 1
  "Display up to this many columns of suggestions"
  :type 'integer
  :group 'ido-vertical-mode)

(defcustom ido-vertical-rows 8
  "Display this many rows of suggestions"
  :type 'integer
  :group 'ido-vertical-mode)

(defcustom ido-vertical-disable-if-short nil
  "Non nil means that ido will go back to horizontal mode if the candidates all fit in the minibuffer area"
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

(defun ido-vertical-or-horizontal-completions (name)
  (if (and ido-vertical-disable-if-short
           (<= (length ido-matches) ido-max-prospects))

      (let ((short-result
             (let ((ido-decorations ido-vertical-old-decorations))
               (funcall ido-vertical-old-completions name))))
        (if (>= (window-body-width (minibuffer-window))
                (+ (minibuffer-prompt-width)
                   (length short-result)))
            short-result
          (ido-vertical-completions name)))

    (ido-vertical-completions name)))

(defvar ido-vertical--visible-count 0
  "how many items got drawn - modified by using ido-vertical--pack-columns")

(defvar ido-vertical--visible-rows 0)
(defvar ido-vertical--visible-cols 0)

(defvar ido-vertical--offset 0
  "offset into the drawn items")

(add-hook 'ido-setup-hook
          (lambda () (setf ido-vertical--offset 0)))

(defun ido-vertical-move (fn)
  (let* ((current-row (% ido-vertical--offset ido-vertical--visible-rows))
         (current-col (/ ido-vertical--offset ido-vertical--visible-rows))
         (posn (funcall fn current-row current-col))
         (row (car posn))
         (col (cdr posn)))
    (setf ido-vertical--offset
          (+ row
             (* col ido-vertical--visible-rows)))

    (when (>= ido-vertical--offset
              ido-vertical--visible-count)
      (when ido-matches
        (let ((next (nth ido-vertical--offset ido-matches)))
          (setq ido-cur-list (ido-chop ido-cur-list next))
          (setq ido-rescan t)
          (setq ido-rotate t)))

      (setf ido-vertical--offset 0))

    (when (< ido-vertical--offset 0)
      (setf ido-vertical--offset 0)
      (ido-prev-match))))

(defun ido-vertical-grid-down ()
  "Put first element of `ido-matches' at the end of the list."
  (interactive)
  (ido-vertical-move ;; todo make me a macro?
   (lambda (row col)
     (let ((newrow (1+ row)))
       (if (= newrow ido-vertical--visible-rows)
           (cons 0 (1+ col))
         (cons newrow col))))))

(defun ido-vertical-grid-up ()
  "Put last element of `ido-matches' at the front of the list."
  (interactive)
  (ido-vertical-move
   (lambda (row col)
     (let ((newrow (- row 1)))
       (if (< newrow 0)
           (cons (- ido-vertical--visible-rows 1) (- col 1))
         (cons newrow col))))))

(defun ido-vertical-grid-right ()
  (interactive)
  (ido-vertical-move
   (lambda (row col)
     (let ((newcol (1+ col)))
       (if (>= newcol ido-vertical--visible-cols)
           (cons (1+ row) 0)
         (cons row newcol))))))

(defun ido-vertical-grid-left ()
  (interactive)
  (ido-vertical-move
   (lambda (row col)
     (let ((newcol (- col 1)))
       (if (< newcol 0)
           (cons (- row 1) (- ido-vertical--visible-cols 1))
         (cons row newcol))))))


(defun ido-vertical--string-with-face (s face)
  "A convenience for taking a string, removing any faces, and then adding a f ace."
  (let ((s (substring-no-properties s 0)))
    (when ido-use-faces
        (add-face-text-property 0 (length s) face nil s))
    s))

;; TODO rotate grid. not sure how ido really works so this is hard to get right.

(defun ido-vertical--pack-columns (items     ;; things to pack
                                   prefix0   ;; prefix of first line
                                   prefix    ;; prefix of other lines
                                   separator ;; separator between columns
                                   ellipsis  ;; ellipsis string to show
                                   width     ;; max width of a column
                                   rows      ;; max number of rows
                                   decorate  ;; function used to fontify etc. items

                                   pad-rows  ;; if not-nil pad to number of rows
                                   max-columns ;; max column count
                                   )
  (let ((separator-length (length separator))
        current-column
        columns
        (column-width 0)
        (index 0)
        (max-item-length 0))

    (setf ido-vertical--visible-count 0
          ido-vertical--visible-rows rows
          ido-vertical--visible-cols 0)

    (while items
      (let* ((item (funcall decorate index (pop items)))
             (item-length (length item))
             (separator-length (if columns separator-length 0))
             (item-row (length current-column))
             )

        (incf index)
        ;; add the item to the column

        (push item current-column)
        (setf max-item-length (max max-item-length item-length))
        ;; check whether we fit
        (let ((new-row-width (+ (* max-item-length (+ 1 (length columns)))
                                (* separator-length (length columns))
                                )))

          (if (and columns (or (>= (length columns) max-columns)
                               (>= new-row-width width)))
              ;; discard this part of solution and end while loop
              (progn (setf items nil)
                     (setf (nth (- rows 1) (car columns)) ellipsis)
                     (decf ido-vertical--visible-count))

            ;; keep it, and if it's the end then accumulate it
            (when (or (null items) (= item-row (- rows 1)))
              ;; new row
              (incf ido-vertical--visible-cols)
              (push (nreverse current-column) columns)
              (setf current-column nil))))
        ))

    ;; at this point columns contains the columns, so we can produce the rows accordingly.
    ;(message (format "%s %d" min-row-widths width))
    (let* ((row-list (make-list rows nil))
           (column-index 0)
           (column-count (length columns)))

      ;; append each column to each row
      (dolist (column columns)
        (let ((max-width max-item-length)
                                        ; this next is if you want variable width columns
                                        ;(apply #'max (mapcar #'length column)))
              (row-list row-list))

          (dolist (cell column)
            (incf ido-vertical--visible-count)
            (push (if (zerop column-index)
                       cell
                       (concat (pad-string cell max-width) separator))

                  (car row-list))
            (pop row-list)
            )
          (incf column-index)
          ))

      (if pad-rows
          (dotimes (row rows)
            (when (null (nth row row-list))
                        (setf (nth row row-list)
                              (list ""))))

        (setq row-list (delete nil row-list)))



      (dolist (row row-list)
        (setcdr row (cons (car row) (cdr row)))
        (setcar row prefix0)
        (setf prefix0 prefix))

      (mapconcat (lambda (x) (apply #'concat x))
                 row-list
                 "\n")
      )))

(defun pad-string (string width)
  (concat
   string
   (make-list (- width (length string)) 32)))

(defun ido-vertical--fewer-rows (candidates ncandidates available-width separator-width)
  "try and use fewer rows"
  (if (or ido-vertical-pad-list
          (>= ncandidates
              (* ido-vertical-rows ido-vertical-columns)))
            ido-vertical-rows
    ;; if we are not padding and we won't overflow, then layout the other way around
    (let* ((maxwidth
            (apply #'max (mapcar (lambda (x) (length (ido-name x))) candidates)))
           (columns
            (/ (- available-width separator-width) (+ maxwidth separator-width))))
      (if (= columns 0)
          ido-vertical-rows
        (max 1 (min ido-vertical-rows
                    (/ (+ ncandidates columns -1)
                       columns)))))))

(defun ido-vertical--exit-minibuffer (o &rest args)
  ;; make ido know that current match is selected
  (message (format  "exit minibuffer - fiddle offset %d" ido-vertical--offset))
  (dotimes (n ido-vertical--offset)
    (ido-next-match))

  ;; (when ido-matches
  ;;   (let ((next (nth ido-vertical--offset ido-matches)))
  ;;     (setq ido-cur-list (ido-chop ido-cur-list next))
  ;;     (setq ido-rescan t)
  ;;     (setq ido-rotate t)))

  ;; carry on
  (setf ido-vertical--offset 0)
  (apply o args))

(advice-add 'ido-exit-minibuffer :around #'ido-vertical--exit-minibuffer)
(advice-remove 'ido-exit-minibuffer #'ido-vertical--exit-minibuffer)

(defun ido-vertical-completions (name)
  "Produce text to go in the minibuffer from `ido-matches' and NAME"
  (let* ((candidates ido-matches)
         (ncandidates (length ido-matches))

         (ind (and (consp (car candidates))
                   (> (length (cdr (car candidates))) 1)
                   ido-merged-indicator))

         (deco-arrow-and-count (if ido-vertical-show-count
                                   (concat (format " [%d]" ncandidates)
                                           (nth 0 ido-decorations))
                                 (nth 0 ido-decorations)))

         (deco-between-prospect (nth 2 ido-decorations))
         (deco-more-candidates (nth 3 ido-decorations))

         (deco-lb-prefix (nth 4 ido-decorations))
         (deco-rb-prefix (nth 5 ido-decorations))
         (deco-no-match-message (nth 6 ido-decorations))
         (deco-exact-match-message (nth 7 ido-decorations))
         (deco-not-readable-message (nth 8 ido-decorations))
         (deco-too-big-message  (nth 9 ido-decorations) )
         (deco-confirm-message (nth 10 ido-decorations))
         (deco-lb-match (nth 11 ido-decorations))
         (deco-rb-match (nth 12 ido-decorations))
         )

    (cond ((zerop ncandidates)          ; no candidates: we just show an informative string
           (cond
            (ido-show-confirm-message
             (or deco-confirm-message " [Confirm]"))
            (ido-directory-nonreadable
             (or deco-not-readable-message  " [Not readable]"))
            (ido-directory-too-big
             (or deco-too-big-message " [Too big]"))
            (ido-report-no-match
             deco-no-match-message) ;; [No match]
            (t "")))

          (ido-incomplete-regexp        ; incomplete regex - wait until it's finished
           (concat " "
                   (ido-vertical--string-with-face
                    (ido-name (car candidates))
                    'ido-incomplete-regexp)))

          ((= 1 ncandidates)            ; exactly 1 candidate - just show that as the match
           (concat
            deco-lb-match

            (ido-vertical--string-with-face
             (ido-name (car candidates))
             'ido-vertical-only-match-face)

            deco-rb-match
            (unless ido-use-faces deco-exact-match-message)))

          (t                            ; more than 1 candidate, so we need to make something up
           (if (and ind ido-use-faces)
               (add-face-text-property 0 1 'ido-indicator nil ind))
           (let* ((decoration-regexp (if ido-enable-regexp ido-text (regexp-quote name)))
                  (available-width (- (window-body-width (minibuffer-window)) 10))
                  (column-separator "   ")
                  (grid

                   (ido-vertical--pack-columns
                    candidates
                    deco-arrow-and-count
                    "   "
                    column-separator
                    "..."

                    available-width
                    (ido-vertical--fewer-rows candidates ncandidates available-width (length column-separator))

                    (lambda (index item)
                      (let ((prospect-name (substring-no-properties (ido-name item) 0)))
                        (when (and ind (= ido-vertical--offset index))
                          (setf prospect-name (concat prospect-name ind)))

                        (when ido-use-faces
                          ;; anything not the first item, with a /, gets ido-subdir face
                          (unless (= ido-vertical--offset index)
                            (if (ido-final-slash prospect-name)
                                (add-face-text-property
                                 0 (length prospect-name)
                                 'ido-subdir
                                 nil prospect-name)))

                          ;; first item gets a special face
                          (when (= ido-vertical--offset index)
                            (add-face-text-property
                             0 (length prospect-name)
                             (cond
                              ((> ncandidates 1) 'ido-vertical-first-match-face)
                              (ido-incomplete-regexp 'ido-incomplete-regexp)
                              (t 'ido-vertical-only-match-face))
                             nil prospect-name))

                          ;; other stuff gets a bit of highlighting
                          (when (string-match decoration-regexp prospect-name)
                            (ignore-errors
                              ;; try and match each group in case it's a regex with groups
                              (let ((group 1))
                                (while (match-beginning group)
                                  (add-face-text-property (match-beginning group)
                                                          (match-end group)
                                                          'ido-vertical-match-face
                                                          nil prospect-name)
                                  (incf group))
                                ;; it's not a regex with groups, so just mark the whole match region.
                                (when (= 1 group)
                                  (add-face-text-property (match-beginning 0)
                                                          (match-end 0)
                                                          'ido-vertical-match-face
                                                          nil prospect-name)
                                  )))))
                        prospect-name))

                    ido-vertical-pad-list
                    ido-vertical-columns
                    ))
                  )


             (if (and (stringp ido-common-match-string)
                        (> (length ido-common-match-string) (length name)))
               (concat
                deco-lb-prefix
                (substring ido-common-match-string (length name))
                deco-rb-prefix
                grid)
               grid)
             )
           ))))

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
  (fset 'ido-completions 'ido-vertical-or-horizontal-completions)

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
    (define-key ido-completion-map (kbd "C-c C-t") 'ido-toggle-prefix))
  (when (memq ido-vertical-define-keys '(C-n-C-p-up-and-down C-n-C-p-up-down-left-right))
    (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
    (define-key ido-completion-map (kbd "<down>") 'ido-next-match))
  (when (eq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (define-key ido-completion-map (kbd "<left>") 'ido-vertical-prev-match)
    (define-key ido-completion-map (kbd "<right>") 'ido-vertical-next-match))
  (when (eq ido-vertical-define-keys 'grid-movement)
    (define-key ido-completion-map (kbd "<up>") 'ido-vertical-grid-up)
    (define-key ido-completion-map (kbd "<down>") 'ido-vertical-grid-down)
    (define-key ido-completion-map (kbd "<left>") 'ido-vertical-grid-left)
    (define-key ido-completion-map (kbd "<right>") 'ido-vertical-grid-right)))

;;;###autoload
(define-minor-mode ido-vertical-mode
  "Makes ido-mode display vertically."
  :global t
  (if ido-vertical-mode
      (turn-on-ido-vertical)
    (turn-off-ido-vertical)))

(provide 'ido-vertical-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ido-vertical-mode.el ends here
