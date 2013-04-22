;;; ido-vertical-mode.el --- Makes ido-mode display vertically.

;; Copyright (C) 2013  Steven Degutis

;; Author: Steven Degutis
;; URL: https://github.com/sdegutis/ido-vertical-mode.el
;; Version: 0.1
;; Keywords: convenience

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

;; This contains some tricks (and one hack) that you'd probably rather
;; not have sitting in your init file.  I've submitted the hack to
;; emacs developers in hopes it will become an official configuration
;; rather than a hack, but who knows how long that'll take, if they
;; even accept it in the first place.

;;; Code:


(require 'ido) ;; this is to try to get rid of the warnings, but its not working :(

(defvar sd/ido-decorations)
(setq sd/ido-decorations '("\n-> "
                           ""
                           "\n   "
                           "\n   ..."
                           "["
                           "]"
                           " [No match]"
                           " [Matched]"
                           " [Not readable]"
                           " [Too big]"
                           " [Confirm]"
                           "\n-> "
                           ""))

(defvar sd/old-ido-decorations)
(defvar sd/old-ido-completions)
(defvar sd/old-ido-enable-flex-matching)
(defvar sd/old-ido-auto-merge-delay-time)
(defvar sd/old-ido-ubiquitous-enable-compatibility)

;; borrowed from ido.el and modified to work better when vertical
(defun sd/ido-completions (name)
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
	  ((null (cdr comps))		;one match
	   (concat (concat (nth 11 ido-decorations)  ;; [ ... ]
                           (ido-name (car comps))
                           (nth 12 ido-decorations))
		   (if (not ido-use-faces) (nth 7 ido-decorations))))  ;; [Matched]
	  (t				;multiple matches
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

(defun turn-on-ido-vertical ()
  (setq sd/old-ido-enable-flex-matching ido-enable-flex-matching)
  (setq ido-enable-flex-matching t)

  (setq sd/old-ido-auto-merge-delay-time ido-auto-merge-delay-time)
  (setq ido-auto-merge-delay-time 99999)

  (when (boundp 'ido-ubiquitous-enable-compatibility)
    (setq sd/old-ido-ubiquitous-enable-compatibility ido-ubiquitous-enable-compatibility)
    (setq ido-ubiquitous-enable-compatibility nil))

  (setq sd/old-ido-decorations ido-decorations)
  (setq sd/old-ido-completions (symbol-function 'ido-completions))

  (setq ido-decorations sd/ido-decorations)
  (fset 'ido-completions 'sd/ido-completions)

  (add-hook 'ido-minibuffer-setup-hook 'sd/ido-disable-line-truncation)
  (add-hook 'ido-setup-hook 'sd/ido-define-keys))

(defun turn-off-ido-vertical ()
  (setq ido-enable-flex-matching sd/old-ido-enable-flex-matching)
  (setq ido-auto-merge-delay-time sd/old-ido-auto-merge-delay-time)
  (when (boundp 'ido-ubiquitous-enable-compatibility)
    (setq ido-ubiquitous-enable-compatibility sd/old-ido-ubiquitous-enable-compatibility))
  (setq ido-decorations sd/old-ido-decorations)
  (fset 'ido-completions sd/old-ido-completions)

  (remove-hook 'ido-minibuffer-setup-hook 'sd/ido-disable-line-truncation)
  (remove-hook 'ido-setup-hook 'sd/ido-define-keys))

;;;###autoload
(define-minor-mode ido-vertical-mode
  "Makes ido-mode display vertically."
  :global t
  (if ido-vertical-mode
      (turn-on-ido-vertical)
    (turn-off-ido-vertical)))

;; remap C-n C-p for vertical ido-mode
(defun sd/ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(defun sd/ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))


(provide 'ido-vertical-mode)
;;; ido-vertical-mode.el ends here
