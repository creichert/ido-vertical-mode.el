(require 'ido)
(require 'ert)
(require 'ido-vertical-mode)

(ido-mode 1)
(ido-vertical-mode 1)

;;; invoke ido-switch-buffer to initialize ido variables that would
;;; otherwise throw void error
(execute-kbd-macro [24 98 return] 1)

(ert-deftest ivm-should-install-decorations ()
  (ido-vertical-mode 1)
  (let ((prospects (ido-completions "")))
    (should (string-match "->" prospects))
    (should (string-match "\n" prospects))))

(ert-deftest ivm-should-indicate-more-results ()
  (ido-vertical-mode 1)
  (let ((buffers (mapcar (lambda (num)
                           (get-buffer-create
                            (format "ivm-test-buffer-%s" num)))
                         (number-sequence 1 11)))
        prospects)
    (save-window-excursion
      (execute-kbd-macro [24 98 ?i ?v ?m ?- ?t ?e ?s ?t])
      (setq prospects (ido-completions "ivm-test"))
      (should (string-match "\.\.\.$" prospects)))
    (mapc 'kill-buffer buffers)))

(ert-deftest ivm-should-properly-disable-itself ()
  (ido-vertical-mode 1)
  (ido-vertical-mode -1)
  (should (not (string-match "\n" (ido-completions "")))))

(ert-deftest ivm-should-show-confirm-dialog ()
  (ido-vertical-mode 1)
  (let* ((no-results [24 98 ?t ?h ?i ?s ?s ?h ?o ?u ?l ?d ?n ?o ?t ?m ?a ?t ?c ?h])
         (confirm (vconcat no-results [return])))
    (execute-kbd-macro no-results 1)
    (should (string-match "No Match" (buffer-name (current-buffer))))
    (kill-buffer (current-buffer))
    (execute-kbd-macro confirm 1)
    (should (string-match "Confirm" (buffer-name (current-buffer))))
    (kill-buffer (current-buffer))))

(ert-deftest ivm-should-handle-fontified-candidates ()
  (let ((comps '(""
                 (#(".ido.last" 1 4 (face flx-highlight-face)) "/Users/JS/")
                 (#("200-ido.el" 4 7 (face flx-highlight-face)) "/Users/JS/.emacs.d/configs/" "~/.emacs.d/configs/"))))
    (should (not (ido-vertical-comps-empty-p comps)))
    (should (ido-vertical-comps-empty-p '("" "" "")))
    (setq ido-matches comps)
    (should (ido-vertical-completions ""))))
