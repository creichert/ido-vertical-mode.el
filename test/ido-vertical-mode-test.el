(require 'ido)
(require 'ert)
(require 'ido-vertical-mode)

(ido-mode 1)
(ido-vertical-mode 1)

;;; invoke ido-swich-buffer to initialize ido variables that would
;;; otherwise throw void error
(execute-kbd-macro [24 98 return] 1)

(ert-deftest ido-vertical-decorations-installed ()
  (ido-vertical-mode 1)
  (let ((prospects (ido-completions "")))
    (should (string-match "->" prospects))
    (should (string-match "\n" prospects))))

(ert-deftest ivm-indicate-more-results ()
  (ido-vertical-mode 1)
  (let ((buffers (mapcar (lambda (num)
                           (get-buffer-create
                            (format "ivm-test-buffer-%s" num)))
                       (number-sequence 1 11)))
      completions)
  (save-window-excursion
    (kmacro-exec-ring-item '("bivm-test" 0 "%d") nil)
    (setq completions (ido-completions "ivm-test"))
    (should (string-match "\.\.\.$" completions)))
  (mapc 'kill-buffer buffers)))

(ert-deftest ido-vertical-can-be-turned-off ()
  (ido-vertical-mode 1)
  (ido-vertical-mode -1)
  (should (not (string-match "\n" (ido-completions "")))))
