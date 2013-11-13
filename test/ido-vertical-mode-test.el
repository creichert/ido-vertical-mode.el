(require 'ido)
(require 'ert)
(require 'ido-vertical-mode)

(ido-mode 1)
(ido-vertical-mode 1)
(kmacro-exec-ring-item (quote ([24 98 return] 0 "%d")) 1)

(ert-deftest ido-vertical-decorations-installed ()
  (ido-vertical-mode 1)
  (let ((prospects (ido-completions "")))
    (should (string-match "->" prospects))
    (should (string-match "\n" prospects))))

(ert-deftest ido-vertical-can-be-turned-off ()
  (ido-vertical-mode 1)
  (ido-vertical-mode -1)
  (should (not (string-match "\n" (ido-completions "")))))
