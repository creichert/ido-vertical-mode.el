# ido-vertical-mode.el

[![Build Status](https://travis-ci.org/creichert/ido-vertical-mode.el.svg?branch=master)](https://travis-ci.org/creichert/ido-vertical-mode.el) Makes ido-mode display vertically.

![screenshot.gif](screenshot.gif)

This mode takes care of some caveats that are otherwise ugly to store
in your init file.

You may also be interested in
[`ido-ubiquitous`](https://github.com/DarwinAwardWinner/ido-ubiquitous)
and [`smex`](https://github.com/nonsequitur/smex).

Additionally, if you are interested in ido-mode displaying a grid of options instead of a vertical or horizontal list you can check [`ido-grid-mode`](https://github.com/larkery/ido-grid-mode.el).

## Install via [MELPA Stable](http://stable.melpa.org/#/) or [marmalade](http://marmalade-repo.org)

`M-x` `package-install` `ido-vertical-mode`

If you use MELPA instead of MELPA Stable, there's no guarantee that
you'll get something that works; I've accidentally broken master
before and will unfortunately probably do it again :(

## Turn it on

    (require 'ido-vertical-mode)
    (ido-mode 1)
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)

Or you can use `M-x ido-vertical-mode` to toggle it.

**N.B.** as of version 1.0.0, we do _not_ bind to <kbd>C-n</kbd> and
<kbd>C-p</kbd> for you! You must include that last `setq` line in your
config somewhere. This is out of a desire not to change the default
emacs keybindings, as <kbd>C-p</kbd> is already bound by default in
`ido-mode`. See the [keybindings section](#alternative-key-bindings)
for more information.

## Customize

#### Count

Show the count of candidates:

```elisp
(setq ido-vertical-show-count t)
```

#### Colors

Make it look like @abo-abo's [blog post](http://oremacs.com/2015/02/09/ido-vertical/):

```elisp
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background "#e5b7c0")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background "#e52b50"
                    :foreground "white")
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground "#b00000")
(ido-vertical-mode 1)
```

Make it look like the screenshot above:

```elisp
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground "orange")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground nil)
(ido-vertical-mode 1)
```

Reset the faces to their defaults:

```elisp
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :background nil
                    :foreground nil)
(ido-vertical-mode 1)

;; optionally
(setq ido-use-faces nil)
```

#### Alternative Key Bindings

Since the prospects are listed vertically, it might make sense to use
`C-n` and `C-p` to navigate through the options, instead of the
standard `C-s` and `C-r`.  To accomplish this, set
`ido-vertical-define-keys` like this:

    (setq ido-vertical-define-keys 'C-n-and-C-p-only)

The standard binding for `C-p` - `ido-toggle-prefix` - will now be
available on `C-c C-t`, which was previously unbound in `ido-mode`'s
key map. Of course, you can also put `ido-toggle-prefix` somewhere
else on your own:

    ;; manually mimic the 0.1.5 behavior of ido-vertical-mode
    (define-key ido-completion-map (kbd "M-p") 'ido-toggle-prefix)

You also have the option to rebind some or all of the arrow keys with
like this:

    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

to use up and down to navigate the options, or:

    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

to use left and right to move through the history/directories.
