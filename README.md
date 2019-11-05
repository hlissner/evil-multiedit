![evil-multiedit](https://img.shields.io/badge/evil--multiedit-v1.3.8-blue.svg)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/evil-multiedit-badge.svg)](http://melpa.org/#/evil-multiedit)
[![MELPA Stable](http://stable.melpa.org/packages/evil-multiedit-badge.svg)](http://stable.melpa.org/#/evil-multiedit)
[![Build Status](https://travis-ci.org/hlissner/evil-multiedit.png?branch=master)](https://travis-ci.org/hlissner/evil-multiedit)

# evil-multiedit

This plugin was an answer to the lack of proper multiple cursor support in
Emacs+evil. It allows you to select and edit matches interactively, integrating
`iedit-mode` into evil-mode with an attempt at sensible defaults.

Since then, [evil-mc] has matured, and now that I use both I've found they can
coexist, filling different niches, complimenting evil's built-in
column/line-wise editing operations.

![evil-multiedit](../screenshots/main.gif?raw=true)

> Thanks to [syl20bnr] for his [evil-iedit-state] plugin, which this plugin was
> heavily inspired by.

## Installation

The package is available on MELPA.

`M-x package-install RET evil-multiedit`

Then load it up:

`(require 'evil-multiedit)`

## Usage

Evil-multiedit does not automatically bind any keys. Call
`(evil-multiedit-default-keybinds)` to bind my recommended configuration:

```elisp
;; Highlights all matches of the selection in the buffer.
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
;; incrementally add the next unmatched match.
(define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;; Match selected region.
(define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;; Insert marker at point
(define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

;; Same as M-d but in reverse.
(define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
(define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)

;; OPTIONAL: If you prefer to grab symbols rather than words, use
;; `evil-multiedit-match-symbol-and-next` (or prev).

;; Restore the last group of multiedit regions.
(define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

;; RET will toggle the region under the cursor
(define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; ...and in visual mode, RET will disable all fields outside the selected region
(define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; For moving between edit regions
(define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
(define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
```

Once regions are highlighted, changes are mirrored across them all.

Many evil-mode motions/operators will have slightly different behavior while
evil-multiedit is active or the cursor is in an iedit region:

* `D`: clear the region
* `C`: clear to end-of-region and go into insert mode
* `A`: go into insert mode at end-of-region
* `I`: go into insert mode at start-of-region
* `V`: select the region
* `$`: go to end-of-region
* `0`/`^`: go to start-of-region
* `gg`/`G`: go to the first/last region

To disable these, set `evil-multiedit-dwim-motion-keys` to `nil` before loading
evil-multiedit.

NOTE: No need to bind a key for `evil-multiedit-abort`, pressing <kbd>ESC</kbd>
in normal mode will invoke it.

### Ex Command

`:iedit [REGEXP]` is available for invoking multiedit from the ex command line.

### Commands

* `evil-multiedit-restore`
* `evil-multiedit-match-all`
* `evil-multiedit-match-and-next`
* `evil-multiedit-match-and-prev`
* `evil-multiedit-match-symbol-and-next`
* `evil-multiedit-match-symbol-and-prev`
* `evil-multiedit-toggle-marker-here`
* `evil-multiedit-toggle-or-restrict-region`
* `evil-multiedit-next`
* `evil-multiedit-prev`
* `evil-multiedit-abort`
* `evil-multiedit-ex-match`

### Options

* `evil-multiedit-dwim-motion-keys` (default: `t`): If non-nil, evil's motion
  keys behave differently when the point is inside a multiedit region. Must be
  set before evil-multiedit is loaded.
* `evil-multiedit-ignore-indent-and-trailing` (default: `t`): If non-nil, skip
  over indentation and trailing whitespace when matching whitespace.
* `evil-multiedit-scope` (default `nil`): How far evil-multiedit should look for
  incremental matches (doesn't affect `evil-multiedit-match-all` or
  `evil-multiedit-ex-match`). Accepts anything that `bounds-of-thing-at-point`
  accepts, such as `'defun`, `'sexp`, `'email` or the default, `'buffer`.
* `evil-multiedit-smart-match-boundaries` (default `t`): If non-nil, multiedit
  will try to be smart about match boundaries when invoked from normal mode.
  E.g.
  + 'evil-multiedit-match' will not match 'evil-multiedit-match-all'
  + 'i' will only match 'i' and not every individual i in 'ignition'.
  * **NOTE:** If evil-multiedit is invoked from visual mode, this is ignored.
* `evil-multiedit-store-in-search-history` (default `nil`): If non-nil,
  highlighted occurrences are stored in `regexp-search-ring`, so that after
  exiting iedit `evil-search-next` and `evil-search-previous` (usually n and N)
  use the last occurrence as if it were the last string in the search history.
* `evil-multiedit-follow-matches` (default `nil`): If non-nil, the cursor will
  jump to each additional match, rather than remain in its original position.

### Co-existing with evil-mc

How the two plugins mingle is entirely personal preference. Mine is to bind
evil-multiedit to <kbd>M-d</kbd>/<kbd>M-D</kbd>, and evil-mc to a bunch of keys
prefixed with <kbd>gz</kbd>:

```emacs-lisp
;; evil-multiedit
:v  "R"     #'evil-multiedit-match-all
:n  "M-d"   #'evil-multiedit-match-symbol-and-next
:n  "M-D"   #'evil-multiedit-match-symbol-and-prev
:v  "M-d"   #'evil-multiedit-match-and-next
:v  "M-D"   #'evil-multiedit-match-and-prev
:nv "C-M-d" #'evil-multiedit-restore
(:after evil-multiedit
  (:map evil-multiedit-state-map
    "M-d" #'evil-multiedit-match-and-next
    "M-D" #'evil-multiedit-match-and-prev
    "RET" #'evil-multiedit-toggle-or-restrict-region)
  (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
    "C-n" #'evil-multiedit-next
    "C-p" #'evil-multiedit-prev))

;; evil-mc
(:prefix "gz"
  :nv "m" #'evil-mc-make-all-cursors
  :nv "u" #'evil-mc-undo-all-cursors
  :nv "z" #'+evil/mc-toggle-cursors
  :nv "c" #'+evil/mc-make-cursor-here
  :nv "n" #'evil-mc-make-and-goto-next-cursor
  :nv "p" #'evil-mc-make-and-goto-prev-cursor
  :nv "N" #'evil-mc-make-and-goto-last-cursor
  :nv "P" #'evil-mc-make-and-goto-first-cursor)
(:after evil-mc
  :map evil-mc-key-map
  :nv "C-n" #'evil-mc-make-and-goto-next-cursor
  :nv "C-N" #'evil-mc-make-and-goto-last-cursor
  :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
  :nv "C-P" #'evil-mc-make-and-goto-first-cursor)
```

You can find [my evil-mc/evil-multiedit config in my emacs.d](https://github.com/hlissner/.emacs.d/blob/master/modules/feature/evil/config.el#L285).


[emacs.d]: https://github.com/hlissner/.emacs.d
[evil-iedit-state]: https://github.com/syl20bnr/evil-iedit-state
[evil-mc]: https://github.com/gabesoft/evil-mc
[evil-mode]: https://bitbucket.org/lyro/evil/wiki/Home
[syl20bnr]: https://github.com/syl20bnr
[vim-multiedit]: https://github.com/hlissner/vim-multiedit
