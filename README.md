# evil-multiedit
[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/evil-multiedit-badge.svg)](http://melpa.org/#/evil-multiedit)
[![MELPA Stable](http://stable.melpa.org/packages/evil-multiedit-badge.svg)](http://stable.melpa.org/#/evil-multiedit)
[![Build Status](https://travis-ci.org/hlissner/evil-multiedit.png?branch=master)](https://travis-ci.org/hlissner/evil-multiedit)

This plugin tries to fill that multi-cursor shaped hole in your heart.

> Credit goes to [syl20bnr] for his [evil-iedit-state] plugin, which this
  plugin was heavily inspired by.

## Why not multiple-cursors or evil-mc?

It could be the [over] complexity of my [emacs.d], but I've never managed to
get [evil-mc] to work for me, and
`multiple-cursors` [doesn't play nice with
evil-mode](https://github.com/magnars/multiple-cursors.el/issues/17).

So I hacked this plugin together to integrate `iedit-mode` into evil-mode. It takes
after [vim-multiedit], offers an approach to multiple cursors like Sublime Text (or
Atom) have, and compliments evil's in-built column and line-wise editing operations.

![evil-multiedit](/../screenshots/main.gif?raw=true)

## Installation

The package is available on MELPA.

`M-x package-install RET evil-multiedit`

Then load it up:

`(require 'evil-multiedit)`

## Usage

Evil-multiedit *does not bind any new keys*, so you will have to yourself. Here is my
recommended configuration:

```elisp
;; Highlights all matches of the selection in the buffer.
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
;; incrementally add the next unmatched match.
(define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;; Match selected region.
(define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)

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
(define-key evil-visual-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

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

NOTE: No need to bind a key for `evil-multiedit-abort`, pressing <kbd>ESC</kbd> in normal
mode will invoke it.

### Ex Command

Use `(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)` so you can use
`:iedit <REGEX>` to highlight matches with a regular expression.

### Commands

* `evil-multiedit-restore`
* `evil-multiedit-match-all`
* `evil-multiedit-match-and-next`
* `evil-multiedit-match-and-prev`
* `evil-multiedit-match-symbol-and-next`
* `evil-multiedit-match-symbol-and-prev`
* `evil-multiedit-toggle-or-restrict-region`
* `evil-multiedit-next`
* `evil-multiedit-prev`
* `evil-multiedit-abort`
* `evil-multiedit-ex-match`

### Options

* `evil-multiedit-dwim-motion-keys` (default: `t`): Whether or not to modify evil's motion
  keys to act differently when the cursor is inside multiedit regions. Must be set before
  evil-multiedit is loaded.
* `evil-multiedit-ignore-indent-and-trailing` (default: `t`): When you match forward
  whitespace and this is non-nil, leading and trailing whitespace will be ignored.
* `evil-multiedit-thing-at-point-fn` (default `(lambda () (bounds-of-thing-at-point
  'word))`): This function dictates what to grab from under the cursor if evil-multiedit
  is invoked from normal mode. It takes no parameters and returns a cons cell (beg . end)
  containing the bounds of the region to mark.
* `evil-multiedit-smart-match-boundaries` (default `t`): If non-nil, multiedit will try to
  be smart about match boundaries when invoked from normal mode. E.g.
  + 'evil-multiedit-match' will not match 'evil-multiedit-match-all'
  + 'i' will only match 'i' and not every individual i in 'ignition'.

  **NOTE:** If evil-multiedit is invoked from visual mode, this is ignored.


[evil-mode]: https://bitbucket.org/lyro/evil/wiki/Home
[vim-multiedit]: https://github.com/hlissner/vim-multiedit
[syl20bnr]: https://github.com/syl20bnr
[evil-iedit-state]: https://github.com/syl20bnr/evil-iedit-state
[emacs.d]: https://github.com/hlissner/.emacs.d
[evil-mc]: https://github.com/gabesoft/evil-mc
