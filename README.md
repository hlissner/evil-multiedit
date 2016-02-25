# evil-multiedit
[![MELPA](http://melpa.org/packages/evil-multiedit-badge.svg)](http://melpa.org/#/evil-multiedit)

This plugin tries to fill that multi-cursor shaped gap in your heart.

Credit goes to [syl20bnr]() for his [evil-iedit-state]() plugin, which this plugin was
heavily inspired by.

> Why not multiple-cursors or evil-mc?

It could be the [over] complexity of my [emacs.d](https://github.com/hlissner/emacs.d),
but I've never managed to get [evil-mc](https://github.com/gabesoft/evil-mc) to work for
me, and `multiple-cursors`
[doesn't play nice with evil-mode](https://github.com/magnars/multiple-cursors.el/issues/17).

So I made this little hack that uses `iedit-mode` to mimic
[vim-multiedit](https://github.com/hlissner/vim-multiedit). It brings to Emacs a
multiple selection implementation that emulates much of what Sublime Text (or Atom) offers;
one that plays nice with evil-mode.

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
```

Once regions are highlighted, edit them however you like. `x`, `c`, `d`, insert edits;
changes will be mirrored across all multiedit regions.

Many evil-mode motions/operators will have special behavior while the cursor is in an edit
region:

* `D`: clear the region
* `C`: clear to end-of-region and go into insert mode
* `A`: go into insert mode at end-of-region
* `I`: go into insert mode at start-of-region
* `V`: select the region
* `gg`/`G`: go to the first/last region

To disable these, set `evil-multiedit-dwim-motion-keys` to `nil` before loading
evil-multiedit.

NOTE: No need to bind a key for `evil-multiedit-abort`, pressing <kbd>ESC</kbd> in normal
mode will invoke it.

### Functions

* `evil-multiedit-restore`
* `evil-multiedit-match-all`
* `evil-multiedit-match-and-next`
* `evil-multiedit-match-and-prev`
* `evil-multiedit-toggle-or-restrict-region`
* `evil-multiedit-next`
* `evil-multiedit-prev`
* `evil-multiedit-abort`

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

