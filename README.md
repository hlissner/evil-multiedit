# evil-multiedit

This plugin tries to fill that multi-cursor shaped gap in your heart.

> Credit goes to [syl20bnr]() for his [evil-iedit-state]() plugin, which this plugin was
> heavily inspired by.

## Installation

`evil-multiedit` will be available on MELPA soon.

For now, download `evil-multiedit.el` somewhere in your `load-path`.

`(require 'evil-multiedit)`

## Usage

![evil-multiedit-match-all](/../screenshots/01.gif?raw=true)
![evil-multiedit-match-and-next](/../screenshots/02.gif?raw=true)

evil-multiedit *does not bind any new keys*, so as not to impose, so you will have to
yourself. Here is my recommended configuration:

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

Once you have regions highlighted, edit the regions however you like. `x`, `c`, `d`,
insert edits; they will all be mirrored across all multiedit regions.

Many evil-mode motions/operators will have special behavior while the cursor is in an edit
region:

* `D`: clear the region
* `C`: clear the region and go into insert mode
* `A`: go into insert mode at the end of region
* `I`: go into insert mode at the start of region
* `V`: select the whole edit region
* `gg`/`G`: go to the first/last region

To disable these, set `evil-multiedit-dwim-motion-keys` to `nil` before loading
evil-multiedit.

NOTE: No need to bind a key for `evil-multiedit-abort`, pressing <kbd>ESC</kbd> in normal
mode will invoke it.

### Functions

* `evil-multiedit-match-all`
* `evil-multiedit-match-and-next`
* `evil-multiedit-match-and-prev`
* `evil-multiedit-toggle-or-restrict-region`
* `evil-multiedit-next`
* `evil-multiedit-prev`
* `evil-multiedit-abort`

## Why not multiple-cursors or evil-mc?

It could be the [over] complexity of my emacs.d, but I've never managed to get
[evil-mc](https://github.com/gabesoft/evil-mc) to work for me, and `multiple-cursors`
[doesn't play nice with evil-mode](https://github.com/magnars/multiple-cursors.el/issues/17).

So I made this little hack that uses `iedit-mode` to mimic
[vim-multiedit](https://github.com/hlissner/vim-multiedit). It brings to Emacs a
multiple-cursors implementation that emulates much of what Sublime Text (or Atom) offers;
one that plays nice with evil-mode.

## Known issues

* `evil-multiedit` forcibly disables all the default `iedit` keybindings in
  `iedit-occurrence-keymap-default`. This only matters if you use iedit directly, however.
