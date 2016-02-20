# evil-multiedit

This plugin tries to fill that multi-cursor shaped gap in your heart.

> Why not multiple-cursors or evil-mc?

It could be the [over] complexity of my emacs.d, but I've never managed to get
[evil-mc](https://github.com/gabesoft/evil-mc) to work for me, and `multiple-cursors`
[doesn't play nice with evil-mode](https://github.com/magnars/multiple-cursors.el/issues/17).

So I settled for this hack that uses `iedit-mode` to mimics
[vim-multiedit](https://github.com/hlissner/vim-multiedit) and bring a multiple-cursors
implementation that emulates much of what Sublime Text (or Atom) offers to Emacs.

And it plays nice with evil-mode.

## Installation

`evil-multiedit` will be available on MELPA soon.

For now, download `evil-multiedit.el` somewhere in your `load-path`.

```elisp
(require 'evil-multiedit)
```

## Usage

(Screenshots coming soon)

<!-- ![evil-multiedit-match-all](/../screenshots/match-all.png?raw=true) -->
<!-- ![evil-multiedit-match-and-next/prev](/../screenshots/matching.png?raw=true) -->
<!-- ![evil-multiedit-restrict-to-region](/../screenshots/match-all.png?raw=true) -->

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

;; When RET is pressed while in visual mode, all edit fields outside the selected region
;; will be disabled.
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

## Functions

* `evil-multiedit-match-all`
* `evil-multiedit-match-and-next`
* `evil-multiedit-match-and-prev`
* `evil-multiedit-toggle-or-restrict-region`
* `evil-multiedit-next`
* `evil-multiedit-prev`
* `evil-multiedit-abort`

