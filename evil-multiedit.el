;;; evil-multiedit.el --- multiple cursors for evil-mode
;;
;; Copyright (C) 2016-21 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <contact@henrik.io>
;; Created: February 20, 2016
;; Modified: November 21, 2021
;; Version: 1.4.3
;; Keywords: multiple cursors, editing, iedit
;; Homepage: https://github.com/hlissner/evil-multiedit
;; Package-Requires: ((emacs "25.1") (evil "1.14.0") (iedit "0.9.9") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This plugin was an answer to the lack of proper multiple cursor support in
;; Emacs+evil. It allows you to select and edit matches interactively,
;; integrating `iedit-mode' into evil-mode with an attempt at sensible defaults.
;;
;; Since then, [evil-mc] has matured, and now that I use both I've found they
;; can coexist, filling different niches, complimenting evil's built-in
;; column/line-wise editing operations.
;;
;;; Usage
;;
;; Evil-multiedit does not automatically bind any keys. Call
;; `evil-multiedit-default-keybinds' to bind my recommended configuration:
;;
;;  - S-r      (visual mode) Highlights all matches of the selection in the
;;             buffer.
;;  - M-d      In normal mode, this matches the word under the cursor.
;;               Consecutive presses will match the next match after point.
;;             In visual mode, the selection is used instead of the word at
;;               point.
;;             In insert mode, a blank marker (ala multiple cursors) is added at
;;               point.
;;  - M-S-d    Like M-d, but backwards instead of forward.
;;  - C-M-d    Restore the last iedit session.
;;  - RET      In normal mode, this toggles the iedit region at point.
;;             In visual mode, this disables all iedit regions *outside* the
;;               selection (i.e. restrict-to-region).
;;  - C-n/C-p  Navigate between iedit regions.
;;
;;  This package also defines an ':ie[dit] REGEXP' ex command, to create iedit
;;  regions for every match of REGEXP.
;;
;;; Code:

(require 'evil)
(require 'cl-lib)
;; HACK `iedit' is very noisy about binding keys at top-level; it emits a
;;      message in the echo area if it succeeds, and a warning if it fails.
;;      Both are meaningless to evil-multiedit users so I suppress them.
(let ((warning-minimum-log-level :error)
      (inhibit-message t))
  (require 'iedit))


;;
;;; Options

(defgroup evil-multiedit nil
  "Multiple cursors for `evil-mode' using iedit."
  :prefix "evil-multiedit-"
  :group 'evil)

(defcustom evil-multiedit-dwim-motion-keys t
  "If evil's motion keys should act differently in multiedit regions.
Must be set before evil-multiedit is loaded."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-match-whitespace t
  "If non-nil, allow matching against whitespace characters.
Whitespace is defined by the active major-mode's syntax table."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-match-punctuation t
  "If non-nil, allow matching against punctuation characters.
Punctuation is defined by the active major-mode's syntax table."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-ignore-indent-and-trailing t
  "If non-nil, trim whitespace from matches."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-use-symbols evil-symbol-word-search
  "Grab the symbol under cursor if evil-multiedit is invoked from
normal mode. If this is nil, words are used instead."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-smart-match-boundaries t
  "If non-nil, multiedit tries to make sure match boundaries match on successive
matches when invoked from normal mode. E.g. 'evil-multiedit-match' will not
match 'evil-multiedit-match-all', or 'i' will only match 'i' and not every
individual i in, say, 'ignition'.

If evil-multiedit is invoked from visual mode, this is ignored."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-store-in-search-history nil
  "If non-nil, highlighted occurrences are stored in `regexp-search-ring', so
that after exiting iedit `evil-search-next' and `evil-search-previous' (usually
n and N) use the last occurrence as if it were the last string in the search
history."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-scope 'buffer
  "How far evil-multiedit should look for additional matches. Accepts 'visible,
or anything that `bounds-of-thing-at-point' accept, such as 'defun, 'sexp or
'email. If 'buffer (the default), evil-multiedit will search the whole buffer."
  :group 'evil-multiedit
  :type 'symbol)

(defcustom evil-multiedit-follow-matches nil
  "If non-nil, cursor will jump to each new match on
`evil-multiedit-match-and-next' and `evil-multiedit-match-and-prev' (and its
symbol variants)."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-marker
  (propertize "|" 'face '(:inverse-video t))
  "The string to display in place of empty markers."
  :group 'evil-multiedit
  :type 'string)

(defvar evil-multiedit--pt-end nil "The end of the first match")
(defvar evil-multiedit--pt-beg nil "The beginning of the first region")
(defvar evil-multiedit--pt-index (cons 1 1) "The forward/backward search indices")

(defvar evil-multiedit--dont-recall nil)
(defvar-local evil-multiedit--last '() "Details about the last multiedit.")

(defvar evil-multiedit--marker nil)


;;
;;; Commands

;;;###autoload
(defun evil-multiedit-restore ()
  "Restore the last group of multiedit regions."
  (interactive)
  (unless evil-multiedit--last
    (user-error "No previous multiedit session to restore"))
  (cl-destructuring-bind (beg end occurrence) evil-multiedit--last
    (let ((iedit-occurrence-keymap-default nil)
          (iedit-mode-occurrence-keymap nil))
      (iedit-start occurrence beg end))
    (iedit-restrict-region beg end)
    (evil-multiedit-mode +1)))

;;;###autoload
(defun evil-multiedit-match-all ()
  "Highlight all matches of the current selection (or symbol under pointer) as
multiedit regions."
  (interactive)
  (if (fboundp 'ahs-clear) (ahs-clear))
  (setq evil-multiedit--dont-recall t)
  (evil-multiedit--start-regexp (car (evil-multiedit--get-occurrence))
                                (point-min) (point-max))
  (when (evil-visual-state-p)
    (evil-normal-state)))

;;;###autoload (autoload 'evil-multiedit-match-symbol-and-next "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-symbol-and-next (&optional count)
  "Same as `evil-multiedit-match-and-next' if invoked from visual mode. From
normal mode, it grabs whole symbols rather than words."
  :jump t
  (interactive "<c>")
  (let ((evil-multiedit-use-symbols t))
    (evil-multiedit-match-and-next count)))

;;;###autoload (autoload 'evil-multiedit-match-symbol-and-prev "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-symbol-and-prev (&optional count)
  "Same as `evil-multiedit-match-and-prev' if invoked from visual mode. From
normal mode, it grabs whole symbols rather than words."
  :jump t
  (interactive "<c>")
  (evil-multiedit-match-symbol-and-next (or (and count (* -1 count)) -1)))

;;;###autoload
(defun evil-multiedit-toggle-marker-here ()
  "Toggle an arbitrary multiedit region at point."
  (interactive)
  (when (and iedit-occurrences-overlays
             (> (iedit-occurrence-string-length) 0))
    (evil-multiedit--cleanup))
  (let ((points (cond ((eq (evil-visual-type) 'block)
                       (mapcar #'overlay-start evil-visual-block-overlays))
                      (t (list (point))))))
    (dolist (point points)
      (let ((ov (iedit-find-overlay-in-region point point 'iedit-occurrence-overlay-name)))
        (if ov
            (progn
              (setq iedit-occurrences-overlays (delete ov iedit-occurrences-overlays))
              (delete-overlay ov))
          (let ((ov (iedit-make-occurrence-overlay point point)))
            (unless ov
              (error "Failed to create marker"))
            (push ov iedit-occurrences-overlays)
            (dolist (prop '(insert-in-front-hooks insert-behind-hooks modification-hooks))
              (let ((old (overlay-get ov prop)))
                (overlay-put ov prop (append old '(evil-multiedit--update-occurrences)))))
            (setq evil-multiedit--marker t)
            (overlay-put ov 'before-string evil-multiedit-marker))))))
  (if iedit-occurrences-overlays
      (evil-multiedit-mode +1)
    (evil-multiedit--cleanup)))

;;;###autoload (autoload 'evil-multiedit-operator "evil-multiedit" nil t)
(evil-define-operator evil-multiedit-operator (beg end type &optional count)
  "Select an iedit region using evil text-objects."
  (interactive "<R><c>")
  (evil-visual-select beg end type)
  (evil-multiedit-match-and-next count))

;;;###autoload (autoload 'evil-multiedit-match-and-next "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-and-next (&optional count)
  "Marks the word at point (or, if in visual mode, the selection), then marks
the next matches on consecutive runs of this function. Jumps to next region if
`evil-multiedit-follow-matches' is non-nil.

Note: the matching behavior differs depending on if it was invoked from normal
or visual mode.

  + From normal mode: `evil-multiedit-use-symbols' determines how to grab the
    match under the cursor.
  + From visual mode, word and symbol boundaries are ignored, allowing for
    in-word matches."
  (interactive "<c>")
  (let ((forward-p (or (null count) (> count 0)))
        (count (or (and count (abs count)) 1)))
    (dotimes (i count)
      (cond (iedit-mode
             (let ((bounds (evil-multiedit--scope))
                   (origin (point))
                   (start (if forward-p evil-multiedit--pt-end evil-multiedit--pt-beg))
                   (whitespace-p (string-match-p "\\`\\s-+\\'" iedit-initial-string-local))
                   beg end)
               (goto-char (+ start (if forward-p 1 -1)))
               (unless (re-search-forward iedit-initial-string-local
                                          (if forward-p (cdr bounds) (car bounds))
                                          t (if forward-p 1 -1))
                 (goto-char origin)
                 (user-error "No more matches"))
               ;; Skip leading and trailing whitespace, if possible
               (while (and whitespace-p evil-multiedit-ignore-indent-and-trailing
                           (or (<= (point) (save-excursion (back-to-indentation) (point)))
                               (>  (point) (save-match-data (save-excursion (evil-last-non-blank) (point)))))
                           (re-search-forward iedit-initial-string-local
                                              (if forward-p (cdr bounds) (car bounds))
                                              t (if forward-p 1 -1))))
               (setq beg (match-beginning 0)
                     end (match-end 0))
               ;; Check for an overlay, if none exist, create one
               (unless (or (iedit-find-overlay-at-point beg 'iedit-occurrence-overlay-name)
                           (iedit-find-overlay-at-point end 'iedit-occurrence-overlay-name))
                 (push (iedit-make-occurrence-overlay beg end)
                       iedit-occurrences-overlays))
               ;; Remember for next time
               (setq evil-multiedit--pt-beg (min beg evil-multiedit--pt-beg)
                     evil-multiedit--pt-end (max end evil-multiedit--pt-end))
               (goto-char (if evil-multiedit-follow-matches beg origin))))
            ((cl-destructuring-bind (occurrence beg end)
                 (evil-multiedit--get-occurrence)
               (unless (and beg end)
                 (user-error "Can't mark anything"))
               (setq evil-multiedit--pt-beg beg
                     evil-multiedit--pt-end end)
               (save-excursion
                 (evil-multiedit--start-regexp occurrence beg end)
                 (let* (evil-ex-search-vim-style-regexp
                        (pattern (evil-ex-make-search-pattern occurrence)))
                   (when evil-multiedit-store-in-search-history
                     (setq evil-ex-search-pattern pattern))
                   (evil-ex-find-next pattern nil t))))))))
  (let ((occurrences (length iedit-occurrences-overlays)))
    (when (and (> occurrences 0)
               (evil-visual-state-p))
      (evil-normal-state))
    occurrences))

;;;###autoload (autoload 'evil-multiedit-match-and-prev "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-and-prev (&optional count)
  "The backwards version of `evil-multiedit-match-and-next'"
  :jump t
  (interactive "<c>")
  (evil-multiedit-match-and-next (or (and count (* -1 count)) -1)))

;;;###autoload
(defun evil-multiedit-toggle-or-restrict-region (&optional beg end)
  "If in visual mode, restrict the multiedit regions to the selected region.
i.e. disable all regions outside the selection. If in any other mode, toggle the
multiedit region beneath the cursor, if one exists."
  (interactive)
  (if (and iedit-mode (iedit-current-occurrence-string))
      (cond ((or (evil-visual-state-p)
                 (and beg end))
             (let ((current-prefix-arg '(4))
                   (beg (or beg (region-beginning)))
                   (end (or end (region-end))))
               (iedit-done)
               (call-interactively #'iedit-mode)
               (save-excursion (iedit-restrict-region beg end))
               (evil-previous-line)))
            (t (iedit-toggle-selection)))
    (call-interactively #'evil-ret)))

;;;###autoload
(defun evil-multiedit-next ()
  "Jump to the next multiedit region."
  (interactive)
  (evil-multiedit--cycle +1))

;;;###autoload
(defun evil-multiedit-prev ()
  "Jump to the previous multiedit region."
  (interactive)
  (evil-multiedit--cycle -1))

;;;###autoload
(defun evil-multiedit-abort ()
  "Clear all multiedit regions, clean up and revert to normal state."
  (interactive)
  (when evil-multiedit-mode
    (setq evil-multiedit--last nil)
    (when (and iedit-occurrences-overlays (not evil-multiedit--dont-recall))
      (setq iedit-occurrences-overlays
            (cl-sort iedit-occurrences-overlays #'< :key #'overlay-start))
      (let ((beg (overlay-start (car iedit-occurrences-overlays)))
            (end (overlay-end (car (last iedit-occurrences-overlays)))))
        (setq-local evil-multiedit--last
                    (list (save-excursion (goto-char beg) (line-beginning-position))
                          (save-excursion (goto-char end) (line-end-position))
                          iedit-initial-string-local))))
    (iedit-done)
    (evil-multiedit-mode -1)))

;;;###autoload (autoload 'evil-multiedit-ex-match "evil-multiedit" nil t)
(evil-define-command evil-multiedit-ex-match (&optional beg end bang regexp)
  "Ex command for invoking evil-multiedit with a regular expression. The
selected area is the boundary for matches. If BANG, invert
`evil-multiedit-smart-match-boundaries'."
  (interactive "<r><!><a>")
  (evil-multiedit-abort)
  (let ((evil-multiedit-smart-match-boundaries
         (if bang
             (not evil-multiedit-smart-match-boundaries)
           evil-multiedit-smart-match-boundaries)))
    (if regexp
        (evil-multiedit--start-regexp regexp beg end)
      (evil-multiedit-restore)
      (when (and beg end)
        (evil-multiedit-toggle-or-restrict-region beg end)))))


;;
;;; Helpers

(defun evil-multiedit--cycle (n)
  (setq iedit-occurrences-overlays
        (cl-sort iedit-occurrences-overlays (if (> n 0) #'< #'>)
                 :key #'overlay-start))
  (let* ((point (point))
         (occurrence
          (cl-find-if (if (> n 0)
                          (lambda (beg) (> beg point))
                        (lambda (end) (< end point)))
                      iedit-occurrences-overlays
                      :key (if (> n 0) #'overlay-start #'overlay-end))))
    (if occurrence
        (goto-char (overlay-start occurrence))
      (user-error "No more occurrences"))))

(defun evil-multiedit--update-occurrences (&rest _)
  (when (if evil-multiedit--marker
            (> (iedit-occurrence-string-length) 0)
          (= (iedit-occurrence-string-length) 0))
    (let ((ov-str (unless evil-multiedit--marker evil-multiedit-marker)))
      (dolist (ov iedit-occurrences-overlays)
        (overlay-put ov 'before-string ov-str))
      (setq evil-multiedit--marker (not evil-multiedit--marker)))))

(defun evil-multiedit--scope ()
  (if (eq evil-multiedit-scope 'visible)
      (cons (window-start) (window-end))
    (or (bounds-of-thing-at-point evil-multiedit-scope)
        (and (null evil-multiedit-scope)
             (bounds-of-thing-at-point 'buffer))
        (error "Invalid/empty scope (%s), check `evil-multiedit-scope'"
               evil-multiedit-scope))))

(defun evil-multiedit--get-occurrence ()
  (cl-destructuring-bind (regexp . bounds)
      (save-match-data
        (cond ((evil-visual-state-p)
               (cons
                (regexp-quote (buffer-substring-no-properties
                               evil-visual-beginning evil-visual-end))
                (cons evil-visual-beginning evil-visual-end)))
              ((or (if evil-multiedit-match-whitespace  (looking-at "\\s-+"))
                   (if evil-multiedit-match-punctuation (looking-at "\\s.+")))
               (cons
                (regexp-quote (match-string-no-properties 0))
                (cons (match-beginning 0) (match-end 0))))
              ((and evil-multiedit-use-symbols
                    (bounds-of-thing-at-point 'symbol))
               (cons
                (format "\\_<%s\\_>" (regexp-quote (thing-at-point 'symbol t)))
                (bounds-of-thing-at-point 'symbol)))
              ((bounds-of-thing-at-point 'word)
               (cons
                (format "\\<%s\\>" (regexp-quote (thing-at-point 'word t)))
                (bounds-of-thing-at-point 'word)))
              ((cons nil nil))))
    (list regexp (car bounds) (cdr bounds))))

(defun evil-multiedit--start (obeg oend &optional beg end)
  (let* ((occurrence (buffer-substring-no-properties obeg oend))
         (sym-p (string-match-p "^[^a-zA-Z0-9]$" occurrence)))
    (when occurrence
      (setq occurrence (regexp-quote occurrence))
      (when (and evil-multiedit-smart-match-boundaries
                 (not sym-p))
        (when (and (goto-char (1- obeg))
                   (looking-at "[^a-zA-Z0-9]"))
          (setq occurrence (concat "\\_<" occurrence)))
        (when (and (goto-char oend)
                   (looking-at "[^a-zA-Z0-9]"))
          (setq occurrence (concat occurrence "\\_>"))))
      (evil-multiedit--start-regexp occurrence (or beg obeg) (or end oend)))))

(defun evil-multiedit--start-regexp (regexp &optional beg end)
  (setq iedit-initial-string-local regexp)
  (when evil-multiedit-store-in-search-history
    (isearch-update-ring regexp t))
  (let ((inhibit-message t)
        (iedit-occurrence-keymap-default nil)
        (iedit-mode-occurrence-keymap nil))
    (iedit-start regexp (or beg (point-min)) (or end (point-max))))
  (evil-multiedit-mode +1)
  regexp)

(defun evil-multiedit--cleanup ()
  (setq evil-multiedit--dont-recall nil
        evil-multiedit--pt-end nil
        evil-multiedit--pt-beg nil
        evil-multiedit--pt-index (cons 1 1)
        iedit-occurrences-overlays nil))

(defmacro evil-multiedit--defun-insert-subst (command docstring &rest body)
  "Call COMMAND and switch to iedit-insert state. If INTERACTIVE is non-nil then
COMMAND is called interactively."
  (declare (indent defun) (doc-string 2))
  `(defun ,command ()
     ,docstring
     (interactive)
     (evil-insert-state)
     (let ((fn (progn ,@body)))
       (when (functionp fn)
         (if (commandp fn)
             (call-interactively fn)
           (funcall fn))))
     ;; required to correctly update the cursors
     (evil-multiedit-mode +1)))


;;
;;; Evil substitutes

(evil-define-motion evil-multiedit-beginning-of-line ()
  "Go to the beginning of the current overlay or line."
  :type exclusive
  (let ((overlay (iedit-find-current-occurrence-overlay)))
    (if overlay
        (goto-char (overlay-start overlay))
      (call-interactively
       (if evil-respect-visual-line-mode
           #'evil-beginning-of-visual-line
         #'evil-beginning-of-line)))))

(evil-define-motion evil-multiedit-first-non-blank ()
  "Go to the beginning of the current overlay or text."
  :type exclusive
  (let ((overlay (iedit-find-current-occurrence-overlay)))
    (if overlay
        (goto-char (overlay-start overlay))
      (call-interactively
       (if evil-respect-visual-line-mode
           #'evil-first-non-blank-of-visual-line
         #'evil-first-non-blank)))))

(evil-define-motion evil-multiedit-end-of-line ()
  "Go to the beginning of the current overlay or line."
  :type exclusive
  (let ((overlay (iedit-find-current-occurrence-overlay)))
    (if overlay
        (goto-char (+ (if (evil-insert-state-p) 0 -1)
                      (overlay-end overlay)))
      (call-interactively #'evil-end-of-line-or-visual-line))))

(evil-multiedit--defun-insert-subst evil-multiedit--append-line
  "Put the point at then end of current overlay and switch to iedit-insert
state."
  (if (iedit-find-current-occurrence-overlay)
      #'evil-multiedit-end-of-line
    #'evil-append-line))

(evil-multiedit--defun-insert-subst evil-multiedit--change
  "Wipe all the occurrences and switch in `iedit-insert state'"
  #'evil-change)
;; https://github.com/noctuid/lispyville/pull/26
;; https://github.com/emacs-evil/evil/issues/916
(when (boundp 'evil-change-commands)
  (add-to-list 'evil-change-commands #'evil-multiedit--change))

(evil-multiedit--defun-insert-subst evil-multiedit--insert-line
  "Place point at beginning of overlay in insert mode."
  #'evil-multiedit-beginning-of-line)

(evil-multiedit--defun-insert-subst evil-multiedit--append
  "Append and switch to `iedit-insert state'"
  #'evil-append)

(evil-multiedit--defun-insert-subst evil-multiedit--open-below
  "Insert new line below and switch to `iedit-insert state'"
  #'evil-open-below)

(evil-multiedit--defun-insert-subst evil-multiedit--open-above
  "Insert new line above and switch to `iedit-insert state'"
  #'evil-open-above)

(defun evil-multiedit--visual-line ()
  "Visually select edit region."
  (interactive)
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (if ov
        (evil-visual-make-region (overlay-start ov) (1- (overlay-end ov)) 'exclusive)
      (call-interactively #'evil-visual-line))))

(defun evil-multiedit--change-line ()
  "Wipe all the occurrences and switch in `iedit-insert state'"
  (interactive)
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (if ov
        (evil-change (overlay-start ov) (overlay-end ov))
      (call-interactively #'evil-change-line))))

(defun evil-multiedit--paste ()
  "Paste after cursor without clobbering the undo history."
  (interactive)
  (evil-with-single-undo
    (call-interactively #'evil-paste-after)))

(defun evil-multiedit--paste-replace ()
  "Replace the iedit region with the clipboard."
  (interactive)
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (if (not ov)
        (call-interactively #'evil-paste-before)
      (goto-char (overlay-start ov))
      (delete-region (overlay-start ov)
                     (overlay-end ov))
      (insert (current-kill 0))
      (iedit-update-occurrences))))

(defun evil-multiedit--delete-occurrences (&optional dont-kill)
  "Delete occurrences. If DONT-KILL, don't add occurence to kill ring."
  (interactive "*")
  (iedit-barf-if-buffering)
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (when ov
      (save-excursion
        (if (not dont-kill)
            (kill-new
             (buffer-substring-no-properties
              (overlay-start ov)
              (overlay-end ov))))
        (delete-region (overlay-start ov) (overlay-end ov))))))


;;
;;; Minor mode

(defvar evil-multiedit-mode-map
  (let ((map (make-sparse-keymap)))
    (when evil-multiedit-dwim-motion-keys
      (evil-define-key* '(normal insert) map
        (kbd "RET") #'evil-multiedit-toggle-or-restrict-region
        (kbd "C-n") #'evil-multiedit-next
        (kbd "C-p") #'evil-multiedit-prev)
      (evil-define-key* 'visual map
        (kbd "RET") #'evil-multiedit-toggle-or-restrict-region)
      (evil-define-key* 'normal map
        "0"         #'evil-multiedit-beginning-of-line
        "^"         #'evil-multiedit-first-non-blank
        "$"         #'evil-multiedit-end-of-line
        "a"         #'evil-multiedit--append
        "A"         #'evil-multiedit--append-line
        "c"         #'evil-multiedit--change
        "C"         #'evil-multiedit--change-line
        "D"         #'evil-multiedit--delete-occurrences
        "gg"        #'iedit-goto-first-occurrence
        "G"         #'iedit-goto-last-occurrence
        "I"         #'evil-multiedit--insert-line
        "o"         #'evil-multiedit--open-below
        "O"         #'evil-multiedit--open-above
        "p"         #'evil-multiedit--paste
        "P"         #'evil-multiedit--paste-replace
        (kbd "C-g") #'evil-multiedit-abort
        "V"         #'evil-multiedit--visual-line
        "za"        #'iedit-toggle-unmatched-lines-visible))
    map)
  "Keymap used for `evil-multiedit-mode'.")

(define-minor-mode evil-multiedit-mode
  "A minor mode that indicates an active evil-multiedit session."
  :init-value nil
  :after-hook (evil-normalize-keymaps)
  (cond (evil-multiedit-mode
         (setq-local iedit-auto-save-occurrence-in-kill-ring nil)
         (add-hook 'iedit-mode-end-hook #'evil-multiedit--cleanup nil 'local)
         (advice-add 'evil-force-normal-state :before #'evil-multiedit-abort))
        (t
         (kill-local-variable 'iedit-auto-save-occurrence-in-kill-ring)
         (remove-hook 'iedit-mode-end-hook #'evil-multiedit--cleanup 'local))))

(defun evil-multiedit-default-keybinds ()
  "Sets up the default keybindings for `evil-multiedit'."
  (evil-define-key* 'visual 'global
    "R"           #'evil-multiedit-match-all
    (kbd "M-d")   #'evil-multiedit-match-and-next
    (kbd "M-D")   #'evil-multiedit-match-and-prev
    (kbd "C-M-d") #'evil-multiedit-restore)
  (evil-define-key* '(normal insert) 'global
    (kbd "M-d")   #'evil-multiedit-match-symbol-and-next
    (kbd "M-D")   #'evil-multiedit-match-symbol-and-prev)
  (evil-define-key* 'insert 'global
    (kbd "C-M-d") #'evil-multiedit-toggle-marker-here)
  (evil-ex-define-cmd "ie[dit]" #'evil-multiedit-ex-match))


;;
;;; Integrations

;; `expand-region' integration
(defun evil-multiedit--iedit-mode-from-expand-region (&optional arg)
  "Start `iedit-mode'."
  (interactive "P")
  (evil-multiedit-mode arg)
  ;; force expand-region temporary overlay map exit
  (setq overriding-terminal-local-map nil))

(define-advice er/prepare-for-more-expansions-internal (:filter-return (ret) add-multiedit-prompt)
  "Add evil-multiedit option to expand-region's minibuffer help prompt."
  (if (not evil-multiedit-mode) ret
    (cl-destructuring-bind (default-msg . default-bindings) ret
      (cons (concat default-msg ", e to edit")
            (add-to-list 'default-bindings
                         '("e" evil-multiedit--iedit-mode-from-expand-region))))))

(provide 'evil-multiedit)
;;; evil-multiedit.el ends here
