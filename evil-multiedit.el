;;; evil-multiedit.el --- multiple cursors for evil-mode
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: February 20, 2016
;; Modified: April 12, 2016
;; Version: 1.2.6
;; Keywords: multiple cursors, editing, iedit
;; Homepage: https://github.com/hlissner/evil-multiedit
;; Package-Requires: ((emacs "24.4") (evil "1.0.8") (iedit "0.97") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This plugin tries to fill that multi-cursor shaped gap in your heart, by integrating
;; iedit-mode into evil-mode.
;;
;; Usage
;;
;;   Evil-multiedit *does not bind any new keys*, so you will have to yourself. Here is my
;;   recommended configuration:
;;
;;     ;; Highlights all matches of the selection in the buffer.
;;     (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
;;
;;     ;; Match the word under cursor (i.e. make it an edit region). Consecutive
;;     ;; presses will incrementally add the next unmatched match.
;;     (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;;     ;; Match selected region.
;;     (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;;
;;     ;; Same as M-d but in reverse.
;;     (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
;;     (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
;;
;;     ;; OPTIONAL: If you prefer to grab symbols rather than words, use
;;     ;; `evil-multiedit-match-symbol-and-next` (or prev).
;;
;;     ;; Restore the last group of multiedit regions.
;;     (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)
;;
;;     ;; RET will toggle the region under the cursor
;;     (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
;;
;;     ;; ...and in visual mode, RET will disable all fields outside the selected
;;     ;; region
;;     (define-key evil-visual-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
;;
;;     ;; For moving between edit regions
;;     (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
;;     (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
;;     (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
;;     (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)
;;
;;     ;; Allows you to invoke evil-multiedit with a regular expression
;;     (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
;;
;;; Code:

(defvar iedit-occurrence-keymap-default (make-sparse-keymap))

(require 'evil)
(require 'iedit)
(require 'cl-lib)

(defgroup evil-multiedit nil
  ""
  :prefix "evil-multiedit-"
  :group 'evil)

(defcustom evil-multiedit-dwim-motion-keys t
  "Whether or not to modify evil's motion keys to act differently when the cursor is
  inside multiedit regions. Must be set before evil-multiedit is loaded."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-ignore-indent-and-trailing t
  "When you match forward whitespace and this is non-nil, leading and trailing whitespace
will be ignored."
  :group 'evil-multiedit
  :type 'boolean)

(defcustom evil-multiedit-thing-at-point-fn
  (lambda () (bounds-of-thing-at-point 'word))
  "This function dictates what to grab from under the cursor if evil-multiedit is invoked
from normal mode. It takes no parameters and returns a cons cell (beg . end) containing
the bounds of the region to mark."
  :group 'evil-multiedit
  :type 'function)

(defcustom evil-multiedit-smart-match-boundaries t
  "If non-nil, multiedit will try to be smart about matches when invoked from normal mode.
E.g. 'evil-multiedit-match' will not match 'evil-multiedit-match-all', or 'i' will only
match 'i' and not every individual i in, say, 'ignition'.

If evil-multiedit is invoked from visual mode, this is ignored."
  :group 'evil-multiedit
  :type 'boolean)

(defvar evil-multiedit--pt-end nil "The end of the first match")
(defvar evil-multiedit--pt-beg nil "The beginning of the first region")
(defvar evil-multiedit--pt-index (cons 1 1) "The forward/backward search indices")

(defvar evil-multiedit--dont-recall nil)
(defvar evil-multiedit--last-markers '() "List of markers from last multiedit.")
(make-variable-buffer-local 'evil-multiedit--last-markers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun evil-multiedit-restore ()
  "Restore the last group of multiedit regions."
  (interactive)
  (iedit-mode 4)
  (when evil-multiedit--last-markers
    (let ((beg-list (mapcar (lambda (m) (marker-position m)) evil-multiedit--last-markers)))
      (save-excursion
        (mapc (lambda (ov)
                (let ((beg (overlay-start ov)))
                  (unless (memq beg beg-list)
                    (goto-char beg)
                    (iedit-toggle-selection))))
              iedit-occurrences-overlays))))
  (evil-multiedit-state))

;;;###autoload
(defun evil-multiedit-match-all ()
  "Highlight all matches of the current selection (or symbol under pointer) as multiedit
regions."
  (interactive)
  (if (fboundp 'ahs-clear) (ahs-clear))
  (let* ((bounds (evil-multiedit--match-bounds))
         (beg (car bounds))
         (end (cdr bounds))
         evil-multiedit-smart-match-boundaries)
    (setq evil-multiedit--dont-recall t)
    (evil-multiedit--start beg end (point-min) (point-max))))

;;;###autoload (autoload 'evil-multiedit-match-symbol-and-next "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-symbol-and-next (&optional count)
  "Same as `evil-multiedit-match-and-next' if invoked from visual mode. From normal mode,
it grabs whole symbols rather than words."
  (interactive "<c>")
  (let ((evil-multiedit-thing-at-point-fn (lambda () (bounds-of-thing-at-point 'symbol))))
    (evil-multiedit-match-and-next count)))

;;;###autoload (autoload 'evil-multiedit-match-symbol-and-prev "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-symbol-and-prev (&optional count)
  "Same as `evil-multiedit-match-and-prev' if invoked from visual mode. From normal mode,
it grabs whole symbols rather than words."
  (interactive "<c>")
  (evil-multiedit-match-symbol-and-next (or (and count (* -1 count)) -1)))

;;;###autoload (autoload 'evil-multiedit-match-and-next "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-and-next (&optional count)
  "Marks the word at point (or, if in visual mode, the selection), then marking the next
matches on consecutive runs of this function.

Note: the matching behavior differs depending on if it was invoked from normal or visual mode.

  + From normal mode: `evil-multiedit-thing-at-point-fn' is used to grab the match under
    the cursor. Also: only whole word matches will be selected (see
    `evil-multiedit-smart-match-boundaries').
  + From visual mode, `evil-multiedit-smart-match-boundaries' is ignored, allowing for in-word
    matches."
  (interactive "<c>")
  (dotimes (i (or (and count (abs count)) 1))
    (let ((backwards-p (and count (< count 0)))
          (evil-multiedit-smart-match-boundaries (not (evil-visual-state-p))))
      (setq evil-ex-search-direction (if backwards-p 'backward 'forward))
      (unless (iedit-find-current-occurrence-overlay)
        (evil-multiedit-abort t))
      (if evil-multiedit--pt-beg
          (save-excursion
            (let ((j (if backwards-p (cdr evil-multiedit--pt-index) (car evil-multiedit--pt-index)))
                  (is-whitespace (string-match-p "^[ \t]+$" iedit-initial-string-local)))
              (goto-char (if backwards-p evil-multiedit--pt-beg evil-multiedit--pt-end))
              (while (and (> j 0)
                          (setq pt (evil-ex-find-next nil (if backwards-p 'backward 'forward) t)))
                (unless (and is-whitespace
                             evil-multiedit-ignore-indent-and-trailing
                             (< (point) (save-excursion (back-to-indentation) (point))))
                  (cl-decf j)))
              (unless (iedit-find-current-occurrence-overlay)
                (iedit-toggle-selection))
              (if (> j 0)
                  (user-error "No more matches!")
                (cl-incf (if backwards-p
                             (cdr evil-multiedit--pt-index)
                           (car evil-multiedit--pt-index)))
                (message "Added match (out of %s)" (length iedit-occurrences-overlays)))))
        (let* ((bounds (evil-multiedit--match-bounds))
               (beg (car bounds))
               (end (cdr bounds))
               occurrence)
          (setq evil-multiedit--pt-beg beg
                evil-multiedit--pt-end end)
          (evil-normal-state)
          (save-excursion
            (setq occurrence (evil-multiedit--start beg end))
            (setq evil-ex-search-pattern (evil-ex-make-search-pattern occurrence))
            (evil-ex-find-next nil nil t))))))
  (length iedit-occurrences-overlays))

;;;###autoload (autoload 'evil-multiedit-match-and-prev "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-and-prev (&optional count)
  "The backwards version of `evil-multiedit-match-and-next'"
  (interactive "<c>")
  (evil-multiedit-match-and-next (or (and count (* -1 count)) -1)))

;;;###autoload
(defun evil-multiedit-toggle-or-restrict-region (&optional beg end)
  "If in visual mode, restrict the multiedit regions to the selected region (i.e. disable
all regions outside the selection). If in any other mode, toggle the multiedit region
beneath the cursor, if one exists."
  (interactive)
  (if (iedit-current-occurrence-string)
      (cond ((or (evil-visual-state-p)
                 (and beg end))
             (let ((current-prefix-arg '(4))
                   (beg (or beg (region-beginning)))
                   (end (or end (region-end))))
               (iedit-done)
               (call-interactively 'iedit-mode)
               (save-excursion (iedit-restrict-region beg end))
               (evil-previous-line)))
            (t
             (iedit-toggle-selection)))
    (call-interactively 'evil-ret)))

;;;###autoload
(defalias 'evil-multiedit-next 'iedit-next-occurrence
  "Jump to the next multiedit region.")
;;;###autoload
(defalias 'evil-multiedit-prev 'iedit-prev-occurrence
  "Jump to the previous multiedit region.")

;;;###autoload
(defun evil-multiedit-abort (&optional inhibit-normal)
  "Clear all multiedit regions, clean up and revert to normal state."
  (interactive)
  (mapc (lambda (m) (set-marker m nil)) evil-multiedit--last-markers)
  (if evil-multiedit--dont-recall
      (setq evil-multiedit--last-markers '())
    (mapc (lambda (ov)
            (let ((m (make-marker)))
              (set-marker-insertion-type m t)
              (set-marker m (overlay-start ov))
              (push m evil-multiedit--last-markers)))
          iedit-occurrences-overlays))
  (iedit-done)
  (unless inhibit-normal
    (evil-normal-state))
  (evil-multiedit--cleanup))

;;;###autoload (autoload 'evil-multiedit-ex-match "evil-multiedit" nil t)
(evil-define-command evil-multiedit-ex-match (&optional beg end bang regexp)
  "Ex command for invoking evil-multiedit with a regular expression. The selected area is
the boundary for matches. If BANG, invert `evil-multiedit-smart-match-boundaries'."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-multiedit--match-bounds ()
  (cond ((evil-visual-state-p)
         (cons evil-visual-beginning evil-visual-end))
        ((looking-at-p "[^a-zA-Z0-9]")
         (cons (point) (1+ (point))))
        (t
         (funcall evil-multiedit-thing-at-point-fn))))

(defun evil-multiedit--start (obeg oend &optional beg end)
  (let* ((occurrence (buffer-substring-no-properties obeg oend))
         (sym-p (string-match-p "^[^a-zA-Z0-9]$" occurrence)))
    (when occurrence
      (setq occurrence (regexp-quote occurrence))
      (when (and evil-multiedit-smart-match-boundaries
                 (not sym-p))
        (when (and (goto-char (1- obeg))
                   (looking-at "[^a-zA-Z0-9]"))
          (setq occurrence (concat "\\<" occurrence)))
        (when (and (goto-char oend)
                   (looking-at "[^a-zA-Z0-9]"))
          (setq occurrence (concat occurrence "\\>"))))
      (evil-multiedit--start-regexp occurrence (or beg obeg) (or end oend)))))

(defun evil-multiedit--start-regexp (regexp &optional beg end)
  (setq iedit-initial-string-local regexp)
  (iedit-start regexp beg end)
  (evil-multiedit-state)
  regexp)

(defun evil-multiedit--cleanup ()
  (setq evil-multiedit--dont-recall nil
        evil-multiedit--pt-end nil
        evil-multiedit--pt-beg nil
        evil-multiedit--pt-index (cons 1 1)))

(defmacro evil-multiedit--switch-to-insert-state-after (command &optional interactive)
  "Call COMMAND and switch to iedit-insert state.
If INTERACTIVE is non-nil then COMMAND is called interactively."
  `(progn
     (if ,interactive
         (call-interactively ',command)
       (funcall ',command))
     ;; required to correctly update the cursors
     (evil-multiedit-state)
     (evil-multiedit-insert-state)))

(defun evil-multiedit--goto-overlay-start ()
  "Return the position of the start of the current overlay."
  (let ((overlay (iedit-find-current-occurrence-overlay)))
    (if overlay
        (goto-char (overlay-start overlay))
      (call-interactively 'evil-digit-argument-or-evil-beginning-of-line))))

(defun evil-multiedit--goto-overlay-end ()
  "Return the position of the end of the current overlay."
  (let ((overlay (iedit-find-current-occurrence-overlay)))
    (if overlay
        (goto-char (overlay-end overlay))
      (call-interactively 'evil-end-of-line))))

(defun evil-multiedit--beginning-of-line ()
  "Go to the beginning of the current overlay."
  (interactive)
  (evil-multiedit--goto-overlay-start))

(defun evil-multiedit--end-of-line ()
  "Go to the beginning of the current overlay."
  (interactive)
  (evil-multiedit--goto-overlay-end))

(defun evil-multiedit--append-line ()
  "Put the point at then end of current overlay and switch to
`iedit-insert state'."
  (interactive)
  (if (iedit-find-current-occurrence-overlay)
      (evil-multiedit--switch-to-insert-state-after evil-multiedit--goto-overlay-end)
    (evil-multiedit--switch-to-insert-state-after evil-append-line t)))

(defun evil-multiedit--insert-line ()
  "Put the point at then end of current overlay and switch to
`iedit-insert state'."
  (interactive)
  (evil-multiedit--switch-to-insert-state-after
   evil-multiedit--goto-overlay-start))

(defun evil-multiedit--change ()
  "Wipe all the occurrences and switch in `iedit-insert state'"
  (interactive)
  (evil-multiedit--switch-to-insert-state-after evil-change t))

(defun evil-multiedit--append ()
  "Append and switch to `iedit-insert state'"
  (interactive)
  (evil-multiedit--switch-to-insert-state-after evil-append t))

(defun evil-multiedit--open-below ()
  "Insert new line below and switch to `iedit-insert state'"
  (interactive)
  (evil-multiedit--switch-to-insert-state-after evil-open-below t))

(defun evil-multiedit--open-above ()
  "Insert new line above and switch to `iedit-insert state'"
  (interactive)
  (evil-multiedit--switch-to-insert-state-after evil-open-above t))

(defun evil-multiedit--visual-line ()
  "Visually select edit region."
  (interactive)
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (if ov
        (evil-visual-make-region (overlay-start ov) (1- (overlay-end ov)) 'exclusive)
      (call-interactively 'evil-visual-line))))

(defun evil-multiedit--substitute ()
  "Wipe all the occurrences and switch in `iedit-insert state'"
  (interactive)
  (if (not (iedit-find-current-occurrence-overlay))
      (call-interactively 'evil-change-line)
    (iedit-delete-occurrences)
    (evil-multiedit-insert-state)))

(defun evil-multiedit--paste-replace (count)
  "Replace the selection with the yanked text."
  (interactive "P")
  (iedit-delete-occurrences)
  (evil-paste-before count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-multiedit*iedit-done ()
  "Advice function that exits Iedit mode and retains occurrence state properly."
  (when iedit-buffering
    (iedit-stop-buffering))
  (setq iedit-last-occurrence-local (iedit-current-occurrence-string)
        iedit-last-occurrence-global iedit-last-occurrence-local
        iedit-last-initial-string-global iedit-initial-string-local
        iedit-num-lines-to-expand-up 0
        iedit-num-lines-to-expand-down 0)
  (iedit-cleanup)
  (setq iedit-initial-string-local nil
        iedit-mode nil)
  (remove-hook 'kbd-macro-termination-hook 'iedit-done t)
  (remove-hook 'change-major-mode-hook 'iedit-done t)
  (remove-hook 'iedit-aborting-hook 'iedit-done t)
  (run-hooks 'iedit-mode-end-hook)
  (advice-remove 'evil-force-normal-state 'evil-multiedit-abort)
  (advice-remove 'iedit-done 'evil-multiedit*iedit-done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-state multiedit
  "`multiedit state' interfacing iedit mode."
  :tag " <ME> "
  :enable (normal)
  :cursor box
  :message "-- MULTIEDIT --"
  (when (eq evil-state 'multiedit)
    (advice-add 'iedit-done :override 'evil-multiedit*iedit-done)
    (advice-add 'evil-force-normal-state :before 'evil-multiedit-abort)
    (if (evil-replace-state-p) (call-interactively 'iedit-mode))))

(evil-define-state multiedit-insert
  "Replace insert state in `iedit state'."
  :tag " <MEi> "
  :enable (insert)
  :cursor (bar . 2)
  :message "-- MULTIEDIT INSERT --")

(when evil-multiedit-dwim-motion-keys
  (let ((map evil-multiedit-insert-state-map))
    (define-key map (kbd "C-g") 'evil-multiedit-abort)
    (define-key map [escape]    'evil-multiedit-state))

  (let ((map evil-multiedit-state-map))
    (evil-redirect-digit-argument map "0" 'evil-multiedit--beginning-of-line)
    (define-key map "^"                   'evil-multiedit--beginning-of-line)
    (define-key map "$"                   'evil-multiedit--end-of-line)
    (define-key map "a"         'evil-multiedit--append)
    (define-key map "A"         'evil-multiedit--append-line)
    (define-key map "c"         'evil-multiedit--change)
    (define-key map "C"         'evil-multiedit--substitute)
    (define-key map "D"         'iedit-delete-occurrences)
    (define-key map "gg"        'iedit-goto-first-occurrence)
    (define-key map "G"         'iedit-goto-last-occurrence)
    (define-key map "i"         'evil-multiedit-insert-state)
    (define-key map "I"         'evil-multiedit--insert-line)
    (define-key map "o"         'evil-multiedit--open-below)
    (define-key map "O"         'evil-multiedit--open-above)
    (define-key map "p"         'evil-multiedit--paste-replace)
    (define-key map (kbd "C-g") 'evil-multiedit-abort)
    (define-key map [escape]    'evil-multiedit-abort)
    (define-key map "V"         'evil-multiedit--visual-line)
    (define-key map "za"        'iedit-toggle-unmatched-lines-visible)))

(provide 'evil-multiedit)
;;; evil-multiedit.el ends here
