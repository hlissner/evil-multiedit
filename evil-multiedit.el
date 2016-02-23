;;; evil-multiedit.el --- multiple cursors for evil-mode
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: February 20, 2016
;; Modified: February 22, 2016
;; Version: 1.1.2
;; Keywords: multiple cursors, editing, iedit
;; Homepage: https://github.com/hlissner/evil-multiedit
;; Package-Requires: ((evil "1.2.10") (iedit "0.97") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This plugin tries to fill that multi-cursor shaped gap in your heart by bringing
;; Sublime Text (or Atom's) multiple cursor functionality to Emacs and evil-mode.
;;
;; Note: Credit goes to [syl20bnr]() for his [evil-iedit-state]() plugin, which this
;; plugin was heavily inspired by.
;;
;; Installation:
;;
;;   `evil-multiedit` will be available on MELPA soon.
;;
;;   For now, download `evil-multiedit.el` somewhere in your `load-path`.
;;
;;     (require 'evil-multiedit)
;;
;; Usage
;;
;;   evil-multiedit *does not bind any new keys*, so as not to impose, so you will have to
;;   yourself. Here is my recommended configuration:
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

(defvar evil-multiedit--pt nil "The point of the first match")
(defvar evil-multiedit--pt-first nil "The beginning of the current region")
(defvar evil-multiedit--pt-index (cons 1 1) "The forward/backward search indices")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun evil-multiedit-match-all (&optional arg)
  "Highlight all matches of the current selection as multiedit regions."
  (interactive "P")
  (if (fboundp 'ahs-clear) (ahs-clear))
  (iedit-mode arg)
  (evil-multiedit-state))

;;;###autoload (autoload 'evil-multiedit-match-and-next "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-and-next (&optional count)
  "Emulates Sublime Text's (and Atom's) multiple cursors functionality by marking the word
at point (or selection) and marking the next one on consecutive executions of this
function."
  (interactive "<c>")
  (let ((backwards-p (and count (< count 0))))
    (setq evil-ex-search-direction (if backwards-p 'backward 'forward))
    (save-excursion
      (if evil-multiedit--pt-first
          (let ((i (if backwards-p (cdr evil-multiedit--pt-index) (car evil-multiedit--pt-index))))
            (goto-char evil-multiedit--pt)
            (while (and (> i 0)
                        (evil-ex-find-next nil (if backwards-p 'backward 'forward) t))
              (cl-decf i))
            (if (> i 0)
                (message "No more matches!")
              (cl-incf (if backwards-p
                           (cdr evil-multiedit--pt-index)
                         (car evil-multiedit--pt-index))))
            (unless (iedit-find-current-occurrence-overlay)
              (iedit-toggle-selection)))
        (let* ((bounds (if (evil-visual-state-p)
                           (cons evil-visual-beginning evil-visual-end)
                         (bounds-of-thing-at-point 'word)))
               (beg (car bounds))
               (end (cdr bounds))
               (occurrence (buffer-substring-no-properties (car bounds) (cdr bounds))))
          (setq evil-multiedit--pt-first (if backwards-p beg end)
                evil-multiedit--pt (if backwards-p beg end))
          (evil-normal-state)
          (setq iedit-initial-string-local occurrence)
          (iedit-start (iedit-regexp-quote occurrence) beg end)
          (evil-multiedit-state)
          (setq evil-ex-search-pattern (evil-ex-make-search-pattern (regexp-quote occurrence)))
          (evil-ex-find-next nil nil t))))))

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
      (cond ((evil-visual-state-p)
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
(defun evil-multiedit-abort ()
  "Clear all multiedit regions, clean up and revert to normal state."
  (interactive)
  (iedit-done)
  (evil-normal-state)
  (evil-multiedit--cleanup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-multiedit--cleanup ()
  (setq evil-multiedit--pt nil
        evil-multiedit--pt-first nil
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

(defun evil-multiedit--beginning-of-line (count)
  "Go to the beginning of the current overlay."
  (interactive "p")
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
  (force-mode-line-update)
  (remove-hook 'kbd-macro-termination-hook 'iedit-done t)
  (remove-hook 'change-major-mode-hook 'iedit-done t)
  (remove-hook 'iedit-aborting-hook 'iedit-done t)
  (run-hooks 'iedit-mode-end-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-state multiedit
  "`multiedit state' interfacing iedit mode."
  :tag " <ME> "
  :enable (normal)
  :cursor box
  :message "-- MULTIEDIT --"
  (if (eq evil-state 'multiedit)
      (progn
        (advice-add 'iedit-done :override 'evil-multiedit*iedit-done)
        (advice-add 'evil-force-normal-state :after 'evil-multiedit-abort)
        (if (evil-replace-state-p) (call-interactively 'iedit-mode)))
    (advice-remove 'iedit-done 'evil-multiedit*iedit-done)
    (advice-remove 'evil-force-normal-state 'evil-multiedit-abort)))

(evil-define-state multiedit-insert
  "Replace insert state in `iedit state'."
  :tag " <MEi> "
  :enable (insert)
  :cursor (bar . 2)
  :message "-- MULTIEDIT INSERT --")

(let ((me-map evil-multiedit-state-map)
      (me-imap evil-multiedit-insert-state-map))
  (when evil-multiedit-dwim-motion-keys
    (define-key me-map "$"                   'evil-multiedit--end-of-line)
    (evil-redirect-digit-argument me-map "0" 'evil-multiedit--beginning-of-line)

    (define-key me-map "a"         'evil-multiedit--append)
    (define-key me-map "A"         'evil-multiedit--append-line)
    (define-key me-map "c"         'evil-multiedit--change)
    (define-key me-map "C"         'evil-multiedit--substitute)
    (define-key me-map "D"         'iedit-delete-occurrences)
    (define-key me-map "gg"        'iedit-goto-first-occurrence)
    (define-key me-map "G"         'iedit-goto-last-occurrence)
    (define-key me-map "i"         'evil-multiedit-insert-state)
    (define-key me-map "I"         'evil-multiedit--insert-line)
    (define-key me-map "o"         'evil-multiedit--open-below)
    (define-key me-map "O"         'evil-multiedit--open-above)
    (define-key me-map "p"         'evil-multiedit--paste-replace)
    (define-key me-map (kbd "C-g") 'evil-multiedit-abort)
    (define-key me-map [escape]    'evil-multiedit-abort)
    (define-key me-map "V"         'evil-multiedit--visual-line)

    (define-key me-map "za"        'iedit-toggle-unmatched-lines-visible)

    (define-key me-imap (kbd "C-g") 'evil-multiedit-abort)
    (define-key me-imap [escape]    'evil-multiedit-state)))

(provide 'evil-multiedit)
;;; evil-multiedit.el ends here
