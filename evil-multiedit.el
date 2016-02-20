;;; evil-multiedit.el --- multiple cursors for evil-mode
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: February 20, 2016
;; Modified: February 20, 2016
;; Version: 1.0.0
;; Keywords: multiple cursors, editing, iedit
;; Homepage: https://github.com/hlissner/evil-multiedit
;; Package-Requires: ((evil "1.2.10") (iedit "0.97") (evil-multiedit-state "1.0"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;
;;
;;; Code:

(defvar iedit-occurrence-keymap-default (make-sparse-keymap))

(require 'evil)
(require 'iedit)

(defgroup evil-multiedit nil
  ""
  :prefix "evil-multiedit-"
  :group 'evil)

(defcustom evil-multiedit-dwim-motion-keys t
  "Whether or not to modify evil's motion keys to act differently when the cursor is
  inside multiedit regions."
  :group 'evil-multiedit
  :type 'boolean)

(defvar evil-multiedit--pt nil "The point of the first match")
(defvar evil-multiedit--pt-first nil "The beginning of the current region")
(defvar evil-multiedit--pt-index (cons 1 1) "The forward/backward search indices")

(evil-define-state multiedit
  "`multiedit state' interfacing iedit mode."
  :tag " <ME> "
  :enable (normal)
  :cursor box
  :message "-- MULTIEDIT --"
  ;; force iedit mode
  (if (evil-replace-state-p) (call-interactively 'iedit-mode))
  (if (eq evil-state 'multiedit)
      (advice-add 'iedit-done :override 'evil-multiedit*iedit-done)
    (advice-remove 'iedit-done 'evil-multiedit*iedit-done)))

(evil-define-state multiedit-insert
  "Replace insert state in `iedit state'."
  :tag " <MEi> "
  :enable (insert)
  :cursor (bar . 2)
  :message "-- MULTIEDIT INSERT --")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun evil-multiedit-match-all (&optional arg)
  (interactive "P")
  (if (fboundp 'ahs-clear) (ahs-clear))
  (iedit-mode arg)
  (evil-multiedit-state))

;;;###autoload (autoload 'evil-multiedit-match-and-next "evil-multiedit" nil t)
(evil-define-command evil-multiedit-match-and-next (&optional count)
  "Emulates Sublime Text's (and Atom's) multiple cursors functionality by marking the
current word/selection and marking the next one on consecutive executions of this
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
              (decf i))
            (if (> i 0)
                (message "No more matches!")
              (incf (if backwards-p
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
  (interactive "<c>")
  (evil-multiedit-match-and-next (or (and count (* -1 count)) -1)))

;;;###autoload
(defun evil-multiedit-toggle-or-restrict-region (&optional beg end)
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
(defalias 'evil-multiedit-next 'iedit-next-occurrence)
;;;###autoload
(defalias 'evil-multiedit-prev 'iedit-prev-occurrence)

;;;###autoload
(defun evil-multiedit-abort ()
  (interactive)
  (iedit-done)
  (evil-normal-state)
  (evil-multiedit--cleanup))

(defun evil-multiedit--cleanup ()
  (setq evil-multiedit--pt nil
        evil-multiedit--pt-first nil
        evil-multiedit--pt-index (cons 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-multiedit*iedit-done ()
  "Exit Iedit mode. Save the current occurrence string locally and globally. Save the
initial string globally."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(advice-add 'evil-force-normal-state :after 'evil-multiedit-abort)

(when evil-multiedit-dwim-motion-keys
  (define-key evil-multiedit-state-map "$"                   'evil-multiedit--end-of-line)
  (evil-redirect-digit-argument evil-multiedit-state-map "0" 'evil-multiedit--beginning-of-line)

  (define-key evil-multiedit-state-map "a"         'evil-multiedit--append)
  (define-key evil-multiedit-state-map "A"         'evil-multiedit--append-line)
  (define-key evil-multiedit-state-map "c"         'evil-multiedit--change)
  (define-key evil-multiedit-state-map "C"         'evil-multiedit--substitute)
  (define-key evil-multiedit-state-map "D"         'iedit-delete-occurrences)
  (define-key evil-multiedit-state-map "gg"        'iedit-goto-first-occurrence)
  (define-key evil-multiedit-state-map "G"         'iedit-goto-last-occurrence)
  (define-key evil-multiedit-state-map "i"         'evil-multiedit-insert-state)
  (define-key evil-multiedit-state-map "I"         'evil-multiedit--insert-line)
  (define-key evil-multiedit-state-map "o"         'evil-multiedit--open-below)
  (define-key evil-multiedit-state-map "O"         'evil-multiedit--open-above)
  ;; (define-key evil-multiedit-state-map "p"         'evil-multiedit--paste)
  (define-key evil-multiedit-state-map (kbd "C-g") 'evil-multiedit-abort)
  (define-key evil-multiedit-state-map [escape]    'evil-multiedit-abort)
  (define-key evil-multiedit-state-map "V"         'evil-multiedit--visual-line)

  (define-key evil-multiedit-state-map "za"        'iedit-toggle-unmatched-lines-visible)

  (define-key evil-multiedit-insert-state-map (kbd "C-g") 'evil-multiedit-abort)
  (define-key evil-multiedit-insert-state-map [escape]    'evil-multiedit-state))

(provide 'evil-multiedit)
;;; evil-multiedit.el ends here
