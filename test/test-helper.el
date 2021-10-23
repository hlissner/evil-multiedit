;;; test-helper.el --- Set up test environment -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Set up environment
(setq package-user-dir (expand-file-name ".packages/" (file-name-directory load-file-name))
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      package-archives '(("melpa" . "https://melpa.org/packages/")))

(require 'cl-lib)
(package-initialize)
(let ((packages (cl-remove-if #'package-installed-p '(evil iedit))))
  (when packages
    (package-refresh-contents))
  (mapc #'package-install packages))



;; Bootstrap tests
(require 'ert)
(require 'evil-multiedit)

(defmacro with! (initial &rest rest)
  (declare (indent 1)
           (debug (form form body)))
  `(save-window-excursion
     (let ((case-fold-search nil)
           orig-buffer-string beg end selection)
       (with-temp-buffer
         (set-input-method nil)
         (evil-local-mode +1)
         (save-excursion (insert ,initial))
         (cond ((re-search-forward "||\\([^|]+\\)||" nil t)
                (setq selection (match-string-no-properties 1))
                (replace-match selection)
                (setq beg (match-beginning 0)
                      end (match-end 0))
                (evil-visual-make-selection beg end 'exclusive))
               ((search-forward "|" nil t)
                (delete-char -1)))
         (setq orig-buffer-string (buffer-string))
         ;; HACK iedit uses `recenter' in an unusual way, which errors if there
         ;;      is no window/buffer to recenter (as is the case when running
         ;;      headless tests), so we create one.
         (switch-to-buffer (current-buffer))
         ,@rest))))

(defmacro from! (point &rest forms)
  (declare (indent 1))
  `(progn
     (goto-char ,point)
     ,@forms))


;;
;;; Bootstrap

(let ((args (member "--" command-line-args-left)))
  (when args
    (while args
      (let ((regexp "\\.el\\'")
            (path (expand-file-name (pop args))))
        (if (file-directory-p path)
            (setq args
                  (append (directory-files path nil regexp)
                          args))
          (when (string-match-p regexp path)
            (load path nil t)))))
    (ert-run-tests-batch)))

;;; test-helper.el ends here
