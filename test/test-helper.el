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
         (pop-to-buffer (current-buffer))
         (insert ,initial)
         (goto-char (point-min))
         (cond ((search-forward "||" nil t)
                (delete-char -2)
                (setq beg (point))
                (if (search-forward "||" nil t)
                    (progn
                      (delete-char -2)
                      (setq end (point)))
                  (error "No end marker detected"))
                (setq selection (buffer-substring-no-properties beg end))
                (evil-visual-make-selection beg end 'exclusive))
               ((search-forward "|" nil t)
                (delete-char -1)))
         (setq orig-buffer-string (buffer-string))
         ,@rest))))

(defmacro from! (point &rest forms)
  (declare (indent 1))
  `(progn
     (goto-char ,point)
     ,@forms))

