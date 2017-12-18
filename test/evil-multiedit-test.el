
(ert-deftest evil-multiedit-selection-test ()
  :tags '(evil-multiedit)
  (with! "The ||quick|| brown"
    (should (string= selection "quick"))))

(ert-deftest evil-multiedit-match-all-test ()
  :tags '(evil-multiedit)
  (with! "The ||quick|| brown\nfox quickly quickens\nhis quick reflexes to\nquickly jump over the not so quick\ndog."
    (evil-multiedit-match-all)
    (should (string= iedit-initial-string-local "quick"))
    (should (= 6 (length iedit-occurrences-overlays)))))

(ert-deftest evil-multiedit-incremental-match-test ()
  :tags '(evil-multiedit)
  (with! "The qu|ick brown fox was as quick as quick can be"
    (should (= 1 (evil-multiedit-match-and-prev)))
    (should (string= iedit-initial-string-local "\\<quick\\>"))
    (should-error (evil-multiedit-match-and-prev)))
  (with! "The quick brown fox was as qu|ick as quick can be"
    (should (= 1 (evil-multiedit-match-and-next)))
    (should (string= iedit-initial-string-local "\\<quick\\>"))
    (should (= 2 (evil-multiedit-match-and-next)))
    (should (= 3 (evil-multiedit-match-and-prev)))
    (should (string= iedit-initial-string-local "\\<quick\\>"))
    (should-error (evil-multiedit-match-and-next))
    (should-error (evil-multiedit-match-and-prev))
    (should (string= iedit-initial-string-local "\\<quick\\>"))))

(ert-deftest evil-multiedit-incremental-visual-match-backward-test ()
  :tags '(evil-multiedit)
  (with! "The quick brown fox was as quick as ||quick brown|| foxes can be"
    (should (= 1 (evil-multiedit-match-and-prev)))
    (should (string= iedit-initial-string-local "quick brown"))
    (should (= 2 (evil-multiedit-match-and-prev))))

  (with! "The ||quick brown|| fox was as quick as quick brown foxes can be"
    (should (= 1 (evil-multiedit-match-and-next)))
    (should (string= iedit-initial-string-local "quick brown"))
    (should (= 2 (evil-multiedit-match-and-next)))))

(ert-deftest evil-multiedit-scope-test ()
  :tags '(evil-multiedit)
  (let ((evil-multiedit-scope 'line))
    (with! "The q|uick\n brown fox\n was as\n quick as\n quick brown foxes\n can be"
      (should (= 1 (evil-multiedit-match-symbol-and-next)))
      (should (string= iedit-initial-string-local "\\_<quick\\_>"))
      (should-error (evil-multiedit-match-symbol-and-next)))

    (with! "The quick\n brown fox\n was as\n quick as\n quick b|rown foxes\n can be"
      (should (= 1 (evil-multiedit-match-symbol-and-prev)))
      (should (string= iedit-initial-string-local "\\_<brown\\_>"))
      (should-error (evil-multiedit-match-symbol-and-next)))))

(ert-deftest evil-multiedit-follow-test ()
  :tags '(evil-multiedit)
  (let ((evil-multiedit-follow-matches t))
    (with! "The q|uick\n brown fox\n was as\n quick as\n quick brown foxes\n can be"
      (should (= 1 (evil-multiedit-match-symbol-and-next)))
      (let ((origin (point)))
        (should (and (evil-multiedit-match-symbol-and-next)
                     (evil-multiedit-match-symbol-and-next)
                     (/= (point) origin)
                     (looking-at-p "quick")))))

    (with! "The quick\n brown fox\n was as\n quick as\n qu|ick brown foxes\n can be"
      (should (= 1 (evil-multiedit-match-symbol-and-prev)))
      (let ((origin (point)))
        (should (and (evil-multiedit-match-symbol-and-prev)
                     (evil-multiedit-match-symbol-and-prev)
                     (/= (point) origin)
                     (looking-at-p "quick"))))))

  (let (evil-multiedit-follow-matches)
    (with! "The q|uick\n brown fox\n was as\n quick as\n quick brown foxes\n can be"
      (should (= 1 (evil-multiedit-match-symbol-and-next)))
      (let ((origin (point)))
        (should (and (evil-multiedit-match-symbol-and-next)
                     (evil-multiedit-match-symbol-and-next)
                     (= (point) origin)))))

    (with! "The quick\n brown fox\n was as\n quick as\n qu|ick brown foxes\n can be"
      (should (= 1 (evil-multiedit-match-symbol-and-prev)))
      (let ((origin (point)))
        (should (and (evil-multiedit-match-symbol-and-prev)
                     (evil-multiedit-match-symbol-and-prev)
                     (= (point) origin)))))))
