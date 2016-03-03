
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

