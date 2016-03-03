;;; evil-multiedit-operations-test.el

(defmacro with-op! (initial start-pos macro expected)
  (declare (indent 1))
  `(with! ,initial
     (evil-multiedit--start beg end (point-min) (point-max))
     (goto-char ,start-pos)
     (execute-kbd-macro ,macro)
     (should (string= (buffer-string) ,expected))))


(ert-deftest evil-multiedit-insert-test ()
  :tags '(evil-multiedit-operations)
  (with-op! "The ||quick|| brown fox was quick" beg
            "iweird and "
            "The weird and quick brown fox was weird and quick")

  (with-op! "The ||quick|| brown fox was quick" end
            "i and weird"
            "The quick and weird brown fox was quick and weird"))

(ert-deftest evil-multiedit-insert-line-test ()
  :tags '(evil-multiedit-operations)
  (with-op! "The ||quick|| brown fox was quick" end
            "Iweird and "
            "The weird and quick brown fox was weird and quick"))

(ert-deftest evil-multiedit-append-line-test ()
  :tags '(evil-multiedit-operations)
  (with-op! "The ||quick|| brown fox was quick" beg
            "A and weird"
            "The quick and weird brown fox was quick and weird"))

(ert-deftest evil-multiedit-change-test ()
  :tags '(evil-multiedit-operations)
  (with-op! "The ||quick|| brown fox was quick" beg
            "clk"
            "The kuick brown fox was kuick")
  (with-op! "The ||quick|| brown fox was quick" beg
            "ciwfast"
            "The fast brown fox was fast"))

(ert-deftest evil-multiedit-change-line-test ()
  :tags '(evil-multiedit-operations)
  (with-op! "The ||quick|| brown fox was quick" beg
            "Cweird"
            "The weird brown fox was weird"))

(ert-deftest evil-multiedit-delete-test ()
  :tags '(evil-multiedit-operations)
  (with-op! "The ||quick|| brown fox was quick" beg
            "D"
            "The  brown fox was "))

(ert-deftest evil-multiedit-$/^-test ()
  :tags '(evil-multiedit-operations)
  (with! "The ||quick|| brown fox was quick"
    (evil-multiedit--start beg end (point-min) (point-max))
    (goto-char beg)
    (evil-multiedit--end-of-line)
    (should (looking-at-p " brown"))
    (should-not (= (point) (point-max)))

    (goto-char end)
    (evil-multiedit--beginning-of-line)
    (should (looking-at-p "quick"))
    (should-not (= (point) (point-min)))))

