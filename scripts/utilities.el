;; -*- Mode: emacs-lisp-mode; Syntax: lisp; indent-tabs-mode-nil; coding: utf-8; show-trailing-whitespace: t -*-

(defun generate-vectors-table ()
  (let* ((start (if (search-forward "*/" nil t)
                    (progn
                      (goto-char (match-end 0))
                      (end-of-line)
                      (point))
                  (point)))
         (end (if (search-forward "* end vectors *" nil t)
                  (progn
                    (goto-char (match-end 0))
                    (end-of-line)
                    (point))
                start)))
    (delete-region start end)
    (insert "\n\n")
    (dolist (v '((:name "Initial_SP_value" :count 1 :mnemonic ".long __stack_end__")
                 (:name "Reset" :count 1 :mnemonic ".long reset")
                 (:name "NMI" :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "Hard_fault" :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "Memory_management_fault" :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "Bus_fault" :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "Usage_fault" :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "Reserved" :count 4 :start 1 :mnemonic ".long __bad_ISR__")
                 (:name "SVCall" :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "Reserved_for_Debug" :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "Reserved" :start 5 :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "PendSV" :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "Systick" :count 1 :mnemonic ".long __bad_ISR__")
                 (:name "IRQ" :count 240 :start 0 :mnemonic ".long __bad_ISR__"))
               (insert "\n/* end vectors */"))
      (cond
       ((integerp (getf v :count))
	(if (> (getf v :count) 1)
	    (loop
	     for i from 0 below (getf v :count)
	     do
	     (insert (format "\n_%s_%d_vector:\n\t%s\n" (getf v :name) (+ i (getf v :start)) (getf v :mnemonic))))
	  (if (integerp (getf v :start))
	      (insert (format "\n_%s_%d_vector:\n\t%s\n" (getf v :name) (getf v :start) (getf v :mnemonic)))
	    (insert (format "\n_%s_vector:\n\t%s\n" (getf v :name) (getf v :mnemonic))))))
       (t
        (insert "\n;\n"))))))

(defun generate-select-case (variable-name cases)
  (let* ((start (if (search-forward "! start select case" nil t)
                    (progn
                      (goto-char (match-end 0))
                      (end-of-line)
                      (point))
                  (point)))
         (end (if (search-forward "! end select case" nil t)
                  (progn
                    (goto-char (match-end 0))
                    (end-of-line)
                    (point))
                start)))
    (delete-region start end)
    (insert "\n")
    (insert (format "select case (%s)\n" variable-name))
    (when cases
      (dolist (v cases (insert "\nend select case\n"))
        (cond
         ((getf v :label)
          (insert (format "  case (%s)\n" (getf v :label)))
          (if (getf v :case-code)
              (loop
               finally (insert "\n")
               for x in (getf v :case-code)
               do
               (insert (format "    %s\n" x)))
            (insert "\n")))
         (t
          (insert "\n"))))
      (insert "! end select case\n\n"))))

(generate-select-case "i2" '((:label "1" :case-code ("go to 6188"))
                             (:label "2" :case-code ("go to 6189"))
                             (:label "3" :case-code ("go to 6190"))
                             (:label "4" :case-code ("go to 6191"))
                             (:label "5" :case-code ("go to 6192"))
                             (:label "6" :case-code ("go to 6193"))))

! start select case
select case (i2)
  case (1)
    go to 6188

  case (2)
    go to 6189

  case (3)
    go to 6190

  case (4)
    go to 6191

  case (5)
    go to 6192

  case (6)
    go to 6193


end select case
! end select case






