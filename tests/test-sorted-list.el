(require 'sorted-list)
(require 'cl-lib)
;; create sorted lists from lists
(ert-deftest sorted-list-created-sorted ()
  (let ((inouts '((1 2 3 4) (1 2 3 4)
                  (6 2 7 15 40 11 11 10 2 2 2 2) (2 2 2 2 2 6 7 10 11 11 15 40)
                  (1 1 1 1 1) (1 1 1 1 1)
                  (3 2 1) (1 2 3))))
    (cl-loop for (input expectedoutput) on inouts by #'cddr
             do
             (let ((result (sorted-list--create input '<)))
               (should (equal (sorted-list--lst result)
                              expectedoutput))))))

;; insert into empty list
(ert-deftest sorted-list-insert ()
  (let ((inouts '((1 2 3 4) (1 2 3 4)
                  (6 2 7 15 40 11 11 10 2 2 2 2) (2 2 2 2 2 6 7 10 11 11 15 40)
                  (1 1 1 1 1) (1 1 1 1 1)
                  (3 2 1) (1 2 3))))
    (cl-loop for (input expectedoutput) on inouts by #'cddr
             do
             (let ((result (sorted-list--create nil '<)))
               (dolist (i input)
                 (sorted-list-insert result i))
               (should (equal (sorted-list--lst result)
                              expectedoutput))))))

;; delete from list
(ert-deftest sorted-list-delete ()
  (let ((indeleteexpected '((1 2 3 4) (2 4) (1 3)
                            (6 2 7 15 40 11 11 10 2 2 2 2) (11 40 2 2 2 2) (2 6 7 10 11 15))))
    (cl-loop for (input deleted expectedoutput) on indeleteexpected by #'cdddr
             do
             (let ((result (sorted-list--create input '<)))
               (dolist (i deleted)
                 (sorted-list-delete result i))
               (should (equal (sorted-list--lst result)
                              expectedoutput))))))
