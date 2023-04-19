(defpackage text-socket/tests/main
  (:use :cl
        :text-socket
        :rove))
(in-package :text-socket/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :text-socket)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
