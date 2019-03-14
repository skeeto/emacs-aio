;;; aio-tests.el --- async unit test suite for aio -*- lexical-binding: t; -*-

;;; Commentary:

;;  $ emacs -batch -Q -l aio-test.elc -f ert-run-tests-batch

;; Because the tests run as async functions, the test suite cannot be
;; run in batch mode. The results will be written into a buffer and
;; Emacs will be left running so you can see the results.

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'ert)

(defmacro aio-with-test (timeout &rest body)
  "Run body asynchronously but block synchronously until it completes.

If TIMEOUT seconds passes without completion, signal an
aio-timeout to cause the test to fail."
  (declare (indent 1))
  `(let* ((promises (list (aio-with-async ,@body)
                          (aio-timeout ,timeout)))
          (select (aio-make-select promises)))
     (aio-wait-for
      (aio-with-async
        (aio-await (aio-await (aio-select select)))))))

;; Tests:

(ert-deftest sleep ()
  (aio-with-test 3
    (let ((start (float-time)))
      (dotimes (i 3)
        (should (eql i
                     (aio-await (aio-sleep 0.5 i)))))
      (should (> (- (float-time) start)
                 1.4)))))

(ert-deftest repeat ()
  (aio-with-test 3
    (let ((sub (aio-lambda (result) (aio-await (aio-sleep .1 result)))))
      (should (eq :a (aio-await (funcall sub :a))))
      (should (eq :b (aio-await (funcall sub :b)))))))

(ert-deftest timeout ()
  (aio-with-test 4
    (let ((sleep (aio-sleep 1.0 t))
          (timeout (aio-timeout 0.5))
          (select (aio-make-select)))
      (aio-select-add select sleep)
      (aio-select-add select timeout)
      (let ((winner (aio-await (aio-select select))))
        (should (equal '(:error aio-timeout . 0.5)
                       (aio-await (aio-catch winner))))))
    (let ((sleep (aio-sleep 0.1 t))
          (timeout (aio-timeout 0.5))
          (select (aio-make-select)))
      (aio-select-add select sleep)
      (aio-select-add select timeout)
      (let ((winner (aio-await (aio-select select))))
        (should (equal '(:success . t)
                       (aio-await (aio-catch winner))))))))

(defun aio-test--shuffle (values)
  "Return a shuffled copy of VALUES."
  (let ((v (vconcat values)))
    (cl-loop for i from (1- (length v)) downto 1
             for j = (cl-random (+ i 1))
             do (cl-rotatef (aref v i) (aref v j))
             finally return (append v nil))))

(ert-deftest sleep-sort ()
  (aio-with-test 8
    (let* ((values (cl-loop for i from 5 to 60
                            collect (/ i 20.0) into values
                            finally return (aio-test--shuffle values)))
           (count (length values))
           (select (aio-make-select))
           (promises (dolist (value values)
                       (aio-select-add select (aio-sleep value value))))
           (last 0.0))
      (dotimes (_ count :done)
        (let ((promise (aio-await (aio-select select))))
          (let ((result (aio-await promise)))
            (should (> result last))
            (setf last result)))))))

(ert-deftest process-sentinel ()
  (aio-with-test 10
    (let ((process (start-process-shell-command "test" nil "exit 0"))
          (sentinel (aio-make-callback)))
      (setf (process-sentinel process) (car sentinel))
      (should (equal "finished\n"
                     (nth 1 (aio-chain (cdr sentinel))))))))

(ert-deftest process-filter ()
  (aio-with-test 10
    (let* ((command
            (if (eq system-type 'windows-nt)
                (mapconcat #'identity
                           '("echo a b c"
                             "waitfor /t 1 x 2>nul"
                             "echo 1 2 3"
                             "waitfor /t 1 x 2>nul")
                           "&")
              "echo a b c; sleep 1; echo 1 2 3; sleep 1"))
           (process (start-process-shell-command "test" nil command))
           (filter (aio-make-callback)))
      (setf (process-filter process) (car filter))
      (should (equal "a b c\n"
                     (nth 1 (aio-chain (cdr filter)))))
      (should (equal "1 2 3\n"
                     (nth 1 (aio-chain (cdr filter))))))))

(ert-deftest sem ()
  (aio-with-test 5
    (let ((n 64)
          (sem (aio-sem 0))
          (promises ())
          (output ()))
      (dotimes (i n)
        ;; Queue up threads on the semaphore
        (push
         (aio-with-async
           (aio-await (aio-sem-wait sem))
           (push i output))
         promises))
      ;; Allow threads to run
      (dotimes (_ n)
        (aio-sem-post sem))
      ;; Wait for all threads to complete (join)
      (aio-await (aio-all promises))
      ;; Check that the threads ran in correct order
      (should (equal (number-sequence 0 63)
                     (nreverse output))))))
