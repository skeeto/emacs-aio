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

;; Tests:

(ert-deftest sleep ()
  (aio-wait-for
   (aio-with-async
     (let ((start (float-time)))
       (dotimes (i 3)
         (should (eql i(aio-await (aio-sleep 0.5 i)))))
       (should (> (- (float-time) start)
                  1.4))))))

(ert-deftest repeat ()
  (aio-wait-for
   (aio-with-async
     (let ((sub (aio-lambda (result) (aio-await (aio-sleep .1 result)))))
       (should (eq :a (aio-await (funcall sub :a))))
       (should (eq :b (aio-await (funcall sub :b))))))))

(ert-deftest timeout ()
  (aio-wait-for
   (aio-with-async
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
                        (aio-await (aio-catch winner)))))))))

(defun aio-test--shuffle (values)
  "Return a shuffled copy of VALUES."
  (let ((v (vconcat values)))
    (cl-loop for i from (1- (length v)) downto 1
             for j = (cl-random (+ i 1))
             do (cl-rotatef (aref v i) (aref v j))
             finally return (append v nil))))

(ert-deftest sleep-sort ()
  (aio-wait-for
   (aio-with-async
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
             (setf last result))))))))

(ert-deftest process-sentinel ()
  (aio-wait-for
   (aio-with-async
     (let ((process (start-process-shell-command "test" nil "exit 0"))
           (sentinel (aio-make-callback)))
       (setf (process-sentinel process) (car sentinel))
       (should (equal "finished\n"
                      (nth 1 (aio-chain (cdr sentinel)))))))))

(ert-deftest process-filter ()
  (aio-wait-for
   (aio-with-async
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
                      (nth 1 (aio-chain (cdr filter)))))))))
