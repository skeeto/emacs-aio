;;; aio-tests.el --- async unit test suite for aio -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs -Q -nw -L . -l aio-test.elc -f aio-run-tests

;; Because the tests run as async functions, the test suite cannot be
;; run in batch mode. The results will be written into a buffer and
;; Emacs will be left running so you can see the results.

;;; Code:

(require 'aio)
(require 'cl-lib)

(defvar aio-tests ())

(defvar aio-test-total nil)
(defvar aio-test-failures nil)

(defun aio-run-tests ()
  (setf aio-test-total 0
        aio-test-failures 0)
  (let* ((buffer (get-buffer-create "*aio-results*"))
         (promises
          (cl-loop for (name . test) in (reverse aio-tests)
                   for promise = (funcall test)
                   for cb =
                   (let ((test-name name))
                     (lambda (value)
                       (insert (format "%S: %S\n" test-name (funcall value)))))
                   collect promise
                   do (aio-listen (aio-catch promise) cb)))
         (done (aio-all promises)))
    (switch-to-buffer buffer)
    (erase-buffer)
    (aio-listen done (lambda (_)
                       (with-current-buffer buffer
                         (insert "*** aio-run-tests complete ***\n")
                         (insert (format "%d / %d PASS\n"
                                         (- aio-test-total aio-test-failures)
                                         aio-test-total)))))))

(defun aio--should (result value-a value-b expr-a expr-b)
  (cl-incf aio-test-total)
  (unless result
    (cl-incf aio-test-failures)
    (insert (format "FAIL:\n%S\n= %S\n%S\n= %S\n"
                    expr-a value-a expr-b value-b))))

(defmacro aio-should (cmp expr-a expr-b)
  (let ((value-a (make-symbol "a"))
        (value-b (make-symbol "b")))
    `(let ((,value-a ,expr-a)
           (,value-b ,expr-b))
       (aio--should (,cmp ,value-a ,value-b)
                    ,value-a ,value-b ',expr-a ',expr-b))))

(defmacro aio-deftest (name _ &rest body)
  (declare (indent defun))
  `(push (cons ',name (aio-lambda () ,@body)) aio-tests))

;; Tests:

(aio-deftest sleep ()
  (let ((start (float-time)))
    (dotimes (i 3)
      (aio-should eql i (aio-await (aio-sleep 0.5 i))))
    (aio-should > (- (float-time) start) 1.4)))

(aio-deftest chain ()
  (let ((sub (aio-lambda (result) (aio-await (aio-sleep .1 result)))))
    (aio-should eq :a (aio-await (funcall sub :a)))
    (aio-should eq :b (aio-await (funcall sub :b)))))

(aio-deftest timeout ()
  (let ((sleep (aio-sleep 1.0 t))
        (timeout (aio-timeout 0.5))
        (select (aio-make-select)))
    (aio-select-add select sleep)
    (aio-select-add select timeout)
    (aio-should equal
                '(:error aio-timeout . 0.5)
                (aio-await (aio-catch (aio-await (aio-select select))))))
  (let ((sleep (aio-sleep 0.1 t))
        (timeout (aio-timeout 0.5))
        (select (aio-make-select)))
    (aio-select-add select sleep)
    (aio-select-add select timeout)
    (aio-should equal
                '(:success . t)
                (aio-await (aio-catch (aio-await (aio-select select)))))))

(defun aio-test--shuffle (values)
  "Return a shuffled copy of VALUES."
  (let ((v (vconcat values)))
    (cl-loop for i from (1- (length v)) downto 1
             for j = (cl-random (+ i 1))
             do (cl-rotatef (aref v i) (aref v j))
             finally return (append v nil))))

(aio-deftest sleep-sort ()
  (let* ((values (cl-loop for i from 5 to 60
                          collect (/ i 20.0) into values
                          finally return (aio-test--shuffle values)))
         (count (length values))
         (select (aio-make-select))
         (promises (dolist (value values)
                     (aio-select-add select (aio-sleep value value))))
         (last 0.0))
    (dotimes (_ count)
      (let ((promise (aio-await (aio-select select))))
        (let ((result (aio-await promise)))
          (aio-should > result last)
          (setf last result))))))

(aio-deftest process-sentinel ()
  (let ((process (start-process-shell-command "test" nil "exit 0"))
        (sentinel (aio-make-callback)))
    (setf (process-sentinel process) (car sentinel))
    (aio-should equal
                "finished\n"
                (nth 1 (aio-chain (cdr sentinel))))))

(aio-deftest process-filter ()
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
    (aio-should equal
                "a b c\n"
                (nth 1 (aio-chain (cdr filter))))
    (aio-should equal
                "1 2 3\n"
                (nth 1 (aio-chain (cdr filter))))))
