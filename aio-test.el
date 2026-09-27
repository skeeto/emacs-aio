;;; aio-tests.el --- async unit test suite for aio -*- lexical-binding: t; -*-

;;; Commentary:

;;  $ emacs -batch -Q -l aio-test.elc -f ert-run-tests-batch

;; Because the tests run as async functions, the test suite cannot be
;; run in batch mode.  The results will be written into a buffer and
;; Emacs will be left running so you can see the results.

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'ert)
(require 'help)
(require 'help-fns)
(require 'rx)
(require 'sort)

(defmacro aio-with-test (timeout &rest body)
  "Run BODY asynchronously but block synchronously until it completes.

If TIMEOUT seconds passes without completion, signal an
`aio-timeout' to cause the test to fail."
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

(defun aio-test--start-process-shell-command (command)
  "Run `start-process-shell-command' ignoring $SHELL on Windows."
  (let ((shell-file-name
         (if (eq system-type 'windows-nt) "cmdproxy" shell-file-name)))
    (start-process-shell-command "aio-test" nil command)))

(ert-deftest process-sentinel ()
  (aio-with-test 10
    (let ((process (aio-test--start-process-shell-command "exit 0"))
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
           (process (aio-test--start-process-shell-command command))
           (filter (aio-make-callback)))
      (setf (process-filter process) (car filter))
      (should (equal "a b c\n"
                     (nth 1 (aio-chain (cdr filter)))))
      (should (equal "1 2 3\n"
                     (nth 1 (aio-chain (cdr filter))))))))

(ert-deftest url-retrieve ()
  "Test that `aio-url-retrieve' does not leak buffers."
  (aio-with-test 10
    (let ((before (length (buffer-list))))
      (dotimes (_ 8)
        (let ((buffer (cdr (aio-await
                            (aio-url-retrieve "data:text/plain,hello")))))
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "hello" nil t)))
          (kill-buffer buffer)))
      (should (= before (length (buffer-list)))))))

(ert-deftest url-retrieve-silent ()
  "Test that `aio-url-retrieve' passes SILENT to `url-retrieve'."
  (aio-with-test 2
    (let ((buffer (cdr (aio-await
                        (aio-url-retrieve "data:text/plain,hello" t t)))))
      (kill-buffer buffer))))

(defmacro aio-test--with-buffers (names &rest body)
  "Bind each of NAMES to a new buffer, evaluate BODY, then kill them."
  (declare (indent 1))
  `(let ,(cl-loop for name in names
                  collect `(,name (generate-new-buffer
                                   ,(format " *aio-test-%s*" name))))
     (unwind-protect
         (progn ,@body)
       ,@(cl-loop for name in names collect `(kill-buffer ,name)))))

(ert-deftest buffer-across-await ()
  "Test that an async function keeps its own current buffer."
  (aio-test--with-buffers (caller mine decoy)
    (let* ((f (aio-lambda ()
                (set-buffer mine)
                (aio-await (aio-sleep 0.01))
                (current-buffer)))
           (promise (with-current-buffer caller
                      (prog1 (funcall f)
                        ;; `set-buffer' must not leak to the caller
                        (should (eq caller (current-buffer)))))))
      ;; Resume from a different buffer
      (with-current-buffer decoy
        (should (eq mine (aio-wait-for promise)))
        (should (eq decoy (current-buffer)))))))

(ert-deftest with-current-buffer-await ()
  "Test `aio-await' inside `with-current-buffer' (#26)."
  (aio-test--with-buffers (caller target decoy)
    (let* ((f (aio-lambda ()
                (list (with-current-buffer target
                        (insert (aio-await (aio-sleep 0.01 "hello")))
                        (current-buffer))
                      (current-buffer))))
           (promise (with-current-buffer caller (funcall f))))
      (with-current-buffer decoy
        (should (equal (list target caller) (aio-wait-for promise))))
      (should (equal "hello" (with-current-buffer target (buffer-string))))
      (should (equal "" (with-current-buffer decoy (buffer-string)))))))

(ert-deftest with-temp-buffer-await ()
  "Test `aio-await' inside `with-temp-buffer'."
  (let* ((temp nil)
         (f (aio-lambda ()
              (with-temp-buffer
                (setf temp (current-buffer))
                (insert "a")
                (aio-await (aio-sleep 0.01))
                (insert "b")
                (buffer-string)))))
    (should (equal "ab" (aio-wait-for (funcall f))))
    (should-not (buffer-live-p temp))))

(ert-deftest save-excursion-await ()
  "Test `aio-await' inside `save-excursion'."
  (aio-test--with-buffers (target other decoy)
    (with-current-buffer target
      (insert "abcdef")
      (goto-char 3))
    (let ((f (aio-lambda ()
               (set-buffer target)
               (save-excursion
                 (goto-char (point-min))
                 (set-buffer other)
                 (aio-await (aio-sleep 0.01))
                 (with-current-buffer target
                   (insert "!")))
               (list (current-buffer) (point)))))
      (with-current-buffer decoy
        ;; The saved point is a marker, so it moves with the insertion
        (should (equal (list target 4) (aio-wait-for (funcall f)))))
      (should (equal "!abcdef" (with-current-buffer target (buffer-string)))))))

(ert-deftest with-current-buffer-await-error ()
  "Test that a signal from `aio-await' unwinds `with-current-buffer'."
  (aio-test--with-buffers (caller target decoy)
    (let* ((f (aio-lambda ()
                (condition-case nil
                    (with-current-buffer target
                      (aio-await (aio-timeout 0.01)))
                  (aio-timeout (current-buffer)))))
           (promise (with-current-buffer caller (funcall f))))
      (with-current-buffer decoy
        (should (eq caller (aio-wait-for promise)))))))

(ert-deftest with-current-buffer-nested ()
  "Test that awaiting another async function preserves the buffer."
  (aio-test--with-buffers (outer inner decoy)
    (let* ((g (aio-lambda ()
                (set-buffer inner)
                (aio-await (aio-sleep 0.01))
                (current-buffer)))
           (f (aio-lambda ()
                (with-current-buffer outer
                  (list (aio-await (funcall g)) (current-buffer))))))
      (with-current-buffer decoy
        (should (equal (list inner outer) (aio-wait-for (funcall f))))))))

(ert-deftest await-killed-buffer ()
  "Test resuming after the current buffer was killed."
  (aio-test--with-buffers (doomed decoy)
    (let* ((lenient (aio-lambda ()
                      (set-buffer doomed)
                      (aio-await (aio-sleep 0.01))
                      (buffer-live-p (current-buffer))))
           (strict (aio-lambda ()
                     (with-current-buffer doomed
                       (aio-await (aio-sleep 0.01))
                       (insert "unreachable"))))
           (lenient-promise (funcall lenient))
           (strict-promise (funcall strict)))
      (kill-buffer doomed)
      (with-current-buffer decoy
        (should (eq t (aio-wait-for lenient-promise)))
        (should-error (aio-wait-for strict-promise))
        (should (equal "" (buffer-string)))))))

(ert-deftest with-async-buffer ()
  "Test that `aio-with-async' starts in the caller's buffer."
  (aio-test--with-buffers (caller decoy)
    (let ((promise (with-current-buffer caller
                     (aio-with-async (current-buffer)))))
      (with-current-buffer decoy
        (should (eq caller (aio-wait-for promise)))))))

(ert-deftest rewrite ()
  "Test that only pausing forms are rewritten."
  (dolist (form '((save-excursion (foo))
                  (save-current-buffer (foo))
                  '(save-excursion (iter-yield x))
                  #'(lambda () (save-excursion (iter-yield x)))
                  (a . b)))
    (should (equal form (aio--rewrite form nil))))
  (should-not (eq 'save-excursion
                  (car (aio--rewrite '(save-excursion (iter-yield x)) nil)))))

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

(aio-defun aio-test-fun (foo &optional bar)
  "Reticulate the splines."
  (declare (obsolete nil nil))
  (interactive "sFoo: ")
  (list foo bar))

(ert-deftest aio-defun ()
  "Test that declarations and ‘interactive’ forms in ‘aio-defun’ work."
  (should (commandp 'aio-test-fun))
  (should (equal (interactive-form 'aio-test-fun) '(interactive "sFoo: ")))
  (should (equal (cdr (help-split-fundoc (documentation 'aio-test-fun) 'aio-test-fun))
                 "Reticulate the splines."))
  (should (equal (gethash (indirect-function 'aio-test-fun) advertised-signature-table)
                 '(foo &optional bar)))
  (should (get 'aio-test-fun 'byte-obsolete-info)))
;;; aio-test.el ends here
