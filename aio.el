;;; aio.el --- async/await for Emacs Lisp -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; The main components of this package are `aio-defun' / `aio-lambda'
;; to define async function, and `aio-await' to pause these functions
;; while they wait on asynchronous events. When an asynchronous
;; function is paused, the main thread is not blocked. It is no more
;; or less powerful than callbacks, but is nicer to use.

;; This is implementation is based on Emacs 25 generators, and
;; asynchronous functions are actually iterators in disguise, operated
;; as stackless, asymmetric coroutines.

;;; Code:

(require 'cl-lib)
(require 'generator)

;; Register new error types
(setf (get 'aio-timeout 'error-conditions) '(aio-timeout error)
      (get 'aio-timeout 'error-message) "Timeout was reached")

(defun aio-promise ()
  "Create a new promise object."
  (record 'aio-promise nil ()))

(defsubst aio-promise-p (object)
  "Return non-nil if OBJECT is a promise."
  (and (eq 'aio-promise (type-of object))
       (= 3 (length object))))

(defsubst aio-result (promise)
  "Return the result of PROMISE, or nil if it is unresolved.

Promise results are wrapped in a function. The result must be
called (e.g. `funcall') in order to retrieve the value."
  (unless (aio-promise-p promise)
    (signal 'wrong-type-argument (list 'aio-promise-p promise)))
  (aref promise 1))

(defun aio-listen (promise callback)
  "Add CALLBACK to PROMISE.
If the promise has already been resolved, the callback will be
scheduled for the next event loop turn."
  (let ((result (aio-result promise)))
    (if result
        (run-at-time 0 nil callback result)
      (push callback (aref promise 2)))))

(defun aio-resolve (promise value-function)
  "Resolve this PROMISE with VALUE-FUNCTION.

A promise can only be resolved once, and any further calls to
`aio-resolve' are silently ignored. The VALUE-FUNCTION must be a
function that takes no arguments and either returns the result
value or rethrows a signal."
  (unless (functionp value-function)
    (signal 'wrong-type-argument (list 'functionp value-function)))
  (unless (aio-result promise)
    (let ((callbacks (nreverse (aref promise 2))))
      (setf (aref promise 1) value-function
            (aref promise 2) ())
      (dolist (callback callbacks)
        (run-at-time 0 nil callback value-function)))))

(defun aio--step (iter promise yield-result)
  "Advance ITER to the next promise.

PROMISE is the return promise of the iterator, which was returned
by the originating async function. YIELD-RESULT is the value
function result directly from the previously yielded promise."
  (condition-case error
      (cl-loop for result = (iter-next iter yield-result)
               then (iter-next iter result)
               until (aio-promise-p result)
               finally (aio-listen result
                                   (lambda (value)
                                     (aio--step iter promise value))))
    (iter-end-of-sequence)
    ;; Rethrow in caller
    (error (aio-resolve promise
                        (lambda ()
                          (signal (car error) (cdr error)))))))

(defmacro aio-with-promise (promise &rest body)
  "Evaluate BODY and resolve PROMISE with the result.

If the body signals an error, this error will be stored in the
promise and rethrown in the promise's listeners."
  (declare (indent defun))
  (cl-assert lexical-binding t)
  `(aio-resolve ,promise
                (condition-case error
                    (let ((result (progn ,@body)))
                      (lambda () result))
                  (error (lambda ()
                           (signal (car error) (cdr error)))))))

(defmacro aio-await (expr)
  "If EXPR evaluates to a promise, pause until the promise is resolved.

Pausing an async function does not block Emacs' main thread. If
EXPR doesn't evaluate to a promise, it is returned immediately
and the function is no paused. Since async functions return
promises, async functions can await directly on other async
functions using this macro.

This macro can only be used inside an async function, either
`aio-lambda' or `aio-defun'."
  `(funcall (iter-yield ,expr)))

(defmacro aio-lambda (arglist &rest body)
  "Like `lambda', but defines an async function.

The body of this function may use `aio-await' to wait on
promises. When an async function is called, it immediately
returns a promise that will resolve to the function's return
value, or any uncaught error signal."
  (declare (indent defun))
  (let ((args (make-symbol "args"))
        (promise (make-symbol "promise")))
    `(lambda (&rest ,args)
       (let* ((,promise (aio-promise))
              (iter (apply (iter-lambda ,arglist
                             (aio-with-promise ,promise
                               ,@body))
                           ,args)))
         (prog1 ,promise
           (aio--step iter ,promise nil))))))

(defmacro aio-defun (name arglist &rest body)
  "Like `aio-lambda' but gives the function a name like `defun'."
  (declare (indent defun))
  `(defalias ',name (aio-lambda ,arglist ,@body)))

(defun aio-timeout (promise seconds)
  "Create a promise based on PROMISE with a timeout after SECONDS.

If PROMISE resolves first, the timeout promise resolves to its
value rather than a timeout. When the timeout is reached, the
aio-timeout symbol is signaled for the result.

Note: The original asynchronous task is, of course, not actually
canceled by the timeout, which may matter in some cases. For this
reason it's preferable for the originator of the promise to
support timeouts directly."
  (let ((result (aio-promise))
        (timeout (lambda () (signal 'aio-timeout seconds))))
    (prog1 result
      (aio-listen promise (lambda (value) (aio-resolve result value)))
      (run-at-time seconds nil #'aio-resolve result timeout))))

;; Useful promise-returning functions:

(require 'url)

(defun aio-select (promises)
  "Return a new promise that resolves when any in PROMISES resolves.

The result of the returned promise is the input promise that
completed. Use an additional `aio-await' or `aio-result' to
retrieve its value."
  (let ((result (aio-promise)))
    (prog1 result
      (dolist (promise promises)
        (aio-listen promise (lambda (_)
                              (when result
                                (aio-resolve result (lambda () promise))
                                (setf result nil))))))))

(aio-defun aio-all (promises)
  "Return a promise that resolves when all PROMISES are resolved."
  (dolist (promise promises)
    (aio-await promise)))

(defun aio-catch (promise)
  "Return a new promise that wraps PROMISE but will never signal.

The promise value is a cons where the car is either :success or
:error. For :success, the cdr will be the result value. For
:error, the cdr will be the error data."
  (let ((result (aio-promise)))
    (cl-flet ((callback (value)
                (aio-resolve result
                             (lambda ()
                               (condition-case error
                                   (cons :success (funcall value))
                                 (error (cons :error error)))))))
      (prog1 result
        (aio-listen promise #'callback)))))

(defun aio-sleep (seconds &optional result)
  "Create a promise that is resolved after SECONDS with RESULT.

The result is a value, not a value function, and it will be
automatically wrapped with a value function (see `aio-resolve')."
  (let ((promise (aio-promise)))
    (prog1 promise
      (run-at-time seconds nil
                   (lambda () (aio-resolve promise (lambda () result)))))))

(defun aio-url-retrieve (url &optional silent inhibit-cookies)
  "Wraps `url-retrieve' in a promise.

This function will never directly signal an error. Instead any
errors will be delivered via the returned promise. The promise
result is a cons of (status . buffer). This buffer is a clone of
the buffer created by `url-retrieve' and should be killed by the
caller."
  (let ((promise (aio-promise)))
    (prog1 promise
      (condition-case error
          (url-retrieve url (lambda (status)
                              (let ((value (cons status (clone-buffer))))
                                (aio-resolve promise (lambda () value))))
                        silent inhibit-cookies)
        (error (aio-resolve promise
                            (lambda ()
                              (signal (car error) (cdr error)))))))))

(defun aio-process-sentinel (process)
  "Return a promise representing the sentinel of PROCESS.

This promise resolves to the status string passed to the sentinel
function. It is safe to apply this function multiple times to the
same process."
  (let ((old-promise (process-get process :aio-sentinel)))
    (if old-promise
        old-promise
      (let* ((promise (aio-promise))
             (callback (lambda (process status)
                         (setf (process-sentinel process) nil
                               (process-get process :aio-sentinel) nil)
                         (aio-resolve promise (lambda () status)))))
        (prog1 promise
          (setf (process-sentinel process) callback
                (process-get process :aio-sentinel) promise))))))

(defun aio-process-filter (process)
  "Return a promise representing the filter of PROCESS.

This promise resolves to the process output string passed to the
filter. If the process terminates, this promise resolves to nil.

It is safe to apply this function multiple times to the same
process. In fact, if the last filter promise has been resolved,
it is necessary to use this function to create a promise for the
next chunk of output."
  (let ((old-filter (process-get process :aio-filter)))
    (if old-filter
        old-filter
      (let* ((promise (aio-promise))
             (sentinel (aio-process-sentinel process))
             (callback (lambda (process output)
                         (setf (process-filter process) nil
                               (process-get process :aio-filter) nil)
                         (aio-resolve promise (lambda () output))))
             (finalizer nil))
        (prog1 promise
          ;; Use setf to allow anonymous function to reference itself
          (setf finalizer
                (lambda (_)
                  (if (process-live-p process)
                      ;; Reregister sentinel
                      (aio-listen sentinel finalizer)
                    (funcall callback process nil))))
          ;; Also monitor the process sentinel
          (aio-listen sentinel finalizer)
          (setf (process-filter process) callback
                (process-get process :aio-filter) promise))))))

(provide 'aio)

;;; aio.el ends here
