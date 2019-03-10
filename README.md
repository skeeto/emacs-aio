# aio: async/await for Emacs Lisp

`aio` is to Emacs Lisp as [`asyncio`][asyncio] is to Python. This
package builds upon Emacs 25 generators to provide functions that
pause while they wait on asynchronous events. They do not block any
thread while paused.

## Usage

An async function is defined using `aio-defun` or `aio-lambda`. The
body of such functions can use `aio-await` to pause the function and
wait on a given promise. The function continues with the promise's
resolved value when it's ready. The package provides a number of
functions that return promises, and every async function returns a
promise representing its future return value.

For example:

```el
(aio-defun foo (url)
  (aio-await (aio-sleep 3))
  (message "Done sleeping. Now fetching %s" url)
  (let* ((buffer (aio-await (aio-url-retrieve url)))
         (contents (with-current-buffer buffer
                     (prog1 (buffer-string)
                       (kill-buffer)))))
    (message "Result: %s" contents)))
```

If an uncaught signal terminates an asynchronous function, that signal
is captured by its return value promise and propagated into any
function that awaits on that function.

```el
(aio-defun arith-error-example ()
  (/ 0 0))

(aio-defun consumer-example ()
  (condition-case error
      (aio-await (arith-error-example))
    (arith-error (message "Caught %S" error))))

(consumer-example)
;; => #s(aio-promise nil nil)
;; *Messages*: Caught (arith-error)
```

Converting a callback-based function into a promise-returning,
async-friendly function is simple. Create a new promise object with
`aio-promise`, then `aio-resolve` that promise in your callback. To
chain onto an existing promise, use `aio-listen` to attach a new
callback.

## Promise-returning functions

Here are some useful promise-returning — i.e. awaitable — functions
defined by this package.

```el
(aio-sleep seconds &optional result)
;; Return a promise that is resolved after SECONDS with RESULT.

(aio-url-retrieve url &optional silent inhibit-cookies)
;; Wraps `url-retrieve' in a promise.

(aio-process-sentinel process)
;; Return a promise representing the sentinel of PROCESS.

(aio-process-filter process)
;; Return a promise representing the filter of PROCESS.

(aio-select promises)
;; Return a promise that resolves when any in PROMISES resolves.

(aio-all (promises)
;; Return a promise that resolves when all PROMISES are resolved."
```


[asyncio]: https://docs.python.org/3/library/asyncio.html
