# aio: async/await for Emacs Lisp

`aio` is to Emacs Lisp as [`asyncio`][asyncio] is to Python. This
package builds upon Emacs 25 generators to provide functions that
pause while they wait on asynchronous events. They do not block any
thread while paused.

See also: [An Async / Await Library for Emacs Lisp][post]

Because it uses `record`, this package requires Emacs 26 or later.

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
  (let* ((result (aio-await (aio-url-retrieve url)))
         (contents (with-current-buffer (cdr result)
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

## Utility macros and functions

```el
(aio-cancel promise)
;; Attempt to cancel PROMISE, returning non-nil if successful.

(aio-with-promise promise &rest body) [macro]
;; Evaluate BODY and resolve PROMISE with the result.

(aio-with-async &rest body) [macro]
;; Evaluate BODY asynchronously as if it was inside `aio-lambda'.

(aio-make-callback &key tag once)
;; Return a new callback function and its first promise.

(aio-chain expr) [macro]
;; `aio-await' on EXPR and replace place EXPR with the next promise.
```

The `aio-make-callback` function is useful for callbacks that are
invoked repeatedly, such as process filters and sentinels. The
`aio-chain` macro works in conjunction.

## Awaitable functions

Here are some useful promise-returning — i.e. awaitable — functions
defined by this package.

```el
(aio-sleep seconds &optional result)
;; Return a promise that is resolved after SECONDS with RESULT.

(aio-idle seconds &optional result)
;; Return a promise that is resolved after idle SECONDS with RESULT.

(aio-url-retrieve url &optional silent inhibit-cookies)
;; Wraps `url-retrieve' in a promise.

(aio-select promises)
;; Return a promise that resolves when any in PROMISES resolves.

(aio-all promises)
;; Return a promise that resolves when all PROMISES are resolved."
```


[asyncio]: https://docs.python.org/3/library/asyncio.html
[post]: https://nullprogram.com/blog/2019/03/10/
