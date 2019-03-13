.POSIX:
EMACS = emacs

compile: aio.elc aio-test.elc

aio.elc: aio.el
aio-test.elc: aio-test.el aio.elc

clean:
	rm -f aio.elc aio-test.elc

check: aio-test.elc
	$(EMACS) -Q -batch -L . -l aio-test.elc -f aio-run-tests

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<


