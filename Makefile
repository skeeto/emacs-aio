.POSIX:
EMACS = emacs
EASK = eask

ci: clean build compile check

clean:
	$(EASK) clean elc

build:
	$(EASK) package
	$(EASK) install

compile:
	$(EASK) compile

check:
	$(EASK) test ert aio-test.el
