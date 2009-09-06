SBCL = sbcl --script

load: load-sbcl load-ccl

load-sbcl:
	$(SBCL) load-sbcl.lisp

load-ccl:
	ccl -n -Q -b -l load-ccl.lisp

run-test-sbcl:
	$(SBCL) load-test-sbcl.lisp

clean:
	find -name *.fasl -exec rm {} \;
	find -name *.lx32fsl -exec rm {} \;
