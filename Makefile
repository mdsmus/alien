SBCL = sbcl --script
CCL = ccl -n -b -l

load: load-sbcl load-ccl

load-sbcl:
	$(SBCL) load-sbcl.lisp

load-ccl:
	$(CCL) load-ccl.lisp

test-sbcl:
	$(SBCL) load-test-sbcl.lisp

test-ccl:
	$(CCL) load-test-ccl.lisp

clean:
	find -name *.fasl -exec rm {} \;
	find -name *.lx32fsl -exec rm {} \;
