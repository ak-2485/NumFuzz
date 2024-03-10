SHELL=/bin/bash
TESTDIR:= $(shell pwd)/examples
FPTAYLOR:=$(TESTDIR)/FPTaylor
NUMFUZZ:=$(TESTDIR)/NumFuzz
GAPPA:=$(TESTDIR)/Gappa

# Benchmarks
TEST_INPUTS:= hypot x_by_xy one_by_sqrtxx \
		sqrt_add test02_sum8 nonlin1 \
 		test05_nonlin1 Horner2 Horner2_with_er \
 		Horner5

.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@printf "*** BENCHMARK: $@ *** \n"
	@printf "*** TOOL: NumFuzz *** \n"
	@dune exec --no-print-directory -- nfuzz \
		$(NUMFUZZ)/$@.fz 
	@printf "*** END NumFuzz *** \n \n" 

	@printf "*** BENCHMARK: $@ *** \n"
	@printf "*** TOOL: FPTaylor *** \n"
	@$(FPTAYLOR)/FPTaylor-0.9.3/fptaylor  -c \
		$(FPTAYLOR)/config.cfg $(FPTAYLOR)/$@.txt
	@printf "*** END FPTAYLOR *** \n \n" 

	@printf "*** BENCHMARK: $@ *** \n"
	@printf "*** TOOL: Gappa *** \n"
	@time gappa $(GAPPA)/$@.g 
	@printf "*** END GAPPA *** \n \n" 

tests:
	@dune build
	@$(MAKE) TIMEFORMAT='real time (s): %3R' \
	--no-print-directory > tests.txt 2>&1

clean:
	rm tests.txt
	dune clean


