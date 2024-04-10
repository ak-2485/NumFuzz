TESTDIR:= $(shell pwd)/examples
FPTAYLOR:=$(TESTDIR)/FPTaylor
NUMFUZZ:=$(TESTDIR)/NumFuzz
GAPPA:=$(TESTDIR)/Gappa

# Benchmarks
TEST_INPUTS:= hypot x_by_xy one_by_sqrtxx \
		sqrt_add test02_sum8 nonlin1 \
 		test05_nonlin1 verhulst predatorPrey\
		test06_sums4_sum1 test06_sums4_sum2 i4\
		Horner2 Horner2_with_er Horner5\
 		Horner10 Horner20

.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@printf "*** START BENCHMARK: $@ *** \n"
	@printf "*** TOOL: NumFuzz *** \n"
	@dune exec --no-print-directory -- nfuzz \
		$(NUMFUZZ)/$@.fz 
	@printf "*** END NumFuzz *** \n \n" 

	@printf "*** BENCHMARK: $@ *** \n"
	@printf "*** TOOL: FPTaylor *** \n"
	@$(FPTAYLOR)/FPTaylor-0.9.4/fptaylor  -c \
    	$(FPTAYLOR)/config.cfg $(FPTAYLOR)/$@.txt

	@printf "*** END FPTAYLOR *** \n \n" 

	@printf "*** BENCHMARK: $@ *** \n"
	@printf "*** TOOL: Gappa *** \n"
	@bash -c "time gappa $(GAPPA)/$@.g"
	@printf "*** END GAPPA *** \n" 
	@printf "*** END BENCHMARK: $@ *** \n \n"


tests:
	@dune build
	@$(MAKE) --no-print-directory > tests.txt 2>&1
	@./out_table.sh

clean:
	rm tests.txt
	dune clean


