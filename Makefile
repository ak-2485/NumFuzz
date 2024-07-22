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

# Mixed-precision benchmarks

MIXED_PREC := Horner2_with_er_mix sqrt_add_mix sums4_sums2 \
		x_by_xy1 x_by_xy2 x_plus_one_by_x

.PHONY: autotest clean tests $(TEST_INPUTS) $(MIXED_PREC)

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

$(MIXED_PREC): 
	@printf "*** START BENCHMARK: $@ *** \n"
	@printf "*** TOOL: NumFuzz *** \n"
	@dune exec --no-print-directory -- nfuzz \
		$(NUMFUZZ)/mixed_precision/$@.fz 
	@printf "*** END NumFuzz *** \n \n" 

	@printf "*** BENCHMARK: $@ *** \n"
	@printf "*** TOOL: FPTaylor *** \n"
	@$(FPTAYLOR)/FPTaylor-0.9.4/fptaylor  -c \
    	$(FPTAYLOR)/config.cfg $(FPTAYLOR)/mixed_precision/$@.txt

	@printf "*** END FPTAYLOR *** \n \n" 

	@printf "*** BENCHMARK: $@ *** \n"
	@printf "*** TOOL: Gappa *** \n"
	@bash -c "time gappa $(GAPPA)/mixed_precision/$@.g"
	@printf "*** END GAPPA *** \n" 
	@printf "*** END BENCHMARK: $@ *** \n \n"

tests_mixed:
	@dune build
	@$(MAKE) --no-print-directory $(MIXED_PREC) > mixed_test_inputs.txt 2>&1
	@./out_table.sh

tests:
	@dune build
	@$(MAKE) --no-print-directory > tests.txt 2>&1
	@./out_table.sh

clean:
	rm tests.txt
	dune clean


