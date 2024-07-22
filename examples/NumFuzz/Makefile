# Benchmarks
TEST_INPUTS:= hypot x_by_xy one_by_sqrtxx \
		sqrt_add test02_sum8 nonlin1 \
 		test05_nonlin1 verhulst predatorPrey\
		test06_sums4_sum1 test06_sums4_sum2 i4\
        Horner2 Horner2_with_er Horner5 Horner10\
		Horner20

MIXED_PREC := Horner2_with_er_mix sqrt_add_mix sums4_sums2 \
		x_by_xy1 x_by_xy2 x_plus_one_by_x

.PHONY: autotest clean tests $(TEST_INPUTS) $(MIXED_PREC)

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@echo " " 
	@echo $@ 
	@dune exec --no-print-directory -- nfuzz $@.fz 

tests: 
	@$(MAKE) --no-print-directory > numfuzz_tests.txt 2>&1

clean : 
	rm numfuzz_tests.txt 

$(MIXED_PREC): 
	@echo " " 
	@echo $@ 
	@dune exec --no-print-directory -- nfuzz mixed_precision/$@.fz

# Target to run tests for MIXED_PREC and save output to numfuzz_tests_mixed.txt
tests_mixed: 
	@$(MAKE) --no-print-directory $(MIXED_PREC) > numfuzz_tests_mixed.txt 2>&1

		
