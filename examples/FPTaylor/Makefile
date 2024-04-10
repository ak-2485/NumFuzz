TEST_INPUTS:= hypot x_by_xy one_by_sqrtxx \
		sqrt_add test02_sum8 nonlin1 \
 		test05_nonlin1 verhulst predatorPrey\
		test06_sums4_sum1 test06_sums4_sum2 i4\
        Horner2 Horner2_with_er Horner5 Horner10\
		Horner20

.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@printf "*** BENCHMARK: $@ *** \n"
	FPTaylor-0.9.4/fptaylor -c config.cfg $@.txt

tests: 
	@$(MAKE) > fptaylor_tests.txt 2>&1

clean : 
	rm fptaylor_tests.txt 
