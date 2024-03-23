# Benchmarks
TEST_INPUTS:= hypot x_by_xy one_by_sqrtxx \
                sqrt_add test02_sum8 nonlin1 \
                test05_nonlin1 verhulst predatorPrey\
                Horner2 Horner2_with_er Horner5

.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@printf "*** BENCHMARK: $@ *** \n"
	@bash -c "time gappa $@.g"

tests: 
	@$(MAKE) --no-print-directory > gappa_tests.txt 2>&1

clean:
	rm gappa_tests.txt

