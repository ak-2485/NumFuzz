# Benchmarks
TEST_INPUTS:= pythagorean_sum squareRoot3\
	squareRoot3Invalid hammarling_distance

.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@echo " " 
	@echo $@ 
	@dune exec --no-print-directory -- nfuzz $@.fz 

tests: 
	@$(MAKE) --no-print-directory > conditional_tests.txt 2>&1

clean : 
	rm conditional_tests.txt 

		
