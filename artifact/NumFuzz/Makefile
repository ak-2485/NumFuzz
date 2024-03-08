SHELL = /bin/bash
TIMEFORMAT='%3R'
# Benchmarks
TEST_INPUTS:= hypot x_by_xy one_by_sqrtxx \
		sqrt_add test02_sum8 nonlin1 \
 		test05_nonlin1 Horner2 Horner2_with_er \
 		Horner5
.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@echo "--------" 
	@echo "BENCHMARK:" $@
	@echo "--------" 
	@echo "TOOL: Gappa"
	@time gappa examples/Gappa/$@.g
	@echo " " 
	@echo "TOOL: NumFuzz"
	@dune exec --no-print-directory -- nfuzz examples/NumFuzz/$@.fz 
	@echo " " 
	@echo "TOOL: FPTaylor"
	@~/FPTaylor-0.9.3/fptaylor -c config.cfg $@.txt
	@echo " " 

tests:
	@dune build
	@$(MAKE) TIMEFORMAT='%3R' --no-print-directory > tests.txt 2>&1

clean:
	rm tests.txt
	dune clean


