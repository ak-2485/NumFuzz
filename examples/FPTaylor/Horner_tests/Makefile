# Benchmarks
TEST_INPUTS:= Horner2 Horner5 Horner10\
	Horner20 Horner50

.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@printf "*** BENCHMARK: $@ *** \n"
	fptaylor -c ../config.cfg $@.txt

tests: 
	@$(MAKE) > fptaylor_Horner_tests.txt 2>&1

clean : 
	rm fptaylor_Horner_tests.txt 
