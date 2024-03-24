TEST_INPUTS := Horner50.fz Horner75.fz Horner100.fz \
	poly50 serial_sum mat_mul_4 mat_mul_16 mat_mul_64\
	mat_mul_128

.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@echo " " 
	@echo $@ 
	@dune exec --no-print-directory -- nfuzz $@ 

tests: 
	@$(MAKE) --no-print-directory > numfuzz_Horner_tests.txt 2>&1

clean : 
	rm numfuzz_Horner_tests.txt 

		