TEST_INPUTS := Horner50 Horner75 Horner100 \
	poly50 serial_sum mat_mul_4 mat_mul_16 mat_mul_64

.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@echo " " 
	@echo $@ 
	@dune exec --no-print-directory -- nfuzz $@.fz 

tests: 
	@$(MAKE) --no-print-directory > large_tests.txt 2>&1

clean : 
	rm large_tests.txt 

		
