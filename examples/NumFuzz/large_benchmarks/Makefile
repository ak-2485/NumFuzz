TEST_INPUTS := Horner10.fz Horner20.fz Horner50.fz Horner75.fz Horner100.fz

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

		