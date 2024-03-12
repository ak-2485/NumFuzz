TEST_INPUTS := hypot.fz x_by_xy.fz one_by_sqrtxx.fz \
		sqrt_add.fz test02_sum8.fz nonlin1.fz \
 		test05_nonlin1.fz Horner2.fz Horner2_with_er.fz \
 		Horner5.fz

.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@echo " " 
	@echo $@ 
	@dune exec --no-print-directory -- nfuzz $@ 

tests: 
	@$(MAKE) --no-print-directory > numfuzz_tests.txt 2>&1

clean : 
	rm numfuzz_tests.txt 

		