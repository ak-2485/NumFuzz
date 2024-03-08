TEST_INPUTS:= hypot.txt x_by_xy.txt one_by_sqrtxx.txt \
		sqrt_add.txt test02_sum8.txt nonlin1.txt \
 		test05_nonlin1.txt Horner2.txt Horner2_with_er.txt \
 		Horner5.txt
.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@echo " " 
	@echo $@ 
	@~/FPTaylor-0.9.3/fptaylor -c config.cfg $@ 

tests: 
	@$(MAKE) --no-print-directory > fptaylor_tests.txt 2>&1
