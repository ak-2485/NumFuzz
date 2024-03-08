SHELL = /bin/bash
TIMEFORMAT='%3R' 

TEST_INPUTS:= hypot.g x_by_xy.g one_by_sqrtxx.g \
		sqrt_add.g test02_sum8.g nonlin1.g \
 		test05_nonlin1.g Horner2.g Horner2_with_er.g \
 		Horner5.g
.PHONY: autotest clean tests $(TEST_INPUTS) 

autotest: $(TEST_INPUTS) 

$(TEST_INPUTS): 
	@echo " " 
	@echo $@
	time gappa $@

tests: 
	@$(MAKE) TIMEFORMAT='%3R' --no-print-directory > gappa_tests.txt 2>&1

clean:
	rm gappa_tests.txt

