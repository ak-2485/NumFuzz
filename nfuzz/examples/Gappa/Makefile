SHELL := /bin/bash


files:= hypot.g          x_by_xy.g     one_by_sqrtxx.g \
				sqrt_add.g       test02_sum8.g nonlin1.g \
 				test05_nonlin1.g Horner2.g     Horner2_with_er.g \
 				Horner5.g


all: timed test

timed:
	@TIMEFORMAT='%3R'

test:
	@for f in $(files); do echo $${f};\
	time gappa $${f}; echo "\n";\
	done > gappa_tests.txt 2>&1

clean:
	rm gappa_tests.txt
