files:= hypot.txt x_by_xy.txt one_by_sqrtxx.txt \
		sqrt_add.txt test02_sum8.txt nonlin1.txt \
 		test05_nonlin1.txt Horner2.txt Horner2_with_er.txt \
 		Horner5.txt

all:
	@for f in $(files); do echo $${f};\
	../../FPTaylor-0.9.3/fptaylor -c config.cfg $${f}; echo "\n";\
	done > fptaylor_tests.txt 2>&1

clean:
	rm fptaylor_tests.txt
