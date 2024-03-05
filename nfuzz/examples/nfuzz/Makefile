files:= hypot.fz          x_by_xy.fz     one_by_sqrtxx.fz \
				sqrt_add.fz       test02_sum8.fz nonlin1.fz \
 				test05_nonlin1.fz Horner2.fz     Horner2_with_er.fz \
 				Horner5.fz


all:
	@for f in $(files); do echo $${f}; \
	dune exec --no-print-directory -- nfuzz $${f}; echo "\n";\
	done > numfuzz_tests.txt 2>&1

clean:
	rm tests.txt
