files:= add_mul_test.fz Horner2_with_er.fz pow4.fz\
	test02_sum8.fz  cart_to_polar.fz   Horner2.fz\
	Horner5_HO.fz   nonlin1.fz         sqrt_add.fz\
	x_by_xy.fz      Horner2_HO.fz      hypot.fz\
	one_by_sqrtxx.fz


all: 
	for f in $(files); do echo $${f}; \
	dune exec --no-print-directory -- nfuzz $${f}; \
	echo "\n"; done > tests.txt 2>&1 
