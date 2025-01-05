COQPATHFILE=$(wildcard _CoqPath)

Makefile.coq: _CoqProject 
	coq_makefile -f $< -o$@ 

clean: 
	rm *.vo *.vok *.glob *.vos

all :  
	coq_makefile -f _CoqProject -o Makefile.coq 
	$(MAKE) -f Makefile.coq 
       	

