OCAMLC = ocamlc
I = interfaces
R = realisations
T = tests

hw1:
	$(OCAMLC) -I $(R)/ $(I)/hw1.mli $(R)/hw1.ml $(T)/t1.ml -o hw1_test

compile_hw1:
	$(OCAMLC) -c $(I)/hw1.mli $(R)/hw1.ml

hw1_red: compile_hw1
	$(OCAMLC) -I $(R)/ $(R)/hw1.cmo $(I)/hw1_reduction.mli $(R)/hw1_reduction.ml $(T)/t1_reduction.ml -o hw1_red_test

clean:
	rm -f $(I)/*.cmi $(R)/*.cmi $(T)/*.cmi $(R)/*.cmo $(T)/*.cmo *_test