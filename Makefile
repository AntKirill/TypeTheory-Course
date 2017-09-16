OCAMLC = ocamlc
I = interfaces
R = implementations
T = tests
HW1_TEST_NAME = t1
HW1_REDUCTION_TEST_NAME = t1_reduction
HW2_UNIFY_TEST_NAME = t2_unify
HW2_INFERENCE_TEST_NAME = t2_infer

hw1:
	$(OCAMLC) -I $(R)/ $(I)/hw1.mli $(R)/hw1.ml $(T)/$(HW1_TEST_NAME).ml -o hw1_test

compile_hw1:
	$(OCAMLC) -c $(I)/hw1.mli $(R)/hw1.ml

hw1_reduction: compile_hw1
	$(OCAMLC) -I $(R)/ $(R)/hw1.cmo $(I)/hw1_reduction.mli $(R)/hw1_reduction.ml $(T)/$(HW1_REDUCTION_TEST_NAME).ml -o hw1_red_test

compile_hw1_reduction: compile_hw1
	$(OCAMLC) -I $(R)/ -c $(I)/hw1_reduction.mli $(R)/hw1_reduction.ml	

hw2_unify:
	$(OCAMLC) -I $(R)/ $(I)/hw2_unify.mli $(R)/hw2_unify.ml $(T)/$(HW2_UNIFY_TEST_NAME).ml -o hw2_unify_test

compile_hw2_unify:
	$(OCAMLC) -I $(R)/ -c $(I)/hw2_unify.mli $(R)/hw2_unify.ml

hw2_inference: compile_hw1 compile_hw1_reduction compile_hw2_unify
	$(OCAMLC) -I $(R)/ $(R)/hw1.cmo $(R)/hw1_reduction.cmo $(R)/hw2_unify.cmo $(I)/hw2_inference.mli $(R)/hw2_inference.ml $(T)/$(HW2_INFERENCE_TEST_NAME).ml -o hw2_inference_test

clean:
	rm -f $(I)/*.cmi $(R)/*.cmi $(T)/*.cmi $(R)/*.cmo $(T)/*.cmo *_test