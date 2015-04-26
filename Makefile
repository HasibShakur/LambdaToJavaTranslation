all: lam

lam.cmo:	lam.ml
	ocamlc -c lam.ml

lam_main.cmo:	lam.cmo gratr2_ocaml.cmo  lam_main.ml 
	ocamlc -c -I ../../ocaml/ lam_main.ml

gratr2_ocaml.cmo: gratr2_ocaml.ml
	ocamlc -c gratr2_ocaml.ml

lam: lam_main.cmo lam.cmo gratr2_ocaml.cmo 
	ocamlc -o lam lam.cmo gratr2_ocaml.cmo lam_main.cmo 
