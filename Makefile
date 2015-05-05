all:
	ocp-build init
	ocp-build

opam-deps:
	opam install ocp-build
