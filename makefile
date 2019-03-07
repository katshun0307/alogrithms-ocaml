mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))

builder = corebuild

dijkstra: dijkstra.ml
	$(builder) $@.native

kmeans: kmeans.ml
	dune build $@.exe
	cd $(mkfile_dir)
	./_build/default/kmeans.exe

clean:
	rm *.native
	dune 