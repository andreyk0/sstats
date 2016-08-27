build:
	stack build sstats

build-prof:
	stack build --profile sstats

clean:
	stack clean

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies

.PHONY: build build-prof clean tags sources

