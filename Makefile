
.PHONY: default build install uninstall utop test clean

default: build

build:
	$(MAKE) -C src build

# Install all libraries and executables
install:
	$(MAKE) -C src install

uninstall:
	$(MAKE) -C src uninstall

# Build and run tests
test:
	$(MAKE) -C src test

