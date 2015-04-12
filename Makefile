prog = ghc
path = /usr/local/bin/ghc-7.8.4

all: deps build install docs licenses

tags:
	hasktags --etags --output='TAGS' *

build:
	cabal configure --with-$(prog)=$(path) && cabal build --with-$(prog)=$(path)

install:
	cabal install -w $(path)

deps:
	cabal install --only-dependencies --enable-documentation -w $(path)

test:
	cabal test -w $(path)

docs:
	cabal haddock --executables

licenses:
	rm -f DEPENDENCY-LICENSES.org && cabal-dependency-licenses | sed 's/#/*/' > DEPENDENCY-LICENSES.org
