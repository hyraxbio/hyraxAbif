package = hyraxAbif
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

all: build test lint
cabal-all: cabal-build cabal-test lint

setup:
	$(stack) setup
	$(stack) build --dependencies-only --test --no-run-tests
	$(stack) install hlint weeder

lint:
	hlint .
	weeder .

check-nightly:
	$(stack) setup --resolver nightly
	$(stack) build --resolver nightly --pedantic --test

cabal-build:
	cabal new-build

build:
	$(stack) build $(package) --no-run-tests

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

run:
	$(stack) build --fast && $(stack) exec -- $(package)-exe

cabal-run:
	cabal new-run $(package)-exe

ghci:
	$(stack) ghci $(package):lib

test:
	$(stack) test $(package)

cabal-test:
	cabal new-test

test-ghci:
	$(stack) ghci $(package):test:$(package)-test

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(package):exe:$(package)-exe"

ghcid-run:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(package):exe:$(package)-exe" --test=":main debug" -W

ghcid-test:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind' $(package):test:$(package)-test" --test="main" -W

dev-deps:
	stack install ghcid


.PHONY : build build-dirty run install ghci test test-ghci ghcid dev-deps lint check-nightly setup
