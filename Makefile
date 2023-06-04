.PHONY: ghcid_test

ghcid_test:
	stack exec ghcid -- \
		--command="stack ghci --ghc-options='-j -fno-write-ide-info' dns-resolver:lib dns-resolver:test:spec" \
		--test="main"

format:
	find * -name '*.hs' | xargs -P0 fourmolu --no-cabal -i
	hlint .

ghcid:
	stack exec ghcid -- \
		--command="stack ghci --ghc-options='-j -fno-write-ide-info -fno-code' dns-resolver:lib dns-resolver:exe:dns-resolver" \
