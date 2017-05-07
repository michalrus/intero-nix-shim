.PHONY: local ci build lint autoformat autoformat-check

local: autoformat       build lint
ci:    autoformat-check build lint

build:
	@nix-build -j$$(nproc)
lint:
	@nix-shell --pure --run 'exec hlint .'
autoformat:
	@nix-shell --pure --run "exec $(MAKE) -j$$(nproc) _autoformat"
autoformat-check: autoformat
	@nix-shell --pure --run 'status=$$(git status --porcelain | grep -v "^M ") ; [ -z "$$status" ] || { printf >&2 "%s\n%s\n" "fatal: some files are unformatted (or repo unclean):" "$$status" ; exit 1 ; }'


#———————————————————————————————————————————————————————————————————————————————


.PHONY: _autoformat

_autoformat: $(shell find . '(' -name '*.hs' -o -name '*.c' ')' -a -not -path '*/.*' -a -not -path './dist/*' -printf 'dist/autoformat/%P_fmt\n')

dist/autoformat/%_fmt: %
	@echo "Formatting $<..."
	@case "$<" in \
		*.hs) hindent --line-length 80 "$<" && stylish-haskell --inplace "$<" ;; \
		*.c) indent -linux "$<" && rm "$<"~ ;; \
	esac && mkdir -p "$(dir $@)" && touch "$@"
