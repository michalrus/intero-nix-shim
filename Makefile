.PHONY: ci autoformat

ci:
	@nix-build

autoformat:
	@nix-shell --pure --run "exec $(MAKE) _autoformat"


#———————————————————————————————————————————————————————————————————————————————


.PHONY: _autoformat

_autoformat: $(shell find . '(' -name '*.hs' -o -name '*.c' ')' -a -not -path '*/.*' -a -not -path './dist/*' -printf 'dist/autoformat/%P_fmt\n')

dist/autoformat/%_fmt: %
	@echo "Formatting $<..."
	@case "$<" in \
			*.hs) hindent --line-length 80 "$<" \
				 && stylish-haskell --inplace "$<" \
						;; \
			*.c) indent -linux "$<" && rm "$<"~ ;; \
		esac && mkdir -p "$(dir $@)" && touch "$@" || true
