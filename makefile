index: index/lazyH index/positionH index/wordH

tokenizer:
	gcc tokenizer.c -o tokenizer

sorted: tokenizer
	cat korpus | ./tokenizer | sort -S 90% > sorted

index/lazyH index/positionH index/wordH: sorted
	cat sorted | cabal run build-index

clean:
	rm sorted
	rm index/*

buildkth: tokenizer
	cabal build
	test /tmp/konkordans && rm -r /tmp/konkordans
	mkdir -p /tmp/konkordans/index
	mv dist/build/konkordans-haskell/konkordans-haskell /tmp/konkordans
	mv tokenizer /tmp/konkordans
	ln -s /info/adk14/labb1/korpus /tmp/konkordans/korpus
	cd /tmp/konkordans; export LC_COLLATE=C; cat korpus | ./tokenizer | sort -S 50% | ./konkordans-haskell build-index


test:
	cd /tmp/konkordans; \
	yes | ./konkordans-haskell för | head -n 100; \
	yes | ./konkordans-haskell regering | tail -n 100; \
	yes | ./konkordans-haskell i | tail;

ifeq (run,$(firstword $(MAKECMDGOALS)))
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(RUN_ARGS):;@:)
endif

run:
	cd /tmp/konkordans; ./konkordans-haskell ${RUN_ARGS}
