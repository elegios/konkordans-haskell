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
