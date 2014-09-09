index: index/lazyH index/positionH index/wordH

tokenizer:
	gcc tokenizer.c -o tokenizer

sorted: tokenizer
	cat korpus | ./tokenizer | iconv -f iso-8859-1 -t utf-8 | sort -S 90% > sorted

index/lazyH index/positionH index/wordH: sorted
	cat sorted | cabal run

clean:
	rm sorted
	rm index/*