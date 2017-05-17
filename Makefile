js:
	cabal build
	cp index.html book-a-visit.jsexe/
	
populate:
	cabal configure --ghcjs
	cp style.css  book-a-visit.jsexe/

style:
	cp book-a-visit.jsexe/style.css .

compress:
	closure-compiler -O ADVANCED  --externs book-a-visit.jsexe/all.js.externs book-a-visit.jsexe/all.js > book-a-visit.jsexe/all.min.js
	cp index.min.html book-a-visit.jsexe/index.html

upload:
	scp index.min.html book-a-visit.jsexe/all.min.js book-a-visit.jsexe/style.css lambdasistemi.net:public/book-a-visit.jsexe

api-docs:
	cabal haddock --executables
	scp -r dist/doc/html/book-a-visit/book-a-visit/* lambdasistemi.net:public/book-a-visit.jsexe/api
