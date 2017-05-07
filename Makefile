js:
	stack setup --stack-yaml stack.js.yaml && stack build --stack-yaml stack.js.yaml 
	cp index.html book-a-visit.jsexe/
	cp book-a-visit.jsexe/style.css .

gtk:
	stack setup && stack build && stack exec book-a-visit

compress:
	closure-compiler -O ADVANCED  .stack-work/install/x86_64-linux/lts-7.19/ghcjs-0.2.1.9007019_ghc-8.0.1/bin/book-a-visit.jsexe/all.js > .stack-work/install/x86_64-linux/lts-7.19/ghcjs-0.2.1.9007019_ghc-8.0.1/bin/book-a-visit.jsexe/all.min.js
	cp index.min.html book-a-visit.jsexe/index.html

upload:
	scp book-a-visit.jsexe/index.html book-a-visit.jsexe/all.min.js book-a-visit.jsexe/style.css lambdasistemi.net:public/book-a-visit.jsexe

