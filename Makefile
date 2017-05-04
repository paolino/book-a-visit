js:
	stack setup --stack-yaml stack.js.yaml && stack build --stack-yaml stack.js.yaml && stack exec book-a-visit --stack-yaml stack.js.yaml

gtk:
	stack setup && stack build && stack exec book-a-visit
