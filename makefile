
all:
	cask eval "(progn (require 'jgkamat-website) (jgkamat-publish))"
	cp -r src/* html/

install:
	cask install
