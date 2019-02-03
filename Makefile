clean:
	rm -f TAGS pom.xml.*

tags: clean
	etags --language=lisp `find . -name '*.clj'`


