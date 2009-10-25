
all: docs

includes-stuff: 
	cd includes; sbcl < ../make-ref.lisp

docs: includes-stuff
	makeinfo -v --html --css-include=style.css --force elephant.texinfo
	makeinfo -v --html --css-include=style.css --force --no-split elephant.texinfo

pdf: includes-stuff
	texi2pdf --texinfo=@setcontentsaftertitlepage --texinfo=@setshortcontentsaftertitlepage \
		elephant.texinfo
