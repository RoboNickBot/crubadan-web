DISTNAME= crubadan-web-front
VERSION= 0.1.1.1
DISTDIR= $(DISTNAME)-$(VERSION)
DISTFILE= $(DISTDIR).tar.gz
WEBDIR= www
JSNAME= writing_systems.js

JQUERY= GHCJS-JQuery
JQUERYLINK= https://github.com/GHCJS/$(JQUERY).git

all: package


package: build dist_dir
	cp ../README.md $(DISTDIR)/
	cp ../LICENSE $(DISTDIR)/
	tar czf $(DISTFILE) $(DISTDIR)

build: sandbox shared_js jquery web_dir
	cabal install --ghcjs
	rm -r ./dist
	cp .cabal-sandbox/bin/$(DISTNAME).jsexe/all.js $(DISTDIR)/$(WEBDIR)/$(JSNAME)
	cp static/* $(DISTDIR)/$(WEBDIR)/


jquery: sandbox
	git clone $(JQUERYLINK)
	cabal install --ghcjs ./$(JQUERY)
	rm -r $(JQUERY)/dist

shared_js: sandbox
	rm -r ../shared/dist; exit 0;
	cabal install --ghcjs ../shared
	rm -r ../shared/dist

web_dir: clean dist_dir
	mkdir $(DISTDIR)/$(WEBDIR)

dist_dir: clean
	mkdir $(DISTDIR)

sandbox: clean
	cabal sandbox init

clean:
	rm -r $(WEBDIR); rm -r $(DISTDIR); rm $(DISTFILE); rm -rf $(JQUERY); rm -r .cabal-sandbox; rm cabal.sandbox.config; exit 0;
