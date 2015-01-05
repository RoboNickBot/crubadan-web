# Won't be useful if you aren't Nick...

DISTNAME= crubadan-web
BACKENDNAME= crubadan-web-back
DISTFILE= $(DISTNAME).tgz
INSTALLPATH= /srv/www/crubadan/
WEBDIR= www
BINDIR= bin

all: local

akira: package
	scp $(DISTFILE) akira:
	ssh akira "tar xzf $(DISTFILE); rm $(DISTFILE); cp $(WEBDIR)/* $(INSTALLPATH); rm -r $(WEBDIR); cp $(BINDIR)/* ./; rm -r $(BINDIR)"

local: build
	cp $(WEBDIR)/* $(INSTALLPATH)

package: build
	tar czf $(DISTFILE) $(WEBDIR) $(BINDIR)/

build: frontend backend web_dir bin_dir
	cp static/* $(WEBDIR)/
	cp .cabal-sandbox/bin/$(DISTNAME).jsexe/all.js $(WEBDIR)/
	cp .cabal-sandbox/bin/$(BACKENDNAME) $(BINDIR)/

frontend: sandbox shared_js
	cabal install --ghcjs ../ghcjs-zone/GHCJS-JQuery
	cabal install --ghcjs ./front-end
	rm -r ./front-end/dist

backend: sandbox shared
	cabal install ./back-end
	rm -r ./back-end/dist

shared: sandbox
	cabal install ./shared
	rm -r ./shared/dist

shared_js: sandbox
	cabal install --ghcjs ./shared
	rm -r ./shared/dist

web_dir: clean
	mkdir $(WEBDIR)

bin_dir: clean
	mkdir $(BINDIR)

sandbox: clean
	cabal sandbox init

clean:
	rm -r $(WEBDIR); rm -r $(BINDIR); rm $(DISTFILE); rm -r .cabal-sandbox; rm cabal.sandbox.config; exit 0;
