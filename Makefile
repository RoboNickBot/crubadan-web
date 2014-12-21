# Won't be useful if you aren't Nick...

DISTNAME= crubadan-web
DISTFILE= $(DISTNAME).tgz
WEBDIR= www

all: local

akira: package
	scp $(DISTFILE) akira:
	ssh akira "tar xzf $(DISTFILE); rm $(DISTFILE); cp $(WEBDIR)/* /srv/www/crubadan/; rm -r $(WEBDIR)"

local: build
	cp $(WEBDIR)/* /srv/www/crubadan/

package: build
	tar czf $(DISTFILE) $(WEBDIR)

build: frontend backend
	cp static/* $(WEBDIR)/

frontend: www_dir sandbox shared_js
	cabal install --ghcjs ../ghcjs-zone/GHCJS-JQuery
	cabal install --ghcjs ./front-end
	rm -r ./front-end/dist

backend: www_dir sandbox shared
	cabal install ./back-end
	rm -r ./back-end/dist

shared: sandbox
	cabal install ./shared
	rm -r ./shared/dist

shared_js: sandbox
	cabal install --ghcjs ./shared
	rm -r ./shared/dist

www_dir: clean
	mkdir $(WEBDIR)

sandbox: clean
	cabal sandbox init

clean:
	rm -r $(WEBDIR); rm $(DISTFILE); rm -r .cabal-sandbox; rm cabal.sandbox.config; exit 0;
