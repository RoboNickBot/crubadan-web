# Won't be useful if you aren't Nick...


akira: package
	scp crubadan.tgz akira:
	ssh akira "tar xzf crubadan.tgz; rm crubadan.tgz; cp www/* /www/crubadan/; rm -r www"

local: build
	cp www/* /srv/www/crubadan/

package: build
	tar czf crubadan-web.tgz www

build: frontend backend
	cp static/* www/

frontend: www_dir sandbox
	cabal install --ghcjs ./front-end

backend: www_dir sandbox
	cabal install ./back-end

shared: sandbox
	cabal install ./shared
	rm -r ./shared/dist

shared_js: sandbox
	cabal install --ghcjs ./shared
	rm -r ./shared/dist

www_dir: clean
	mkdir www

sandbox: clean
	cabal sandbox init

clean:
	rm -r www; rm crubadan.tgz; exit 0;
