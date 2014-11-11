# Won't be useful if you aren't Nick...

main: test

deploy: package
	scp crubadan.tgz akira:/www/crubadan/
	ssh akira "cd /www/crubadan; tar xzf crubadan.tgz; rm crubadan.tgz"

test: build
	sudo cp build/* /srv/www/
	firefox http://localhost/

package: build
	cd build; tar czf ../crubadan.tgz ./*

	
build:
	mkdir build
	cabal configure --ghcjs
	cabal build
	cp dist/build/crubadan-web/crubadan-web.jsexe/*.js build/
	cp static/* build/

clean:
	rm -r build
	rm -r dist
	rm crubadan.tgz
