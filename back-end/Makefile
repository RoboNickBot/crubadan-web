DISTNAME= crubadan-web-back
VERSION=  0.1
EXECNAME= crback
INITNAME= $(EXECNAME)-init
DISTDIR= $(DISTNAME)-$(VERSION)
DISTFILE= $(DISTDIR).tar.gz
BINDIR= bin
OTHERMAKE= Makefile.debian

all: package

package: pack
	tar czf $(DISTFILE) $(DISTDIR)

pack: build dist_dir
	cp -r $(BINDIR) $(DISTDIR)/
	cp $(OTHERMAKE) $(DISTDIR)/Makefile
	cp ../README.md $(DISTDIR)/
	cp ../LICENSE   $(DISTDIR)/

dist_dir: clean
	mkdir $(DISTDIR)

build: sandbox shared bin_dir
	cabal install
	rm -r dist
	cp .cabal-sandbox/bin/$(EXECNAME) $(BINDIR)/
	cp $(INITNAME) $(BINDIR)/

shared: sandbox
	rm -r ../shared/dist; exit 0;
	cabal install ../shared
	rm -r ../shared/dist

bin_dir: clean
	mkdir $(BINDIR)

sandbox: clean
	cabal sandbox init

clean:
	rm -r $(BINDIR); rm $(DISTDIR); rm $(DISTFILE); exit 0;
	rm -r .cabal-sandbox; rm cabal.sandbox.config; exit 0;
