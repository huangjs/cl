all:
	./make-desktop.sh

clean:
	rm -f *~ *.fasl *.core clim-desktop.tar.bz2

tarball: clean
	cd ..; tar cjvf clim-desktop.tar.bz2  clim-desktop; mv clim-desktop.tar.bz2 clim-desktop
	scp clim-desktop.tar.bz2 dholman@common-lisp.net:/project/clim-desktop/public_html/
