#!/bin/bash
echo $1
echo "Do you want me to grab all of the clnet packages for:"
echo "Flexichain, clx, split-sequence, cl-ppcre, climacs, mcclim, closure, beirc?"
echo "y/N"
read -n 1 answer

case $answer in
    [yY]) echo "";
	echo "sure thing";
        wget http://www.niksula.hut.fi/~tsiivola/files/clnet
	chmod +x ./clnet
	./clnet gsharp Flexichain;
	./clnet climacs climacs;
	./clnet mcclim mcclim;
	./clnet closure closure;
	./clnet beirc beirc
	wget http://bl0rg.net/~mgr/flux/tab-layout_2005-09-19_02-52+0200.tar.bz2
	tar zxvf tab-layout_2005-09-19_02-52+0200.tar.bz2
	mv tab-layout ~/.sbcl/site/
	mv Flexichain ~/.sbcl/site/
	mv climacs ~/.sbcl/site/
	mv mcclim ~/.sbcl/site/
	mv closure ~/.sbcl/site/
	mv beirc ~/.sbcl/site/
	ln -s ~/.sbcl/site/Flexichain/Flexichain.asd ~/.sbcl/systems/Flexichain.asd
	ln -s ~/.sbcl/site/climacs/climacs.asd ~/.sbcl/systems/climacs.asd
	ln -s ~/.sbcl/site/mcclim/mcclim.asd ~/.sbcl/systems/mcclim.asd
	ln -s ~/.sbcl/site/closure/closure.asd ~/.sbcl/systems/closure.asd
	ln -s ~/.sbcl/site/beirc/beirc.asd ~/.sbcl/systems/beirc.asd

        echo "Now, for the asdf-installs."
        sbcl --eval "(progn (require 'asdf) (require 'asdf-install) (asdf-install:install :clx) (asdf-install:install :split-sequence)  (asdf-install:install :cl-ppcre))"
	ln -s ~/.sbcl/site/foo/foo.asd ~/.sbcl/systems/foo.asd
;;
    

    *) echo "";
	echo "All right, assuming you have those asdf-available";;
esac

#http://www.niksula.hut.fi/~tsiivola/files/clnet
#http://www.niksula.hut.fi/~tsiivola/files/sfnet
