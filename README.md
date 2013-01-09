Concraft
========

Concraft is a morphosyntactic disambiguation tool and library based on
constrained conditional random fields [1].  The tool is currently adapted
to the Polish language and resources, but the library should be applicable
to at least other highly inflected languages.

Building Concraft
=================

You will need the [Glasgow Haskell Compiler](http://www.haskell.org/ghc/)
and the [Cabal](http://www.haskell.org/cabal/) tool to build Concraft.
The easiest way to get both GHC and Cabal is to install the latest
[Haskell Platform](http://www.haskell.org/platform/).

To install Concraft from the official [Hackage](http://hackage.haskell.org/package/concraft)
repository just run:

    cabal install concraft

If you want to update Concraft to a newer version you should update the package list first:

    cabal update 
    cabal install concraft

To install the latest development version from github just run

    cabal install

from the `concraft` toplevel directory.

Data format
===========

The current version of Concraft works on a simple `plain` text format which is one of the
formats supported by the [Corpus2](http://nlp.pwr.wroc.pl/redmine/projects/corpus2/wiki)
tools.  You can use the [Maca](http://nlp.pwr.wroc.pl/redmine/projects/libpltagger/wiki)
tool for preliminary morphosyntactic analysis:

    maca-analyse morfeusz-nkjp-official -o plain < intput.txt > output.plain

Training
========

If you have the training material with disambiguation annotations you can train
the Concraft model yourself.

    concraft train config/nkjp-tagset.cfg train.plain -e eval.plain --igntag=ign -o model.bin

The first program argument is a specification of the NKJP morphosyntactic tagset.
It can be found in the `config` toplevel directory.  Run `concraft train --help`
to learn more about the program arguments and possible training options.

Remember that you can supply the program with additional
[runtime system options](http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html).
For example, to train the model using four threads, run:

    concraft train config/nkjp-tagset.cfg train.plain -e eval.plain --igntag=ign -o model.bin +RTS -N4

Disambiguation
==============

Once you have the model you can use the following command to disambiguate
the `plain` text file.

    concraft disamb model.bin < input.plain > output.plain

Run `concraft disamb --help` to learn more about the possible disambiguation options.
Note, that Concraft doesn't disambiguate over base forms.  As a result, there may
be more than one disamb annotation assigned to one lexeme in the output file.

*Remember to use the same preprocessing pipeline (segmentation + analysis)
for both training and disambiguation.  Inconsistencies between training
material and input data may severely harm the quality of disambiguation.*

References
==========

[1] Jakub Waszczuk, "Harnessing the CRF complexity with domain-specific constraints.
The case of morphosyntactic tagging of a highly inflected language.",
in Proceedings of COLING 2012, Mumbai, India.
