Concraft
========

Concraft is a morphosyntactic disambiguation tool and library based on
constrained conditional random fields [1].  The tool is currently adapted
to the Polish language and resources, but the library should be applicable
to at least other highly inflected languages.

<!---
## Library contents

The concraft library contains...
-->

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

References
==========

[1] Jakub Waszczuk, "Harnessing the CRF complexity with domain-specific constraints.
The case of morphosyntactic tagging of a highly inflected language.",
in Proceedings of COLING 2012, Mumbai, India.
