
- make 11 packages
https://begriffs.com/posts/2014-10-25-creating-package-hackage.html

  - Chose a namespace

  - Categories are determined by whatever you put in the Category field.
  You should try to pick existing categories when possible. You can have more than one category, separated by commas.

  - try stack sdist / stack upload on prelude

  - about the hyperliked-source option
  https://www.reddit.com/r/haskell/comments/6o4rnb/psa_regarding_cabal_haddock/

  - a script to build dock
  https://gist.github.com/stbuehler/7068764

  -  The hackage-server attempts to build documentation for library packages, but this can fail. Maintainers can generate their own documentation and upload it by using something along the lines of the shell script below (note that the last two commands are the key ones):
#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --builddir="$dir" --for-hackage --haddock-option=--hyperlinked-source
# Starting with cabal 2.0, `--publish` is needed for uploading to non-candidate releases
cabal upload -d $dir/*-docs.tar.gz




--html-location=http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html
--for-hackage
--hoogle

for stackage, which ghc versions should I support? https://www.fpcomplete.com/blog/2014/05/stackage-server
