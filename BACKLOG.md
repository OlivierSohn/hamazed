
- make 11 packages
https://begriffs.com/posts/2014-10-25-creating-package-hackage.html

  - set ghc options

  - Categories are determined by whatever you put in the Category field.
  You should try to pick existing categories when possible. You can have more than one category, separated by commas.

  -  The hackage-server attempts to build documentation for library packages, but this can fail. Maintainers can generate their own documentation and upload it by using something along the lines of the shell script below (note that the last two commands are the key ones):
#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --builddir="$dir" --for-hackage --haddock-option=--hyperlinked-source
# Starting with cabal 2.0, `--publish` is needed for uploading to non-candidate releases
cabal upload -d $dir/*-docs.tar.gz
