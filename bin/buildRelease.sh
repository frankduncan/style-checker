#!/bin/bash

version=$(sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options --eval '(format t "~A" (asdf:component-version (asdf:find-system :style-checker)))' --eval "(quit)")

echo -n "Building version $version, hit enter to continue"
read

mkdir style-checker_$version
cp -ap src/main/* style-checker_$version/
tar zcf style-checker_${version}.tar.gz style-checker_$version/
rm -rf style-checker_$version

echo "All done, it's in style-checker_${version}.tar.gz, you should tag it and push it up to github"
