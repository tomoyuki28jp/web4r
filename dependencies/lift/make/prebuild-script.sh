#!/bin/sh

script_dir=`dirname $0`
root_dir=`dirname $script_dir`
timeout_dir="$root_dir/timeout/"

echo $script_dir
echo $root_dir
echo $timeout_dir

# update with-timeout
cp ~/darcs/trivial-timeout/dev/package.lisp $timeout_dir
cp ~/darcs/trivial-timeout/dev/with-timeout.lisp $timeout_dir

