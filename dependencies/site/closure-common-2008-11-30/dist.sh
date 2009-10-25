#!/bin/sh
cd $(dirname $0)
home=$(pwd)
name=$(basename $home)
dir=${name}-$(date --iso)

TMPDIR=`mktemp -d /tmp/dist.XXXXXXXXXX`
cleanup() {
    cd
    rm -rf $TMPDIR
}
trap cleanup exit

cd $TMPDIR
cvs -d "`cat $home/CVS/Root`" export -r HEAD -d "$dir" "$name"

tgz=$TMPDIR/${dir}.tgz
tar czf $tgz $dir
gpg -b -a $tgz

mv $tgz $tgz.asc $home/
