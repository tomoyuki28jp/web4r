#!/bin/sh

# Latest 4.5 release from http://www.oracle.com/technology/software/products/berkeley-db/db/index.html

OLDDIR=`pwd`
VERSION="4.5.20"
echo Downloading $VERSION

if [ -d db-$VERSION ]; then
   echo "Berkeley DB $VERSION already downloaded, will not install it"
else
   wget http://download.oracle.com/berkeley-db/db-$VERSION.tar.gz
   tar -xvf db-$VERSION.tar.gz
   cd db-$VERSION/build_unix/
   ../dist/configure
   make
  
   #Check for sufficient write privileges
   if [ -w /usr/local/BerkeleyDB.4.5 ] ; then 
       make install
   else
       #Install as super user
       echo "Elephant install-bdb"
       echo "Make install needs write permissions to /usr/local, running:"
       echo "sudo make install"
       sudo make install 
   fi
fi
cd $OLDDIR
echo "Create my-config.sexp"
if [ -e /usr/local/BerkeleyDB.4.5/lib/ ] ; then
   if [ -e my-config.sexp ] ; then
     echo "my-config.sexp already exists, will not overwrite it"
   else
    echo '' > my-config.sexp
    echo '((:berkeley-db-include-dir . "/usr/local/BerkeleyDB.4.5/include/")' >> my-config.sexp
    echo ' (:berkeley-db-lib-dir . "/usr/local/BerkeleyDB.4.5/lib/")' >> my-config.sexp
    echo ' (:berkeley-db-lib . "/usr/local/BerkeleyDB.4.5/lib/libdb-4.5.so")' >> my-config.sexp
    echo ' (:pthread-lib . nil)' >> my-config.sexp
    echo ' (:clsql-lib . nil))' >> my-config.sexp
   fi
fi