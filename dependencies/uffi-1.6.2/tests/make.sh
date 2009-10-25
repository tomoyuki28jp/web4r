#!/bin/sh

case "`uname`" in
    Linux) os_linux=1 ;;
    FreeBSD) os_freebsd=1 ;;
    GNU/kFreeBSD) os_gnukfreebsd=1;;
    Darwin) os_darwin=1 ;;
    SunOS) os_sunos=1 ;;
    AIX) os_aix=1 ;;
    GNU) os_gnu=1 ;;
    *) echo "Unable to identify uname " `uname`
       exit 1 ;;
esac

if [ "$os_linux" ]; then
    gcc -fPIC -DPIC -c $SOURCE -o $OBJECT
    gcc -shared $OBJECT -o $SHARED_LIB

elif [ "$os_gnu" ]; then
    gcc -fPIC -DPIC -c $SOURCE -o $OBJECT
    gcc -shared $OBJECT -o $SHARED_LIB

elif [ "$os_freebsd" ]; then
    gcc -fPIC -DPIC -c $SOURCE -o $OBJECT
    gcc -shared $OBJECT -o $SHARED_LIB

elif [ "$os_gnukfreebsd" ]; then
    gcc -fPIC -DPIC -c $SOURCE -o $OBJECT
    gcc -shared $OBJECT -o $SHARED_LIB

elif [ "$os_darwin" ]; then
    cc -dynamic -c $SOURCE -o $OBJECT
    ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -o $BASE.dylib $OBJECT
    ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress /usr/lib/libz.dylib -o z.dylib

elif [ "$os_sunos" ]; then
    cc -KPIC -c $SOURCE -o $OBJECT
    cc -G $OBJECT -o $SHARED_LIB

elif [ "$os_aix" ]; then
    gcc -c -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void $SOURCE
    make_shared -o $SHARED_LIB $OBJECT
fi

exit 0
