# This file must be sourced with ". set_env.sh" or "source set_env.sh"

export TREELANGDIR=`pwd`

export PLTADDONDIR=$TREELANGDIR/.racket_sandbox/

INSTALL="stack --install-ghc build "

# A shortcut to make things easier:
function tc() {
    cur=`pwd`
    cd $TREELANGDIR/gibbon-compiler/
    $INSTALL
    if [ "$?" == "0" ]; then
        CMD=`stack exec -- which gibbon`;
        cd $cur;
        $CMD $@;
    else
        cd $cur;
        echo "'stack build' failed";
        return 1;
    fi
}

# A debugging version that prints back-traces upon exceptions.
#
# WARNING: as of stack 1.2 you need to stack-clean between using
# tc/tcd.  It is not smart enough to rebuild automatically and keep
# the build artifacts separate.
function tcd() {    
    INSTALL="$INSTALL --trace" tc $@
}

# A quick verison that doesn't check for recompile.
function tcq() {    
    cur=`pwd`
    cd $TREELANGDIR/gibbon-compiler/
    CMD=`stack exec -- which gibbon`
    cd $cur
    $CMD $@
}
