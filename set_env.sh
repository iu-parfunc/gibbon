# This file must be sourced with ". set_env.sh" or "source set_env.sh"

export TREELANGDIR=`pwd`

# A shortcut to make things easier:
function tc() {
    cur=`pwd`
    cd $TREELANGDIR/Gibbon/
    stack build 
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

# A quick verison that doesn't check for recompile.
function tcq() {    
    cur=`pwd`
    cd $TREELANGDIR/Gibbon/
    CMD=`stack exec -- which gibbon`
    cd $cur
    $CMD $@
}
