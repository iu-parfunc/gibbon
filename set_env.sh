# This file must be sourced with ". set_env.sh" or "source set_env.sh"

export TREELANGDIR=`pwd`

# alias tc="stack build && stack exec -- treec"

function tc() {
    cur=`pwd`
    cd $TREELANGDIR/TreeLang/
    stack build 
    if [ "$?" == "0" ]; then
        CMD=`stack exec -- which treec`;
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
    cd $TREELANGDIR/TreeLang/
    CMD=`stack exec -- which treec`
    cd $cur
    $CMD $@
}
