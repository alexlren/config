#######################################
# Zsh xconfig                         #
# Made by shaoner <shaoner@gmail.com> #
#######################################

# ---------------------------------------
# FUNCTIONS
# ---------------------------------------


function f()
{
    FINDPATH="."
    if [ $# -eq 2 ]
    then
        FINDPATH=$2
    fi
    find $FINDPATH -name $1
}

function fin()
{
    FINDPATH="."
    if [ $# -eq 2 ]
    then
        FINDPATH=$2
    fi
    find $FINDPATH -iname $1
}

function each()
{
    FINDPATH="."
    CMD=$2
    if [ $# -eq 3 ]
    then
        FINDPATH=$2
        CMD=$3
    fi
    CMD=${CMD/@f/\$f}
    for f in $(find $FINDPATH -name $1)
    do
        eval $CMD
    done

}

function _wgrep()
{
    FGRPATH="."
    if [ $# -eq 3 ]
    then
        FGRPATH="$3"
    fi
    grep --color "$1" "$2" $FGRPATH
}

function hl()
{
    _wgrep -EiI "$1|\$" $2
}

function gr()
{
    _wgrep -HIrn "$1" "$2"
}

function gri()
{
    _wgrep -HIrin "$1" "$2"
}

function bak()
{
    if [ $# -eq 1 ] && [ -f $1 ]
    then
        mv -v $1{,.bak}
    fi
}

function rst()
{
    if [ $# -eq 1 ] && [ -f $1 ]
    then
        mv -v $1{.bak,}
    fi
}

function relink()
{
    if [ $# -eq 2 ]
    then
        if [ -L $2 ]
        then
            rm -f $2
        fi
        ln -s $1 $2
    fi
}

function extract()
{
    if [ $# != 1 ]
    then
        return 1
    fi

    case $1 in
        *.rpm)
            DIR=$(echo $1 | cut -d'.' -f1)
            [ -d $DIR ] || mkdir $DIR || (echo "Cannot create $DIR in $(dirname $0)" ; return 1)
            cd $DIR
            rpm2cpio ../$1 | cpio -idmv
            ;;
        *.tar.bz2)
            tar xjvf $1
            ;;
        *.tar.gz|*.tgz)
            tar xzvf $1
            ;;
        *.zip)
            unzip $1
            ;;
        *.rar)
            unrar e $1
            ;;
        *)
            echo "* $1 is an unsupported archive" 1>&2
            ;;
    esac
}

function reload()
{
    for fn in $*
    do
        unfunction $fn
        autoload -U $fn
    done
    source ~/.zshrc

}

function rmip()
{
    if [ $# -ne 1 ]
    then
        return 1
    fi
    IPADDR=$(echo $1 | sed 's/\./\\\./g')
    sed -i -e "/^$IPADDR/d" ~/.ssh/known_hosts
}

function nkill()
{
    killall $1
    [ $? -eq 1 ] && return
    killall $1 >/dev/null 2>&1
    [ $? -eq 0 ] && killall -9 $1
    return 0
}
