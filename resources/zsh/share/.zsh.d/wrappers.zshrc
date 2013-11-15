#######################################
# Zsh xconfig                         #
# Made by shaoner <shaoner@gmail.com> #
#######################################

# ---------------------------------------
# WRAPPERS
# ---------------------------------------

function __wpkg_apt()
{
    echo "Not implemented yet."
}

function __wpkg_yum()
{
    if [ $# -eq 0 ]
    then
        return 1
    fi
    case $1 in
        installed)
            if [ $# -eq 2 ]
            then
                RES=$(rpm -qa | grep $2)
                echo "* \`$2' matches packages: "
                BAKIFS=$IFS
                IFS="
"
                for r in $RES
                do
                    echo "[*] $r"
                done
                IFS=$BAKIFS
            else
                rpm -qa
            fi
            ;;
            provided)
            if [ $# -eq 1 ]
            then
                echo "$0 $1: missing argument"
                return 1
            fi
            if [ ! -f $2 ]
            then
                echo "* $2 does not exist"
            fi
            RES=$(rpm -qf $2)
            echo "Package(s) providing $2:"
            BAKIFS=$IFS
            IFS="
"
            for r in $RES
            do
                echo "[*] $r"
            done
            IFS=$BAKIFS
            ;;
    esac
}

function wpkg()
{
    which yum >/dev/null 2>&1 && __wpkg_yum $* ; return $?
    which apt-get >/dev/null 2>&1 && __wpkg_apt $* ; return $?
    echo "Unknown package manager"
}
