# ---------------------------------------
# FUNCTIONS
# ---------------------------------------

__print_err()
{
    echo -e "$*" 1>&2
    return 1
}

__expected_arg()
{
    __print_err "Expected $1 arguments but got $2"
}

__file_exists()
{
    test -e "$1" || __print_err "$1: No such file or directory"
}

s()
{
    if [ $# -eq 1 ]; then
        find -name "$1"
    elif [ $# -eq 2 ]; then
        find "$2" -name "$1"
    else
        find $*
    fi
}

si()
{
    if [ $# -eq 1 ]; then
        find -iname "$1"
    elif [ $# -eq 2 ]; then
        find "$2" -iname "$1"
    else
        find $*
    fi
}

__hl()
{
    if [ $# -eq 1 ]; then
        __expected_arg 1 0
        return 1
    fi
    local opts=$1
    local pattern=$2
    shift
    grep $opts "$pattern|\$" $*
}

hl()
{
    __hl -EiIR $*
}

hlf()
{
    __hl -EiIRHn $*
}

__xgrep()
{
    if [ $# -eq 0 ]; then
        __expected_arg 1 0
        return 1
    fi
    local opts pattern grpath
    while [ $# -ne 0 ] && [[ "$1" =~ '^-[^ ]+' ]]; do
        opts="$opts $1"
        shift
    done
    pattern=$1
    shift
    if [ $# -eq 0 ]; then
        grpath=.
    else
        grpath=$*
    fi
    grep $opts $pattern $grpath
}

gr()
{
    __xgrep -HIRn $*
}

gri()
{
    __xgrep -HIRin $*
}

bak()
{
    if [ $# -eq 0 ]; then
        __expected_arg 1 0
        return 1
    fi
    for fd in $*; do
        if [ -e $fd ]; then
            mv -v $fd{,.bak}
        fi
    done
}

rst()
{
    if [ $# -eq 0 ]; then
        __expected_arg 1 0
        return 1
    fi
    local bn
    for fd in $*; do
        if [ -e $fd ]; then
            bn=$(basename $fd .bak)
            if [ ! "$fd" = "$bn" ]; then
                mv -v $bn{.bak,}
            fi
        fi
    done
}

reln()
{
    if [ $# -ne 2 ]; then
        __expected_arg 2 $#
        return 1
    fi
    if [ -L "$2" ]; then
        rm -f "$2"
    fi
    ln -s "$1" "$2"
}

__extract_mkdir()
{
    local ext=$(echo $1 | rev | cut -d. -f1 | rev)
    local dir=$(basename $1 .$ext)
    mkdir -p "$dir"
    echo "$dir"
}

extract()
{
    if [ $# -ne 1 ]; then
        __expected_arg 1 $#
        return 1
    fi
    if [ ! -w . ]; then
        __print_err "Cannot extract tarball here"
        return 1
    fi
    if ! __file_exists "$1"; then
        return 1
    fi
    local dir=$(__extract_mkdir "$1")
    local fullpath=$(realpath "$1")
    cd $dir
    case "$1" in
        *.rpm)
            rpm2cpio "$fullpath" | cpio -idmv
            ;;
        *.tar.bz2)
            tar xjvf "$fullpath"
            ;;
        *.tar.xz)
            tar xJvf "$fullpath"
            ;;
        *.tar.gz|*.tgz)
            tar xzvf "$fullpath"
            ;;
        *.zip)
            unzip "$fullpath"
            ;;
        *.rar)
            unrar e "$fullpath"
            ;;
        *)
            __print_err "$(basename $1) is an unsupported archive"
            cd -
            rm -rf "$dir"
            ;;
    esac
}

tarball()
{
    if [ $# -eq 0 ]; then
        __expected_arg 1 0
        return 1
    fi
    if ! __file_exists "$1"; then
        return 1
    fi
    local tn="tar.xz:tar cJvf;tar.bz2:tar cjvf;tar.gz:tar czvf;zip:zip"
    local i ty choice create tarname
    echo "Which type of tarball? "
    IFS=";"
    i=0
    for tyn in $tn; do
        i=$(($i + 1))
        ty=$(echo $tyn | cut -d: -f1)
        echo "[$i] $ty"
    done

    while [[ ! "$choice" =~ "[1-$i]+" ]]; do
        read -n 'choice?Type [1]: '
        if [ -z "$choice" ]; then
            choice=1
            break
        fi
    done
    choice=$(echo "$tn" | cut -d';' -f$choice)
    ext=$(echo $choice | cut -d: -f1)
    create=$(echo $choice | cut -d: -f2)
    if [ $# -eq 1 ]; then
        tarname="$1.$ext"
    else
        while [ -z "$tarname" ]; do
            read -n 'tarname?Name of the tarball: '
        done
        tarname="$tarname.$ext"
    fi
    echo "Creating $tarname..."
    eval $create $tarname $*
}

lst()
{
    if [ $# -ne 1 ]; then
        __expected_arg 1 $#
        return 1
    fi
    if ! __file_exists "$1"; then
        return 1
    fi
    case "$1" in
        *.tar.*)
            tar tf "$1"
            ;;
        *.zip)
            unzip -v "$1"
            ;;
        *.rar)
            unrar v "$1"
            ;;
        *)
            __print_err "$1 is an unsupported archive"
    esac
}

reload()
{
    echo "Reload zsh configuration..."
    for fn in $*; do
        unfunction $fn
        autoload -U $fn
    done
    source ~/.zshrc
}

nkill()
{
    if [ $# -ne 1 ]; then
        __expected_arg 1 $#
        return 1
    fi
    killall $1
    [ $? -eq 0 ] && return 0
    killall -9 $1
}

setup_proxy()
{
    local hostname port username password line
    while [ -z "$hostname" ]; do
        read -n 'hostname?Hostname: '
    done
    read -n 'port?Port: '
    read -n 'username?Username: '
    read -s -n 'password?Password: '
    line="http://"
    if [ ! -z "$username" ]; then
        line="$line$username"
        if [ ! -z "$password" ]; then
            password=$(python -c "import urllib; print urllib.quote('$password')")
            line="$line:$password"
        fi
        line="$line@"
    fi
    line="$line$hostname"
    if [ ! -z "$port" ]; then
        line="$line:$port"
    fi
    echo -n "Setup proxy: "
    echo $line | sed -r 's#(http://[^:]+):[^@]+(@.*)#\1\2#'
    export http_proxy="$line"
    export https_proxy="$line"
    export ftp_proxy="$line"
    export all_proxy="$line"
}

scan()
{
    local opts="-sS -O"
    local target=""
    while [ $# -gt 0 ]; do
        if [[ "$1" =~ "-[^ ]+" ]]; then
            opts="$opts $1"
        else
            target="$1"
        fi
        shift
    done
    if [ "$target" = "" ]; then
        local ifc=$(route -n | grep '^0\.0\.0\.0' | awk '{ print $8 }')
        local ipaddr=$(ifconfig $ifc | egrep -o 'inet [^ ]+' | awk '{ print $2 }')
        target=$(echo $ipaddr | cut -d'.' -f1-3).0/23
    fi
    sudo nmap $opts $target
}

erase_disk()
{
    if [ $# -ne 1 ]; then
        __expected_arg 1 $#
        return 1
    fi
    local drive=${1##/dev/}
    if [ ! -b "/dev/$drive" ]; then
        __print_err "$1 is not a valid drive"
        return 2
    fi
    local disk_size=$(df -B1 /dev/$drive | tail -n1 | tr -s ' ' | cut -d' ' -f2)
    local phys_block_size=$(cat /sys/block/$drive/queue/physical_block_size)
    local num_blocks=$(($disk_size / $phys_block_size))

    openssl enc -aes-256-ctr -pass pass:"$(dd if=/dev/random bs=128 count=1 2>/dev/null | base64)" -nosalt </dev/zero \
        | pv -bartpes $disk_size | sudo dd bs=$phys_block_size count=$num_blocks of=/dev/$drive
}

mount_enc()
{
    if [ $# -ne 2 ]; then
        __expected_arg 2 $#
        return 1
    fi
    sudo cryptsetup open $1 $2
    sudo mkdir -p /mnt/$2
    sudo mount /dev/mapper/$2 /mnt/$2
}

umount_enc()
{
    if [ $# -ne 1 ]; then
        __expected_arg 1 $#
        return 1
    fi
    sudo umount /dev/mapper/$1
    sudo cryptsetup close $1
    sudo rm -rf /mnt/$1
}

__wemacs()
{
    local command=$1
    shift
    local args filename line column
    while [ $# -ne 0 ]; do
        # Skip options
        if [[ ! "$1" =~ "-[^ ]+" ]]; then
            filename=$(echo $1 | cut -d: -f1)
            # Check if arg matches <filename>:<line>
            # and filename exists
            if [ "$filename" != "$1" ] && [ -e "$filename" ]; then
                line=$(echo $1 | cut -d: -f2)
                # Check if <line> is a number
                if [[ "$line" =~ "[0-9]+" ]]; then
                    column=$(echo $1 | cut -d: -f3)
                    # Check if <column> exists and is a number
                    # if not, <column> is discarded
                    if [[ "$column" =~ "[0-9]+" ]]; then
                        args="$args +$line:$column $filename"
                    else
                        args="$args +$line $filename"
                    fi
                    shift
                    continue
                fi
            fi
        fi
        args="$args $1"
        shift
    done
    eval $command $args
}

list_func()
{
    functions | grep -E '^[a-zA-Z][a-zA-Z_]+ \(\) \{$' | sed -r 's/^([^ ]+) .*$/\1/' | tr '\n' ' '
    echo
}

ipti()
{
    sudo iptables -nL $* --line-numbers
}
