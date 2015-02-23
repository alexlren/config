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
    if [ $2 -lt $1 ]; then
        __print_err "$funcstack[2]: Expected at least $1 arguments, got $2"
        return 1
    fi
    return 0
}

__file_exists()
{
    if [ ! -e "$1" ]; then
        __print_err "$funcstack[2]: Cannot find \`$1': No such file or directory"
        return 1
    fi
    return 0
}

__read_ans()
{
    local ans
    echo -n "$* " 1>&2
    read ans
    echo $ans
}

__yesno()
{
    local ans=$(__read_ans "$* [y/N]")
    ([ "$ans" = "y" ] || [ "$ans" = "Y" ]) && return 0
    return 1
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

hl()
{
    __expected_arg 1 $# || return 1
    local pattern="$1"
    shift
    grep -EIRi "$pattern|\$" ${*:-.}
}

hlf()
{
    __expected_arg 1 $# || return 1
    local pattern="$1"
    shift
    grep -EHIRin "$pattern|\$" ${*:-.}
}

gr()
{
    __expected_arg 1 $# || return 1
    local pattern="$1"
    shift
    grep -EHIRn "$pattern" ${*:-.}
}

gri()
{
    __expected_arg 1 $# || return 1
    local pattern="$1"
    shift
    grep -EHIRin "$pattern" ${*:-.}
}

bak()
{
    __expected_arg 1 $# || return 1
    for fd in $*; do
        if [ -e $fd ]; then
            mv -v $fd{,.bak}
        fi
    done
}

rst()
{
    __expected_arg 1 $# || return 1
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
    __expected_arg 2 $# || return 1
    if [ -L "$2" ]; then
        rm -f "$2"
    fi
    ln -s "$1" "$2"
}

__extract_mkdir()
{
    local bfile=$(basename $1)
    local dir=${bfile%.*}
    mkdir -p "$dir"
    echo "$dir"
}

extract()
{
    __expected_arg 1 $# || return 1
    if [ ! -w . ]; then
        __print_err "Cannot extract tarball here"
        return 1
    fi
    __file_exists "$1" || return 1
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
        *.tar)
            tar xvf "$fullpath"
            ;;
        *.gz)
            gunzip "$fullpath"
            ;;
        *.bz2)
            bunzip2 "$fullpath"
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
    __expected_arg 1 $# || return 1
    __file_exists "$1" || return 1
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
    __expected_arg 1 $# || return 1
    __file_exists "$1" || return 1
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
    __expected_arg 1 $# || return 1
    killall $1
    [ $? -eq 0 ] && return 0
    killall -9 $1
}

set_proxy()
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
    echo
    echo -n "Setup proxy: "
    echo $line | sed -r 's#(http://[^:]+):[^@]+(@.*)#\1\2#'
    export http_proxy="$line"
    export https_proxy="$line"
    export ftp_proxy="$line"
    export all_proxy="$line"
}

unset_proxy()
{
    unset http_proxy https_proxy ftp_proxy all_proxy
}

scan()
{
    local target
    if [ $# -ne 0 ]; then
        target="$1"
    else
        local ifc=$(route -n | grep '^0\.0\.0\.0' | awk '{ print $8 }')
        local ipaddr=$(ifconfig $ifc | egrep -o 'inet [^ ]+' | awk '{ print $2 }')
        target=$(echo $ipaddr | cut -d'.' -f1-3).0/23
    fi
    sudo nmap -sS -O $target
}

erase_disk()
{
    __expected_arg 1 $# || return 1
    local drive=${1##/dev/}
    if [ ! -b "/dev/$drive" ]; then
        __print_err "$1 is not a valid drive"
        return 2
    fi
    local disk_size=$(df -B1 /dev/$drive | tail -n1 | tr -s ' ' | cut -d' ' -f2)
    local phys_block_size=$(cat /sys/block/$drive/queue/physical_block_size)
    local num_blocks=$(($disk_size / $phys_block_size))
    echo "/dev/$drive: Size=$disk_size, Block size=$phys_block_size, Number of blocks=$num_blocks"
    if __yesno "Erasing /dev/$drive?"; then
        openssl enc -aes-256-ctr -pass pass:"$(dd if=/dev/random bs=128 count=1 2>/dev/null | base64)" -nosalt </dev/zero \
            | pv -bartpes $disk_size | sudo dd bs=$phys_block_size count=$num_blocks of=/dev/$drive
    fi
}

mount_enc()
{
    __expected_arg 2 $# || return 1
    __file_exists "$1" || return 1
    sudo cryptsetup open $1 $2
    sudo mkdir -p /mnt/$2
    sudo mount /dev/mapper/$2 /mnt/$2
}

umount_enc()
{
    __expected_arg 1 $# || return 1
    __file_exists "/dev/mapper/$1" || return 1
    __file_exists "/dev/$1" || return 1
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

encrypt()
{
    __expected_arg 1 $# || return 1
    __file_exists "$1" || return 1
    local infile=$1
    local outfile=$infile.gpg
    if [ -d "$infile" ]; then
        local indir=$(dirname "$infile")
        tar -C $indir -czf "$infile.tar.gz" "$(basename $infile)"
        rm -rf "$infile" >/dev/null
        infile=$infile.tar.gz
        outfile=$infile.gpg
    fi
    if [ $# -gt 1 ]; then
        outfile=$2
    fi
    echo "Encrypting $1 into $outfile"
    gpg --cipher-algo BLOWFISH --output "$outfile" --symmetric "$infile"
    rm -f "$infile" >/dev/null
}

decrypt()
{
    __expected_arg 1 $# || return 1
    __file_exists "$1" || return 1
    local infile=$1
    local outfile
    if [ $# -gt 1 ]; then
        outfile=$2
    elif [ "${infile##*.}" = "gpg" ]; then
        outfile=${infile%%.gpg}
    else
        __print_err "$infile does not contain a .gpg extension"
        __expected_arg 2 $#
        return 1
    fi
    gpg --output "$outfile" --decrypt "$infile"
    if ! [ "${outfile%%.tar.gz}" = "$outfile" ]; then
        local outdir=$(dirname "$outfile")
        tar -C "$outdir" -xzf "$outfile"
        rm -f "$outfile" >/dev/null
        outfile=${outfile%%.tar.gz}
    fi
    echo "Decrypting $infile into $outfile"
    rm -f "$infile" >/dev/null
}

ports()
{
    local opts="nelp"
    if [ $# -ne 0 ]; then
        case $1 in
            tcp)
                opts="${opts}t"
                ;;
            udp)
                opts="${opts}u"
                ;;
            *)
                opts="${opts}tu"
                ;;
        esac
    else
        opts="${opts}tu"
    fi
    netstat -$opts
}
