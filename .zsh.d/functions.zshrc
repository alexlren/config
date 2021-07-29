# ---------------------------------------
# FUNCTIONS
# ---------------------------------------

__print_err()
{
    echo -e "$*" 1>&2
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

bak()
{
    __expected_arg 1 $# || return 1
    __file_exists "$1" || return 2
    mv -v $1{,.bak}
}

rst()
{
    __expected_arg 1 $# || return 1
    __file_exists "$1" || return 2
    if [ -e "$1" ]; then
        bn=`basename $1 .bak`
        if [ ! "$1" = "$bn" ]; then
                mv -v $bn{.bak,}
        fi
    fi
}

extract()
{
    __expected_arg 1 $# || return 1
    __file_exists "$1" || return 1
    local fullpath=`realpath "$1"`
    local tmpdir=`mktemp -d`
    cd $tmpdir
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
            __print_err "Unsupported archive"
            cd -
            rm -rf $tmpdir
            return 1
            ;;
    esac
}

lsa()
{
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
            __print_err "Unsupported archive"
            return 1
            ;;
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

# /!\ Dangerous function
#
# erase_disk()
# {
#     __expected_arg 1 $# || return 1
#     local drive=${1##/dev/}
#     if [ ! -b "/dev/$drive" ]; then
#         __print_err "$1 is not a valid drive"
#         return 2
#     fi
#     local disk_size=$(df -B1 /dev/$drive | tail -n1 | tr -s ' ' | cut -d' ' -f2)
#     local phys_block_size=$(cat /sys/block/$drive/queue/physical_block_size)
#     local num_blocks=$(($disk_size / $phys_block_size))
#     echo "/dev/$drive: Size=$disk_size, Block size=$phys_block_size, Number of blocks=$num_blocks"
#     if __yesno "Erasing /dev/$drive?"; then
#         openssl enc -aes-256-ctr -pass pass:"$(dd if=/dev/random bs=128 count=1 2>/dev/null | base64)" -nosalt </dev/zero \
#             | pv -bartpes $disk_size | sudo dd bs=$phys_block_size count=$num_blocks of=/dev/$drive
#     fi
# }

mount_enc()
{
    sudo cryptsetup open $1 $2
    sudo mkdir -p /mnt/$2
    sudo mount /dev/mapper/$2 /mnt/$2
}

umount_enc()
{
    sudo umount /dev/mapper/$1
    sudo cryptsetup close $1
    sudo rm -rf /mnt/$1
}

sym_encrypt()
{
    __expected_arg 1 $# || return 1
    __file_exists "$1" || return 1
    local infile=$1
    local outfile=$infile.gpg
    if [ -d "$infile" ]; then
        local indir=$(dirname "$infile")
        tar -C $indir -czf "$infile.tar.gz" "$(basename $infile)"
        rm -rf "$infile"
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

sym_decrypt()
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

zsudo()
{
    local zshrc=~/.zshrc
    sudo zsh -i -c "source $zshrc"'; eval "$0" "$@"' "$@"
}

clean_me()
{
    sudo flatpak uninstall --unused
    sudo rm -rf /var/tmp/flatpak-cache-*
    sudo dnf clean all
    sudo journalctl --vacuum-size=500M
    pkcon refresh force
}
