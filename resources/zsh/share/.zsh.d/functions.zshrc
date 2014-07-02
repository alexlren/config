#######################################
# Zsh xconfig						 #
# Made by shaoner <shaoner@gmail.com> #
#######################################

# ---------------------------------------
# FUNCTIONS
# ---------------------------------------


function _search()
{
	if [ $# -lt 2 ]; then
		return 1
	fi
	local opts=$1
	shift
	local pattern=$1
	shift
	local findpath="."
	if [ $# -gt 0 ]; then
		findpath=$*
	fi
	find $findpath $opts $pattern
}

function s()
{
	_search "-name" $@
}

function si()
{
	_search "-iname" $@
}

function each()
{
	if [ $# -lt 2 ]; then
		return 1
	fi
	local pattern="$1"
	local cmd="$2"
	cmd=${cmd/@f/\$f}
	for f in $(find . -name "$pattern"); do
		eval $cmd
	done

}

function _wgrep()
{
	if [ $# -lt 2 ]; then
		return 1
		fi
	local opts=$1
	shift
	local pattern=$1
	shift
	local fgrpath="."
	if [ $# -gt 0 ]; then
		fgrpath="$*"
	fi
	grep --color $opts $pattern $fgrpath
}

function hl()
{
	_wgrep -EiI "$1|\$" $@
}

function gr()
{
	_wgrep -HIrn $@
}

function gri()
{
	_wgrep -HIrin "$1" "$2"
}

function bak()
{
	if [ $# -eq 1 ] && [ -f "$1" ]; then
		local name="${1}.bak"
		while [ -f "$name" ]; do
			name="${name}.bak"
		done
		mv -v "$1" "$name"
	fi
}

function rst()
{
	if [ $# -eq 1 ] && [ -f "$1" ]; then
		local ext=$(echo $1 | rev | cut -d'.' -f1 | rev)
		local file=$1
		if [ "$ext" = "bak" ]; then
			mv -v ${file%%.bak}{.bak,}
		fi
	fi
}

function reln()
{
	if [ $# -eq 2 ]; then
		if [ -L "$2" ]; then
			rm -f "$2"
		fi
		ln -s $1 $2
	fi
}

function extract()
{
	if [ $# -ne 1 ]; then
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

function tarball()
{
	if [ $# -lt 1 ] || [ ! -e "$1" ]; then
		return 1
	fi
	local tarname="${1}.tar.bz2"
	tar cjf $tarname $*
}

function reload()
{
	for fn in $*; do
		unfunction $fn
		autoload -U $fn
	done
	source ~/.zshrc

}

function rmip()
{
	if [ $# -ne 1 ]; then
		return 1
	fi
	local ipaddr=$(echo $1 | sed 's/\./\\\./g')
	sed -i -e "/^$ipaddr/d" ~/.ssh/known_hosts
}

function nkill()
{
	if [ $# -ne 1 ]; then
		return 1
	fi
	killall $1
	[ $? -eq 1 ] && return 0
	killall $1 >/dev/null 2>&1
	[ $? -eq 0 ] && killall -9 $1
	return 0
}
