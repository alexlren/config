# ---------------------------------------
# ENVIRONMENT
# ---------------------------------------

umask 022
export EDITOR='emacsclient -a "" -t'
export VISUAL="$EDITOR"
export SUDO_EDITOR="$EDITOR"
export PATH="$PATH:/usr/local/bin:/usr/local/sbin:/bin:/usr/bin:/usr/sbin:/usr/bin/X11:/usr/X11R6/bin:/usr/games:/sbin:$HOME/bin"
export MAKE='gmake'
if [ -z "$TERM" ]
then
	export TERM='xterm-256color'
fi
