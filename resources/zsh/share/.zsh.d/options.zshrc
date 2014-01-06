#######################################
# Zsh xconfig                         #
# Made by shaoner <shaoner@gmail.com> #
#######################################

# -------
# OPTIONS
# -------

# No Beep
unsetopt beep
unsetopt hist_beep
unsetopt list_beep
# Handle >|
#unsetopt clobber
# Ctrl+D = logout
unsetopt ignore_eof
# Display exit status
unsetopt print_exit_value
# Ask when removing all
unsetopt rm_star_silent
# Fix the command if unknown
#setopt correct
# Remove useless jokers
setopt nullglob
#setopt menu_complete
#unsetopt list_ambiguous
# Remove useless / after a space
#setopt auto_remove_slash
# No completion for hidden files
#unsetopt glob_dots
# Handle symbolic links
setopt chase_links
# Write command without executing in history search
setopt hist_verify
# Change directory without using cd
setopt auto_cd
# Push directory on the stack during cd
setopt auto_pushd
# Ignore stack elements that already exist
setopt pushd_ignore_dups
# Don't display stack after push/pop
setopt pushd_silent
# "pushd" = "pushd $HOME"
setopt pushd_to_home
# Extending globing
setopt extendedglob
setopt numericglobsort
# jobs in background set to 0
unsetopt bg_nice
# don't send hup to processes
unsetopt hup
# IFS support
setopt shwordsplit
