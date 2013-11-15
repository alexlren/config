#######################################
# Zsh xconfig                         #
# Made by shaoner <shaoner@gmail.com> #
#######################################

# ---------------------------------------
# PROMPT
# ---------------------------------------

# Colors
red="%{[33;01;31m%}"
green="%{[33;01;32m%}"
blue="%{[33;01;36m%}"
yellow="%{[33;01;33m%}"
purple="%{[33;01;34m%}"
lred="%{[33;00;31m%}"
lgreen="%{[33;00;32m%}"
lblue="%{[33;00;36m%}"
lyellow="%{[33;00;33m%}"
lpurple="%{[33;00;34m%}"
white="%{[0m%}"

# Display prompt
export PS1="%B[%b%(!.$lred.$lpurple)%n$white@%(!.$lred.$lpurple)%m$white:%(0?,$white,$lred)%?$white%B]%b "
export RPS1="%B[%b${lgreen}%~${white}%B]%b %B[%b%*%B]%b"
export PS2="$lblue>$white "
