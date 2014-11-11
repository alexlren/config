#######################################
# Zsh xconfig                         #
# Made by shaoner <shaoner@gmail.com> #
#######################################

# ---------------------------------------
# PROMPT
# ---------------------------------------

__setup_prompt()
{
    local red="%{[33;01;31m%}"
    local green="%{[33;01;32m%}"
    local blue="%{[33;01;36m%}"
    local yellow="%{[33;01;33m%}"
    local purple="%{[33;01;34m%}"
    local lred="%{[33;00;31m%}"
    local lgreen="%{[33;00;32m%}"
    local lblue="%{[33;00;36m%}"
    local lyellow="%{[33;00;33m%}"
    local lpurple="%{[33;00;34m%}"
    local none="%{[0m%}"

    # Display prompt
    export PS1="%B[%b%(!.$lred.$lpurple)%n$none@%(!.$lred.$lpurple)%m$none:%(0?,$none,$lred)%?$none%B]%b "
    export RPS1="%B[%b${lgreen}%~$none%B]%b %B[%b%*%B]%b"
    export PS2="$lblue>$none "
}

__setup_prompt
