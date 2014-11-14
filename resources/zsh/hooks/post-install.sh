#! /bin/sh

if [ ! -f ~/.private.zshrc ]; then
    email=""
    while true; do
        email=$(__input "Your email address:")
        if [[ ! "$email" =~ ^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9.-]+$ ]]; then
            __pr_err "Invalid email address"
        else
            break
        fi
    done
    echo "export EMAIL=$email" > ~/.private.zshrc
fi
