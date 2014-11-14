#! /bin/sh

confd=~/.gitconfig.d

if [ ! -f "$confd/user.conf" ]; then
    realname=$(grep "^$USER:" /etc/passwd | cut -d':' -f5)
    email="$USER@$(hostname)"
    while true; do
        rrealname=$(__input "Your real name [$realname]:")
        if [ ! -z "$rrealname" ]; then
            realname=$rrealname
        fi
        remail=$(__input "Your email address [$email]:")
        if [ ! -z "$remail" ]; then
            email=$remail
        fi
        __pr_info "Real name: $realname"
        __pr_info "Email address: $email"
        ans=$(__input "Is it correct? [y/N]")
        if [ "$ans" = "y" ]; then
            break
        fi
    done
    mkdir -p $confd
    cat > "$confd/user.conf" <<EOF
[user]
	name = $realname
	email = $email
EOF
fi
