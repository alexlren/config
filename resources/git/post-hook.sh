#! /bin/sh

if [ -d ~/.gitconfig.d ]
then
    REALNAME=$(grep "^${USER}:" /etc/passwd | cut -d':' -f5)
    HOSTDOMAIN=$(hostname)
    EMAIL_ADDR="${USER}@${HOSTDOMAIN}"
    cat > ~/.gitconfig.d/user.conf <<EOF
[user]
	name = ${REALNAME}
	email = ${EMAIL_ADDR}
EOF
fi
