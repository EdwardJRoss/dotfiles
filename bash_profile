# shellcheck shell=bash

export PATH="${PATH}:${HOME}/bin:${HOME}/.bin:${HOME}/.local/bin"
export EDITOR="vi"
export VISUAL="vi"

# /etc/profile sets umask 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
umask 027

[[ -f ~/.bashrc ]] && source ~/.bashrc
