#!/bin/bash
# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

################################################################################
# History
################################################################################
# Make bash append rather than overwrite the history on disk.
shopt -s histappend

# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
shopt -s cdspell

# History options
export HISTFILESIZE=20000
export HISTSIZE=20000
export HISTCONTROL=ignoredups:erasedups

# Write each command to the history file and read in commands from other shells.
PROMPT_COMMAND='history -a; history -n'


################################################################################
# Aliases
################################################################################

if type -a vim > /dev/null 2>&1; then
    alias vi=vim
    alias view='vim -R'
fi

export VISUAL="vi"

# /etc/profile sets umask 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
umask 027

Color_Off="\[\033[0m\]"       # Text Reset
Black="\[\033[0;30m\]"        # Black
Red="\[\033[0;31m\]"          # Red
Green="\[\033[0;32m\]"        # Green
Yellow="\[\033[0;33m\]"       # Yellow
Blue="\[\033[0;34m\]"         # Blue
Purple="\[\033[0;35m\]"       # Purple
Cyan="\[\033[0;36m\]"         # Cyan
White="\[\033[0;37m\]"        # White


# Last part is python
export PATH="${PATH}:${HOME}/bin:${HOME}/.bin:${HOME}/.local/bin"


if [[ -e ~/.localrc ]]; then
    source ~/.localrc
fi

# export PYTHONSTARTUP=$HOME/config/interactive.py
# curl -o ~/.git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
GIT_PROMPT=/usr/lib/git-core/git-sh-prompt
if [[ -e "$GIT_PROMPT" ]]; then
    source "$GIT_PROMPT"
    export PS1="${Green}[\u@\h]${Purple} ${Yellow}\$(__git_ps1)${Color_Off}\n\w $ "
else
    export PS1="${Green}[\u@\h]${Purple}${Color_Off}\n\w $ "
fi
