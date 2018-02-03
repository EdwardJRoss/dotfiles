# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

################################################################################
# History
################################################################################
# Make bash append rather than overwrite the history on disk
shopt -s histappend

# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
shopt -s cdspell

# Uncomment to turn on programmable completion enhancements.
# Any completions you add in ~/.bash_completion are sourced last.
# [[ -f /etc/bash_completion ]] && . /etc/bash_completion

# History Options
export HISTFILESIZE=20000
export HISTSIZE=20000

# Backup history

# Don't put duplicate lines in the history.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups

# Ignore some controlling instructions
# HISTIGNORE is a colon-delimited list of patterns which should be excluded.
# The '&' is a special pattern which suppresses duplicate entries.
# export HISTIGNORE=$'[ \t]*:&:[fb]g:exit'
# export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls' # Ignore the ls command as well
#
# Whenever displaying the prompt, write the previous line to disk
export PROMPT_COMMAND="history -a"


################################################################################
# Aliases
################################################################################

if type -a vim > /dev/null 2>&1; then
    vi=vim
    alias vi=vim
    alias view='vim -R'
else
    echo >&2 "Vim not available on host"
    vi=vi
fi

export VISUAL="vi"

alias e="$VISUAL"
alias py='python'

alias l='ls -CF'                 # classify files in colour
alias ls='ls -hF --color=tty'                 # classify files in colour
alias ll='ls --color=auto -Flh'
alias la='ls -A'                              # all but . and ..
alias ct='cat -v -T'

alias cl='column -ts"»  " | head'
alias col='column -ts"» " | less -S'
alias tl='tail -n+2'

alias 'current-branch'='git rev-parse --abbrev-ref HEAD'

alias info='info --vi-keys'

# Default to human readable figures
alias df='df -h'
alias du='du -h'
#
# Misc :)
alias less='less -s -r'                          # raw control characters
alias where='type -a'                        # where, of a sort
alias grep='grep --color=auto'                     # show differences in colour
alias egrep='egrep --color=auto'              # show differences in colour
alias fgrep='fgrep --color=auto'              # show differences in colour

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

# Mercurial PS1: Need to resove
#hg_ps1() {
#        hg prompt "on hg branch {branch}" 2> /dev/null
#    }

# Python Virtual Environment PS1 need to resolve
#venv_ps1() {
#    venv=`basename "$VIRTUAL_ENV"`
#    if [ -n "$venv" ] ; then
#        echo "{$venv}"
#    fi
#}

# export PS1="$Green[\u@\h]$Purple \$(venv_ps1)$Yellow\$(__git_ps1)\$(hg_ps1)$Color_Off\n\w $ "
export PS1="$Green[\u@\h]$Color_Off\n\w $ "
# export VIRTUAL_ENV_DISABLE_PROMPT=1

# Last part is python
export PATH="$PATH:$HOME/bin:$HOME/.bin:$HOME/.local/bin"


# Requires virtualenvwrapper to be installed
export WORKON_HOME=".virtualenvs"
export PROJECT_HOME="$HOME/projects"
for dir_root in /usr "$HOME/.local"; do
    venv_script="bin/virtualenvwrapper_lazy.sh"
    if [[ -e "${dir_root}/${venv_script}" ]]; then
    source "${dir_root}/${venv_script}"
    fi
done


if [[ -e ~/.localrc ]]; then
    source ~/.localrc
fi

# export PYTHONSTARTUP=$HOME/config/interactive.py
# curl -o ~/.git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
if [[ -e /usr/share/git/git-prompt.sh ]]; then
    source /usr/share/git/git-prompt.sh
    export PS1="$Green[\u@\h]$Purple $Yellow\$(__git_ps1)$Color_Off\n\w $ "
else
    export PS1="$Green[\u@\h]$Purple$Color_Off\n\w $ "
fi

# https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
