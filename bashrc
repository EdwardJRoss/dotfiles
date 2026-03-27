# shellcheck shell=bash

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

################################################################################
# Aliases
################################################################################

if type -a vim > /dev/null 2>&1; then
    alias vi=vim
    alias view='vim -R'
fi

################################################################################
# Prompt
################################################################################

Color_Off="\[\033[0m\]"       # Text Reset
Green="\[\033[0;32m\]"        # Green
Yellow="\[\033[0;33m\]"       # Yellow

__prompt_git_dir=
__prompt_git_branch=

__update_prompt() {
    local cwd branch

    history -a
    history -n

    cwd=$PWD
    if [[ $cwd != "$__prompt_git_dir" ]]; then
        __prompt_git_dir=$cwd
        __prompt_git_branch=

        if command -v git >/dev/null 2>&1 && git -C "$cwd" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
            branch=$(git -C "$cwd" symbolic-ref --quiet --short HEAD 2>/dev/null)
            if [[ -z $branch ]]; then
                branch=$(git -C "$cwd" rev-parse --short HEAD 2>/dev/null)
            fi

            if [[ -n $branch ]]; then
                __prompt_git_branch=" ${Yellow}(${branch})${Color_Off}"
            fi
        fi
    fi

    PS1="${Green}[\u@\h]${Color_Off}${__prompt_git_branch}\n\w $ "
}

if [[ -e "$HOME/.localrc" ]]; then
    # shellcheck disable=SC1090
    source "$HOME/.localrc"
fi

PROMPT_COMMAND='__update_prompt'
