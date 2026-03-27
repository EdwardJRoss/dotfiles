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

PROMPT_DIRTRIM=3

Color_Off=
Red=
Green=
Yellow=
Cyan=
Bold=

if [[ -t 1 ]] && command -v tput >/dev/null 2>&1; then
    if [[ $(tput colors 2>/dev/null || printf 0) -ge 8 ]]; then
        Color_Off="\[$(tput sgr0)\]"
        Red="\[$(tput setaf 1)\]"
        Green="\[$(tput setaf 2)\]"
        Yellow="\[$(tput setaf 3)\]"
        Cyan="\[$(tput setaf 6)\]"
        Bold="\[$(tput bold)\]"
    fi
fi

__prompt_git_ref() {
    local ref

    command -v git >/dev/null 2>&1 || return
    git -C "$PWD" rev-parse --is-inside-work-tree >/dev/null 2>&1 || return

    ref=$(git -C "$PWD" symbolic-ref --quiet --short HEAD 2>/dev/null)
    if [[ -z $ref ]]; then
        ref=$(git -C "$PWD" rev-parse --short HEAD 2>/dev/null)
    fi

    [[ -n $ref ]] || return
    printf ' %s(%s)%s' "$Yellow" "$ref" "$Color_Off"
}

__prompt_context() {
    local context=

    if [[ -n ${VIRTUAL_ENV-} ]]; then
        context=${context:+$context,}venv:$(basename "$VIRTUAL_ENV")
    fi

    if [[ -n ${SSH_CONNECTION-} ]]; then
        context=${context:+$context,}ssh
    fi

    [[ -n $context ]] || return
    printf ' %s[%s]%s' "$Cyan" "$context" "$Color_Off"
}

__prompt_jobs() {
    local jobs_count=0 _

    while read -r _; do
        ((jobs_count++))
    done < <(jobs -p 2>/dev/null)

    ((jobs_count > 0)) || return
    printf ' %s{%s job%s}%s' "$Yellow" "$jobs_count" "$([[ $jobs_count -eq 1 ]] && printf '' || printf 's')" "$Color_Off"
}

__update_prompt() {
    local status=$?
    local status_mark context jobs

    history -a

    if (( status != 0 )); then
        status_mark="${Red}!${status}${Color_Off} "
    fi

    context=$(__prompt_context)
    jobs=$(__prompt_jobs)
    PS1="${status_mark}${Green}[\u@\h]${Color_Off}${jobs}${context}$(__prompt_git_ref)\n${Cyan}\w${Color_Off} \$ "
}

if [[ -e "$HOME/.localrc" ]]; then
    # shellcheck disable=SC1090
    source "$HOME/.localrc"
fi

PROMPT_COMMAND='__update_prompt'
