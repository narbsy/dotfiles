# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="[\t] \W: "
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

#aliases GOOO HERE
alias andrew='ssh cnarburg@unix.andrew.cmu.edu'
alias andrewy='andrew -Y'
alias andrewssh='ssh -xl unix.andrew.cmu.edu'
alias vps='ssh 216.59.3.125'
# alias vps_root='ssh 206.225.10.200'
alias isrunning='ps -ef | grep'
alias gems='gem'
alias migrate='rake db:migrate'
alias createdb='rake db:create'
alias gen='script/generate'
alias railsql='mysql -urails -prails'
alias svnaddall='svn status | grep "^\?" | sed -e "s/? *//" | sed -e "s/ /\\\\ /g" | xargs svn add'
alias svnrmall='svn status | grep "^\!" | sed -e "s/! *//" | sed -e "s/ /\\\\ /g" | xargs svn rm'
alias cluster='ssh -D 6789 -L 50128:127.0.0.1:50128 cnarburg@64.88.164.202'
alias sml='rlwrap sml'
alias scheme='rlwrap scheme'
alias git='git-achievements'

#for getting memory usage of a process that may span multiple processes, like chromium
total_mem () { 
  cmd=`ps -Ao %mem,comm | grep $1 | sed 's/^\s*//' | cut --delimiter=' ' -f 1 - | (sed 's/^/x+=/' ; echo x) | bc`
  echo "${cmd}"
}

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'
    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

#for RMagick
export LD_LIBRARY_PATH=/usr/local/lib
#Add cabal for haskell, llvm stuff for clang
export PATH=$PATH:~/.cabal/bin:~/bin/llvm/Debug/bin:~/bin/llvm/tools/clang/utils:~/bin/llvm/tools/clang/tools/scan-view
# For java apps that misbehave
export AWT_TOOLKIT=MToolkit

# For git-achievements
export PATH="$PATH:~/bin/git-achievements"

# The bash history file should save last 10000 commands. Default is 500.
export HISTFILESIZE=10000
# The number of commands to remember in the in-memory command history, as
# reported by the 'history' built-in. Default is 500.
export HISTSIZE=1000
# Don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoreboth

PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

