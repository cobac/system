if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi

export TERM=xterm-256color
export KEYTIMEOUT=1
export PATH=$PATH:/usr/local/texlive/2020/bin/x86_64-linux:/home/coba/.nix-profile/bin:/home/coba/.local/bin
export TEXMFDIST=/usr/local/texlive/2020/texmf-dist/


# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=2000
SAVEHIST=2000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install

# The following lines were added by compinstall
zstyle :compinstall filename '/home/coba/.zshrc'

# End of lines added by compinstall
export BROWSER="firefox"

autoload -Uz promptinit
promptinit

#Powerline10k
source ~/.config/zsh/powerlevel10k/powerlevel10k.zsh-theme
source ~/.config/zsh/prompt.zsh
#source ~/.config/zsh/p10k.zsh
	
# fzf
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

#Aliases
alias v="nvim"
alias rn="ranger"
alias ls="ls --color=auto"
alias e="emacsclient -n"
alias kali="sudo docker run -ti kalilinux/kali-rolling /bin/bash"
alias emacsdre="systemctl --user stop emacs.service && echo 'Stopped' && systemctl --user enable --now emacs.service"
alias pacmanS="pacman -Slq | fzf --multi --preview 'cat <(pacman -Si {1}) <(pacman -Fl {1} | awk \"{print \$2}\")' | xargs -ro sudo pacman -S"
alias sshp="ssh -L 9090:127.0.0.1:8384 -p 48172 168.119.49.137"
alias loopon="pactl load-module module-loopback"
alias loopoff="pactl unload-module module-loopback"
alias tl="tldr --list | fzf --preview 'tldr {} --color always' | xargs tldr"
alias muinitcoba="mu init --maildir=/home/coba/.Mail/ --my-address=coba@cobac.eu --my-address=cosas@cobac.eu"
alias mm="make"
alias gg="gotop"
alias pydataenv="source ~/.virtualenvs/datastuff/bin/activate"
alias ansiblep="ansible-pull -U https://github.com/cobac/system.git --ask-become-pass -vv"


# Completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix
autoload -Uz compinit && compinit -i

setopt extended_glob

# Custom functions

extract() {
    if [ -f $1 ] ; then
	case $1 in
	    *.tar.bz2)   tar xjvf $1 ;;
	    *.tar.gz)    tar xzvf $1 ;;
	    *.tar.xz)    tar xJvf $1 ;;
	    *.bz2)       bunzip2 $1 ;;
	    *.rar)       unrar xv $1 ;;
	    *.gz)        gunzip $1 ;;
	    *.tar)       tar xvf $1 ;;
	    *.tbz2)      tar xjvf $1 ;;
	    *.tgz)       tar xzvf $1 ;;
	    *.zip)       unzip $1 ;;
	    *.7z)        7z $1 ;;
	    *.xz)        xz -vd $1 ;;
	    *)           echo "'$1' cannot be extracted via extract()" ;;
	esac
    else
	echo "'$1' is not a valid file"
    fi
}

# Required for libvterm
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

purge-pdf(){
exiftool -all:all= $1 -overwrite_original &&
qpdf --linearize --replace-input $1
}
