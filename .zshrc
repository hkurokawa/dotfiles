# 少し凝った zshrc
zmodload zsh/zprof
 
########################################
# 環境変数
export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8
 
 
# 色を使用出来るようにする
autoload -Uz colors
colors
 
# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
 
# プロンプト
# 1行表示
# PROMPT="%~ %# "
# 2行表示
PROMPT="%{${fg[red]}%}[%n@%m]%{${reset_color}%} %~
%# "
 
 
# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default
# ここで指定した文字は単語区切りとみなされる
# / も区切りと扱うので、^W でディレクトリ１つ分を削除できる
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified
 
########################################
# 補完
# 補完機能を有効にする
autoload -Uz compinit
compinit
 
# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
 
# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..
 
# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                   /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
 
# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'
 
 
########################################
# vcs_info
 
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%1(v|%F{green}%1v%f|)"
 
 
########################################
# オプション
# 日本語ファイル名を表示可能にする
setopt print_eight_bit
 
# beep を無効にする
setopt no_beep
 
# フローコントロールを無効にする
setopt no_flow_control

# '#' 以降をコメントとして扱う
setopt interactive_comments
 
# ディレクトリ名だけでcdする
setopt auto_cd

# コマンドのスペルミスを自動補正
setopt correct

# ファイルを間違えて上書きしない
setopt noclobber

# シングルクォート内のシングルクォートをダブルクォートで代用できるようにする
setopt rcquotes

# cd したら自動的にpushdする
setopt auto_pushd

# 重複したディレクトリを追加しない
setopt pushd_ignore_dups
 
# = の後はパス名として補完する
setopt magic_equal_subst
 
# 同時に起動したzshの間でヒストリを共有する
setopt share_history
 
# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups
 
# ヒストリファイルに保存するとき、すでに重複したコマンドがあったら古い方を削除する
setopt hist_save_nodups
 
# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space
 
# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks
 
# 補完候補が複数あるときに自動的に一覧表示する
setopt auto_menu
 
# 高機能なワイルドカード展開を使用する
setopt extended_glob
 
########################################
# キーバインド
 
# ^R で履歴検索をするときに * でワイルドカードを使用出来るようにする
bindkey '^R' history-incremental-pattern-search-backward
 
########################################
# エイリアス
 
alias la='ls -a'
alias ll='ls -l'
alias lt='ls -latr'
 
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias cd='pushd'
alias pd='popd'
alias ch='cd $HOME'
 
alias mkdir='mkdir -p'
 
if [[ -x `which colordiff` ]]; then
  alias diff='colordiff -u'
else
  alias diff='diff -u'
fi

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '
 
alias odx='od -x -w16 -A x'
alias csv2table='for line in $(sed -e '"'"'s/, */ /g'"'"'); do echo ${line}; done'
alias cl=clear

alias cdir='cd $(find $* | peco)' 
# AWS
alias awls="aws ec2 describe-instances --output text --query 'Reservations[*].Instances[*].[State.Name, Tags[?Key==\`Name\`].Value | [0], PublicDnsName, PrivateDnsName]' | sed -e 's/^running/R/g' | sed -e 's/^stopped/S/g' | peco"
alias awls-vpc="aws ec2 describe-instances --output text --region ap-southeast-1 --filter 'Name=vpc-id,Values=vpc-457e9320' --query 'Reservations[*].Instances[*].[State.Name, Tags[?Key==\`Name\`].Value | [0], PrivateIpAddress]' | sed -e 's/^running/R/g' | sed -e 's/^stopped/S/g' | peco"
alias awssh='row=$(awls); name=$(echo ${row} | cut -f 2); host=$(echo ${row} | cut -f 3); echo "...ssh ${name} (${host})"; ssh ${host}'
alias awsshi='row=$(awls); name=$(echo ${row} | cut -f 2); host=$(echo ${row} | cut -f 4); echo "...ssh ${name} (${host})"; ssh ${host}'
alias awssh-vpc='row=$(awls-vpc); name=$(echo ${row} | cut -f 2); host=$(echo ${row} | cut -f 3); echo "...ssh ${name} (${host})"; ssh ${host}'

# Android
alias lsavds="android list avds | grep -oe 'Name: [a-zA-Z0-9_]*' | sed -e 's/Name: //' | peco"
alias em='avd=$(lsavds); echo "..running emulator -avd ${avd}"; emulator -avd ${avd}'
# Uncomment this when you found the equivalent way to do getifaddr on Ubuntu
#alias em-prox="em -http-proxy http://$(ipconfig getifaddr en0):8888"
alias adb-pkill='adb shell kill $(adb shell ps | peco | tr -s '"'"' '"'"' '"'"'\t'"'"' | cut -f 2)'
alias lspkgs="adb shell dumpsys activity | grep -B 1 \"Run #[0-9]*:\" | grep -oe '\(I\|A\)=[a-z.]*' | sed -e 's/^.*=//' | peco"

# Git and ghi
command -v brew > /dev/null 2>&1
if [ ! $? ]; then
    fpath=($(brew --prefix)/share/zsh/site-functions $fpath)

    autoload -U compinit
    compinit -u
elif [ -d ~/.zsh/completion ]; then
    fpath=(~/.zsh/completion $fpath)

    autoload -U compinit
    compinit -u
fi

alias g=git
alias g-rmbranch='git branch --delete $(git branch --merged master | egrep -v "^\\s*master\\s*$" | peco)'
alias g-co='git checkout $(git branch -a | peco | sed -e '"'"'s|^remotes/[^/]*/\(.*\)$|\1|g'"'"')'
alias g-ls='$(git st -s | cut -d '"'"' '"'"' -f 3 | peco)'
alias g-ad='git add $(g-ls)'
alias g-dftag='git log $(git tag | peco)...$(git tag | peco) --oneline'
alias find-code='find . -type f \( -name "*.java" -o -name "*.go" -o -name "*.py" -o -name "*.c" \)'
alias g-log-source='git log -p $(find-code | peco)'
alias ghi-issno='ghi list | peco | sed -e '"'"'s/^ *\([0-9][0-9]*\).*$/\1/g'"'"
alias ghi-milno='ghi milestone | peco | sed -e '"'"'s/^ *\([0-9][0-9]*\):.*$/\1/g'"'"
alias g-delete-merged='git branch --merged | grep -v "\*" | grep -v master | grep -v dev | xargs -n 1 git branch -d'
if which hub > /dev/null; then eval $(hub alias -s); fi

# ghq
alias cr='cd $(ghq list -p | peco)'

# グローバルエイリアス
alias -g L='| less'
alias -g G='| grep'
 
# # Setup for qfc (https://github.com/pindexis/qfc)
# [[ -s "$HOME/.qfc/bin/qfc.sh" ]] && source "$HOME/.qfc/bin/qfc.sh"
# if which qfc_quick_command > /dev/null; then
#   qfc_quick_command 'cd' '\C->' 'cd $0'
#   qfc_quick_command 'vi' '\C-<' 'vi $0'
# fi

# C で標準出力をクリップボードにコピーする
# mollifier delta blog : http://mollifier.hatenablog.com/entry/20100317/p1
if which pbcopy >/dev/null 2>&1 ; then
    # Mac
    alias -g C='| pbcopy'
elif which xsel >/dev/null 2>&1 ; then
    # Linux
    alias -g C='| xsel --input --clipboard'
elif which putclip >/dev/null 2>&1 ; then
    # Cygwin
    alias -g C='| putclip'
fi

if which xsel > /dev/null 2>&1 ; then
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
fi


#########################################
# Functions
copy_structure() {
    if [ $# -ne 3 ]; then
        echo "Usage: $0 src dest filename" 1>&2
        return 1
    fi
    local src=$1
    local dst=$2
    local name=$3

    if [ ! -d "${src}" ]; then
        echo "No such dir: ${src}" 1>&2
        return 1
    fi
    if [ ! -d "${dst}" ]; then
        echo "No such dir: ${dst}" 1>&2
        return 1
    fi
    if [ -z "${name}" ]; then
        echo "Empty name: ${name}"
        return 1
    fi
    (cd "${src}"; find . -iname "${name}" | cpio -pdmu "${dst}")
}

########################################
# OS 別の設定
case ${OSTYPE} in
    darwin*)
        #Mac用の設定
        export CLICOLOR=1
        alias ls='ls -G -F'
        ;;
    linux*)
        #Linux用の設定
        ;;
esac
 
# vim:set ft=zsh:

#########################################
# Git
RPROMPT="%{${fg[blue]}%}[%~]%{${reset_color}%}"

autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "%F{green}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () { vcs_info }
RPROMPT=$RPROMPT'${vcs_info_msg_0_}'

#########################################
# Go
export GOPATH=$HOME/go
export PATH=$PATH:/usr/local/go/bin:${GOPATH//://bin:}/bin

#########################################
# Android
export ANDROID_SDK=$HOME/android-sdk
export ANDROID_NDK=$HOME/android-ndk
export ANDROID_HOME=$ANDROID_SDK
export PATH=$PATH:${ANDROID_SDK}/platform-tools:${ANDROID_SDK}/tools:${ANDROID_NDK}
export USE_CCACHE=1

#########################################
# rbenv
if which rbenv > /dev/null; then
    export RBENV_ROOT=${HOME}/.rbenv
    eval "$(rbenv init -)"
fi

#########################################
# nodebrew
export PATH=$HOME/.nodebrew/current/bin:$PATH

#########################################
# Utils
export PATH=$PATH:$HOME/bin

# The next line updates PATH for the Google Cloud SDK.
[[ -s "$HOME/google-cloud-sdk/path.zsh.inc" ]] && source "$HOME/google-cloud-sdk/path.zsh.inc"

# The next line enables shell command completion for gcloud.
[[ -s "$HOME/google-cloud-sdk/completion.zsh.inc" ]] && source "$HOME/google-cloud-sdk/completion.zsh.inc"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/usr/local/opt/share/google-cloud-sdk/path.zsh.inc' ]; then . '/usr/local/opt/share/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/usr/local/opt/share/google-cloud-sdk/completion.zsh.inc' ]; then . '/usr/local/opt/share/google-cloud-sdk/completion.zsh.inc'; fi

# Yarn
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# NVM
#export NVM_DIR="$HOME/.nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
#if which direnv > /dev/null; then
#  eval "$(direnv hook bash)"
#fi

# SML/NJ
PATH=/usr/share/smlnj/bin:$PATH

# Print out load stats
#zprof
