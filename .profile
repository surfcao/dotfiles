# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

#JAVA_HOME=/usr/lib/jvm/java-6-sun

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
if [ -d "$HOME/mytools" ] ; then
    PATH="$HOME/mytools:$PATH"
fi
#export OFFICE=141.142.220.247
#export cgvm3=cgvm3.cigi.uiuc.edu
export TTU=129.118.32.65
export TTUHOMEPAGE=raiderdrive.tosm.ttu.edu/users/gucao
export EDITOR=vim
export w=$HOME/Dropbox/work/ttu/grants/big-data
export uc=$HOME/Dropbox/work/ucsb
export ui=$HOME/Dropbox/work/ui
export tt=$HOME/Dropbox/work/ttu
#alias t='python ~/mytools/t/t.py --task-dir ~/Dropbox/tasks --list tasks'
export XDG_CONFIG_HOME=$HOME/.config

#for autojump 
[[ -s /home/guofeng/.autojump/etc/profile.d/autojump.sh ]] && source /home/guofeng/.autojump/etc/profile.d/autojump.sh 

#alias list

alias sudo='sudo '
alias data='mount -t cifs //labmanager/e$ ~/mnt/labmanager -o user=gucao'
alias labs='mount -t cifs //labmanager/d$ ~/mnt/labs -o user=gucao'
alias class='mount -t cifs //geospatial/GIST4302 ~/mnt/classpage -o user=gucao'
alias homepage='mount -t cifs //raiderdrive.tosm.ttu.edu/users/gucao/ ~/mnt/homepage -o user=gucao'

alias ml='matlab -nodesktop -nosplash'
# reboot / halt / poweroff
alias reboot='sudo /sbin/reboot'
alias poweroff='sudo /sbin/poweroff'
alias halt='sudo /sbin/halt'
alias shutdown='sudo /sbin/shutdown'
alias update='sudo apt-get update'
alias upgrade='sudo apt-get upgrade'
alias dallas='rdesktop -g 100% -P -z 129.118.31.144'
alias labmanager='rdesktop -g 100% -P -z labmanager'
alias cgstdb='rdesktop -g 100% -P -z cgstdb.tosm.ttu.edu'

#suppress warning messages.
export NO_AT_BRIDGE=1
