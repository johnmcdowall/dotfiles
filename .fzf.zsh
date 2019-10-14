# Setup fzf
# ---------
if [[ ! "$PATH" == */home/jmd/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/jmd/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/jmd/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/jmd/.fzf/shell/key-bindings.zsh"
