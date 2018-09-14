# Tmux notes

## Tricks

1. Tmux can not display Powerline symbols.

   Reason: Tmux does not use utf-8 defaultly.

   Solution:

   ```shell
   ~ export LANG=en_US.UTF-8
   ~ export LC_CTYPE=en_US.UTF-8
   ~ tmux
   
   # or
   ~ tmux -u
   ```

   Ref: https://github.com/wernight/powerline-web-fonts/issues/8