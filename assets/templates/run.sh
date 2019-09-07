#!/bin/bash

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                              _                                          #
#                             | |                                         #
#                 _ __    ___ | |_  _ __    __ _ __      __               #
#                | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /               #
#                | | | ||  __/| |_ | |_) || (_| | \ V  V /                #
#                |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                 #
#                                  | |                                    #
#                                  |_|                                    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                         #
# File contributors:                                                      #
#   - M.A. Constantin                                                     #
#                                                                         #
# File description:                                                       #
#   - bash template for the simulation `Setup` class                      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# Assume that tmux and htop are installed.

# Declare the array of scripts to run.
# E.g., replace `{{ scripts }}` with `"simulator_range_1_to_20.R" "simulator_range_21_to_40.R"`.
declare -a scripts=({{scripts}})

# Create variable for the tmux window number.
declare -i i=1

# Run the install script.
Rscript ./scripts/install.R

# Ask for user confirmation that the installation went fine.
read -n1 -rsp $'Press any key to continue or `Ctrl+C to` exit...\n'

# Declare the tmux session name.
sessionname=sims

# Create tmux session with default window for monitoring.
tmux new-session -s "$sessionname" -n monitor -d "htop"

# Iterate over the R scripts.
for script in "${scripts[@]}"
do
    # Spawn a window for each script.
    tmux new-window -t "$sessionname:$i" -n "sim" "Rscript ./scripts/$script;bash -i"

    # Increment window number.
    i=$i+1
done

# Select monitor window 
tmux select-window -t "$sessionname:0"

# Attach to the session.
tmux -2 attach-session -t "$sessionname"
