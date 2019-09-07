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



# Declare the array of scripts to run.
# E.g., replace `{{ scripts }}` with `"simulator_range_1_to_10.R", "simulator_range_11_to_20.R"`.
$scripts = @({{scripts}})

# Window title.
$host.ui.rawui.WindowTitle = "Installing package..."

# Install the package
Rscript.exe ./scripts/install.R

# Update the window title.
$host.ui.rawui.WindowTitle = "Package installed"

# Ask for confirmation that the installation went fine.
pause

# Update window title.
$host.ui.rawui.WindowTitle = "Engaging simulators..."

# Invoke the simulator scripts.
foreach ($script in $scripts) {
    # Start a process. See: https://stackoverflow.com/a/17777205/5252007.
    Start-Process powershell -ArgumentList ("`$host.ui.rawui.WindowTitle = '$script'.replace('_', ' ').replace('.R', ''); Rscript.exe ./scripts/'$script'; pause")
}
