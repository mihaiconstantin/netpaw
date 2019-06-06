# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                             _                                                                           #
#                                                            | |                                                                          #
#                                                _ __    ___ | |_  _ __    __ _ __      __                                                #
#                                               | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /                                                #
#                                               | | | ||  __/| |_ | |_) || (_| | \ V  V /                                                 #
#                                               |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                                                  #
#                                                                 | |                                                                     #
#                                                                 |_|                                                                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                                                                                         #
# File contributors:                                                                                                                      #
#   - M.A. Constantin                                                                                                                     #
#                                                                                                                                         #
# File description:                                                                                                                       #
#   - this file contains constants used throughout the entire package                                                                     #
#                                                                                                                                         #
# Classes/ functions:                                                                                                                     #
#   - n.a.                                                                                                                                #
#                                                                                                                                         #
# Additional information:                                                                                                                 #
#   - n.a.                                                                                                                                #
#                                                                                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Graphics ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

..GRAPHICS.. <- list(
    positive.edge.color = "#3F51B5",
    negative.edge.color = "#F44336"
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Errors ------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

..ERRORS.. <- list(
    # Occurrences:
    #   - when a parent class with generator is instantiated
    non.instantiable.class = "Class not meant to be instantiated. Use it as an abstract class.",

    # Occurrences:
    #   - when a type is requested by string (i.e., graph or model alias) and no implementation exists.
    unsupported.type = "Requested type is not supported. You may request it at: https://github.com/mihaiconstantin/netpaw/issues.",

    # Occurrences:
    #   - when a object type other than what is expected was provided via the dependency injection
    incorrect.object.type = "One or more injected arguments are not instances of expected class.",

    # Occurrences:
    #   - when running into things not yet implemented (e.g., time series models)
    not.implemented = "Not yet implemented. Most likely it is under development.",

    # Occurrences:
    #   - when running into things that must have an override (e.g., `generator` methods)
    not.overwritten = "Pure virtual function is not overwritten."

    # Occurrences:
    #   - n.a.
)
