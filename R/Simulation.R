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
#   - contains a class that defines a simulation                          #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Simulation class --------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Simulation <- R6::R6Class("Simulation",

    private = list(
        ..config = NULL,
        ..result = NULL,
        ..completed = NULL
    ),


    public = list(
        # Constructor.
        initialize = function(config) {
            # Type check.
            assert("Config" %in% class(config), ..ERRORS..$incorrect.object.type)

            # Set fields.
            private$..config <- config
        },


        # Convert a config to a simulation procedure and perform it.
        perform = function() {
            # Runs based on a config.
        }
    ),


    active = list(
        config = function() {
            return(private$..config)
        },


        result = function() {
            return(private$..result)
        },


        completed = function() {
            return(private$..completed)
        }
    )
)



# End of file.
