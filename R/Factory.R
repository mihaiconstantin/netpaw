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
#   - this file contains a simple factory used to generate an arbitrary   #
#     amount of R6 objects                                                #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Factory of object -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Factory <- R6::R6Class("Factory",

    private = list(
        ..warehouse = list()
    ),


    public = list(
        initialize = function(object, ..., amount = 1) {
            # Enforce a type R since R misses strong typed arguments.
            stopifnot(class(object) == "R6ClassGenerator")
            
            for(i in 1:amount) {
                private$..warehouse[[i]] <- object$new(...)
            }
        }
    ),


    active = list(
        warehouse = function() {
            return(private$..warehouse)
        },


        size = function() {
            return(length(private$..warehouse))
        },


        first = function() {
            return(private$..warehouse[[1]])
        }
    )
)



# End of file.
