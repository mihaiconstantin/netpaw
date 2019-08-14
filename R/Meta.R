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
#   - contains a class that defines the meta data                         #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include TypeHandler.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Meta class --------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Meta <- R6::R6Class("Meta",

    private = list(
        ..timestamp = NULL,
        ..type = NULL,
        ..ancestor = NULL,
        ..alias = NULL
    ),


    public = list(
        # Constructor.
        initialize = function(type = NULL, handler = TypeHandler) {
            # Type check.
            assert(class(handler) == "R6ClassGenerator", ..ERRORS..$incorrect.object.type)

            # Instantiate the handler.
            handler <- handler$new(type)

            # Record timestamp.
            private$..timestamp <- Sys.time()

            # Set the class fields from the handler.
            private$..type <- type
            private$..ancestor <- handler$ancestor
            private$..alias <- handler$alias
        },


        # Flatten object to list.
        to.list = function() {
            return(list(
                timestamp = private$..timestamp,
                ancestor = private$..ancestor,
                type = private$..type,
                alias = private$..alias
            ))
        }
    ),


    active = list(
        timestamp = function() {
            return(private$..timestamp)
        },


        type = function() {
            return(private$..type)
        },


        ancestor = function() {
            return(private$..ancestor)
        },


        alias = function() {
            return(private$..alias)
        }
    )
)



# End of file.
