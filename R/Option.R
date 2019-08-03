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
#   - contains a class that defines an option                             #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Option class ------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Option <- R6::R6Class("Option",

    private = list(
        ..values = NULL,
        ..meta = NULL,


        # Try to guess which class has been inherited.
        ..guess.ancestor = function() {
            # Get the last words that make up the camel case type.
            words = strsplit(private$..meta$type, "([a-z])(?=[A-Z])", perl = TRUE)[[1]]

            # Get the last word which represents the inherited class.
            word = tolower(words[length(words)])

            return(word)
        }
    ),


    public = list(
        # Constructor
        initialize = function(values = NULL, meta = NULL) {
            # Set fields.
            private$..values <- self$set.values(values)
            private$..meta <- self$set.meta(meta)
        },


        # Flatten object to a named list.
        to.list = function() {
            # Determine the name.
            name = private$..guess.ancestor()

            # Prepare the simplified list.
            simple = list()

            # Append option and meta values.
            simple[[name]] = private$..values
            simple[[name]]$timestamp = private$..meta$timestamp

            return(simple)
        },


        # Setters.
        set.values = function(values) {
            private$..values <- values
        },


        set.meta = function(meta) {
            # Check types.
            if(!is.null(meta)) assert("Meta" %in% class(meta), ..ERRORS..$incorrect.object.type)

            private$..meta <- meta
        }
    ),


    active = list(
        values = function() {
            return(private$..values)
        },


        meta = function() {
            return(private$..meta)
        }
    )
)



# End of file.
