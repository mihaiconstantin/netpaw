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
#   - contains a class that defines a model comparison outcome            #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Outcome class -----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Outcome <- R6::R6Class("Outcome",

    public = list(
        values = NULL,


        # Constructor.
        initialize = function(values) {
            # Set the values from passed values.
            self$values <- values
        },


        # Print.
        print = function(api = TRUE) {
            # General details.
            cat(crayon::bold("Outcome:"))
            cat("\n")
            cat("  -", paste(paste(names(self$values), ":", sep = ""), crayon::yellow(self$values), collapse = "\n  - "))
            cat("\n")

            # API details.
            if(api) print.class.api(Outcome)
        }
    )
)



# End of file.
