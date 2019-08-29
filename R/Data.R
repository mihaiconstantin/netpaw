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
#   - contains a class that defines the data                              #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data class --------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Data <- R6::R6Class("Data",

    public = list(
        dataset = NULL,


        # Constructor.
        initialize = function(dataset = NULL) {
            # Set fields.
            self$dataset <- dataset
        },


        # Print.
        print = function(api = TRUE) {
            # General details.
            cat(crayon::bold("Data:"))
            cat("\n")
            cat("  - dimensions:", crayon::yellow(paste0(nrow(self$dataset), "x", ncol(self$dataset))))
            cat("\n")

            # API details.
            if(api) print.class.api(Data)
        }
    )
)



# End of file.
