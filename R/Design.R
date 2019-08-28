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
#   - contains a class that defines a simulation design                   #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Design class ------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Design <- R6::R6Class("Design",

    private = list(
        ..options = NULL,
        ..replications = NULL,
        ..structure = list(),


        # Boilerplate.
        ..boot = function() {
            # Prepare the Option object and set the meta field.
            private$..options <- Option$new(meta = Meta$new(type = class(self)[1]))

            # Set the values field on the options at runtime.
            patch.function.within.environment("..set.structure", private, "private$..options$set.values(combine.arguments(private$..set.structure, as.list(match.call())[-1]))")
        },


        # Pure virtual function for setting the design structure
        ..set.structure = function(...) {
            stop(..ERRORS..$non.instantiable.class)
        },


        # Pure virtual function for setting the replications.
        ..set.replications = function() {
            stop(..ERRORS..$non.instantiable.class)
        }
    ),


    public = list(
        # Constructor.
        initialize = function(...) {
            # Boot.
            private$..boot()

            # Set the design structure.
            private$..set.structure(...)

            # Set the replications.
            private$..set.replications()
        },


        # Print.
        print = function() {
            # General details.
            cat(crayon::black$bgGreen$bold("Simulation design details:"))
            cat("\n")
            cat("  - replications:", crayon::yellow(private$..replications))
            cat("\n")

            # Model details.
            for (model in private$..structure$model) {
                cat("  - model ", model$alias, ":", sep = "")
                cat("\n")
                
                # Step details.
                for(step in names(model)[names(model) != "alias" & names(model) != "graph"]) {
                    cat("    -", step, "options:")
                    cat("\n")

                    for (option in names(model[[step]])) {
                        cat("      - ", option, ": ", paste(crayon::yellow(model[[step]][[option]]), collapse = crayon::silver(" | ")), sep = "")
                        cat("\n")
                    }
                }

                # Graph details.
                for(graph in names(model$graph)) {
                    cat("    - graph ", graph, ":", sep = "")
                    cat("\n")

                    for(option in names(model$graph[[graph]])) {
                        cat("      - ", option, ": ", paste(crayon::yellow(model$graph[[graph]][[option]]), collapse = crayon::silver(" | ")), sep = "")
                        cat("\n")
                    }
                }
            }
        }
    ),


    active = list(
        structure = function() {
            return(private$..structure)
        },


        options = function() {
            return(private$..options)
        },


        replications = function() {
            return(private$..replications)
        }
    )
)



# End of file.
