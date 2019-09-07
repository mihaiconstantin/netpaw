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
#   - contains a class that defines the simulation setup                  #
#   - TODO: this class has a bit of hard coding. When I have more time    #
#           I should revisit and refactor.                                #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Setup class -------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Setup <- R6::R6Class("Setup",

    private = list(
        ..simulator = NULL,
        ..cores = NULL,
        ..os = NULL,
        ..path = NULL,
        ..callback = NULL,
        ..shell = NULL,
        ..ranges = list(),
        ..splits = list(),
        ..expressions = list(),


        # Boilerplate.
        ..boot = function(simulator, cores, os, path, callback) {
            # Type check.
            assert("Simulator" %in% class(simulator), ..ERRORS..$incorrect.object.type)

            # Set the simulator.
            private$..simulator <- simulator

            # Set the cores.
            if(is.null(cores)) private$..cores <- parallel::detectCores() - 1 else private$..cores <- cores

            # Set the OS.
            if(is.null(os)) private$..os <- .Platform$OS.type else private$..os <- os

            # Set the path where to write the setup.
            if(is.null(path)) private$..path <- getwd() else private$..path <- path

            # Set the callback.
            private$..callback = callback

            # Create ranges to match the number of cores.
            private$..set.ranges()

            # Split the simulator.
            private$..set.splits()

            # Prepare install script.
            private$..set.install.expression()

            # Prepare console scripts.
            private$..set.simulator.expressions()
        },


        # Internal setters for the fields that are used to write the setup fields.


        # Set the ranges according to the number of cores.
        ..set.ranges = function() {
            # If there are simulations than number of cores.
            if(private$..simulator$total < private$..cores) {
                private$..ranges <- c(private$..ranges, list(1:private$..simulator$total))

            # Create approximately equal splits.
            } else {
                # Split in approximately equal groups.
                private$..ranges <- split(1:private$..simulator$total, cut(1:private$..simulator$total, quantile(1:private$..simulator$total, (0:private$..cores) / private$..cores), include.lowest = TRUE, labels = FALSE))
            }

            # Set informative names for the ranges.
            names(private$..ranges) <- sapply(private$..ranges, function(range) { 
                paste0("simulator_range_", range[1], "_to_", range[length(range)]) 
            })
        },


        # Clone (i.e., not deep because we want to keep the pointer associated to each simulation instance) and split the simulator accordingly.
        ..set.splits = function() {
            # For each range create a clone of the simulator and pop the redundant simulations.
            for (name in names(private$..ranges)) {
                # Shallow clone the simulator.
                private$..splits[[name]] <- private$..simulator$clone()

                # Keep only the simulations in the current range.
                private$..splits[[name]]$keep(private$..ranges[[name]])
            }
        },


        # Prepare and set the expression for the R script file that will handle the update.
        ..set.install.expression = function() {
            # Write the update expression. Note that `quote` with `{}` can be used instead of `expression`. See: https://stackoverflow.com/a/40164111/5252007.
            private$..expressions[["install"]] <- expression(
                # Set package options.
                options("netpaw" = list(logo.colors = FALSE)),

                # Remove previous installation.
                try(remove.packages("netpaw")),

                # Check if `devtools` is needed.
                if(!("devtools" %in% rownames(installed.packages()))) install.packages("devtools"),

                # Install most recent version.
                devtools::install_github("mihaiconstantin/netpaw@dev"),

                # Test that the package can be loaded.
                library(netpaw),

                # Inform about the update.
                cat("\n", "Installation successful.", "\n\n", sep = "")
            )
        },


        # Prepare and set the expressions for the R script files that will invoke the simulator.
        ..set.simulator.expressions = function() {
            for(name in names(private$..ranges)) {
                # Construct the function that will run the simulator.
                simulate <- function(simulator.load.path, simulator.save.path, callback) {
                    # Set package options.
                    options("netpaw" = list(logo.colors = FALSE))

                    # Load the library.
                    library(netpaw)

                    # Load the specific simulator object.
                    simulator <- readRDS(paste0(getwd(), simulator.load.path))

                    # No matter what, do the following.
                    on.exit({
                        # Save the simulator.
                        saveRDS(simulator, file = paste0(getwd(), simulator.save.path))

                        # Provide feedback about where the simulator was saved.
                        cat("\n", "Simulator was saved at `", getwd(), simulator.save.path, "`.", "\n", sep = "")
                    })

                    # Run the simulator.
                    simulator$run(callback = callback)
                }

                # Construct the expression.
                private$..expressions[[name]] <- as.expression(as.call(list(
                    simulate = simulate,
                    simulator.load.path = paste0("/scripts/objects/", name, ".rds"),
                    simulator.save.path = paste0("/output/simulators/", name, "_completed.rds"),
                    callback = private$..callback
                )))
            }
        },


        # Creators of directories and scripts based on the class fields.


        # Create directory structure for the scripts.
        ..create.directories = function() {
            # Create script and input simulators directory.
            dir.create(paste0(private$..path, "/", "setup/scripts/objects"), recursive = TRUE, showWarnings = FALSE)

            # Output simulator objects directory.
            dir.create(paste0(private$..path, "/", "setup/output/simulators"), recursive = TRUE, showWarnings = FALSE)

            # Output simulations objects directory.
            dir.create(paste0(private$..path, "/", "setup/output/simulations"), recursive = TRUE, showWarnings = FALSE)

            # Output misc directory.
            dir.create(paste0(private$..path, "/", "setup/output/misc"), recursive = TRUE, showWarnings = FALSE)
        },


        # Create the R scripts.
        ..create.scripts = function() {
            # Each expression gets its file.
            for(name in names(private$..expressions)) {
                # Dump the expression to the file.
                dump.contents(paste0(private$..path, "/", "setup/scripts/", name, ".R"), paste0(private$..expressions[[name]], collapse = "\n"))
            }
        },


        # Create the split simulator objects used for running the simulations.
        ..create.objects = function() {
            for(name in names(private$..splits)) {
                # Save the split simulator as an R object.
                saveRDS(private$..splits[[name]], file = paste0(private$..path, "/", "setup/scripts/objects/", name, ".rds"))
            }
        },


        # Create shell script to wrap the R scripts.
        ..create.shell = function() {
            # Create the shell according to the `os` field.
            if(private$..os == "windows") { private$..windows.shell() } else { private$..linux.shell() }

            # Create the file with the shell code.
            dump.contents(file = paste0(private$..path, "/setup/run.ps1"), private$..shell)
        },


        # Shell types.


        # Shell script for running the simulations on windows.
        ..windows.shell = function() {
            # Load the template for the Windows Powershell script.
            private$..shell <- readLines("./assets/templates/run.ps1")

            # Replace the placeholder with the scripts.
            private$..shell <- gsub("{{scripts}}", paste0("\"", names(private$..ranges), ".R\"", collapse = ", "), private$..shell, perl = TRUE)
        },


        # Shell script for running the simulations on linux.
        ..linux.shell = function() {
            # TODO: implement.
            stop(..ERRORS..$not.implemented)
        },


        # Setup writer.


        # Write the setup folders, objects and scripts.
        ..write.setup = function() {
            # Create folders.
            private$..create.directories()

            # Create split simulator objects.
            private$..create.objects()

            # Create R scripts.
            private$..create.scripts()

            # Create the shell.
            private$..create.shell()
        }
    ),


    public = list(
        # Constructor.
        initialize = function(simulator, cores = NULL, os = NULL, path = NULL, callback = NULL) {
            # Boot.
            private$..boot(simulator, cores, os, path, callback)

            # Write setup.
            private$..write.setup()
        },


        # Print.
        print = function() {
            # General details.
            cat(crayon::bold("Simulation setup:"))
            cat("\n")
            cat("  - os:", crayon::yellow(private$..os))
            cat("\n")
            cat("  - path:", crayon::yellow(private$..path))
            cat("\n")
            cat("  - simulations:", crayon::yellow(private$..simulator$total))
            cat("\n")
            cat("  - cores:", crayon::yellow(private$..cores))
            cat("\n")
            cat("  - splits:", crayon::yellow(length(private$..splits)))
            cat("\n")
            cat("  - ranges:", paste(crayon::yellow(private$..ranges), collapse = crayon::silver(" | ")))
            cat("\n")

            # API details.
            print.class.api(Setup)
        }
    ),


    active = list(
        simulator = function() {
            return(private$..simulator)
        },


        cores = function() {
            return(private$..cores)
        },


        os = function() {
            return(private$..os)
        },


        path = function() {
            return(private$..path)
        },


        shell = function() {
            return(private$..shell)
        },


        callback = function() {
            return(private$..callback)
        },


        ranges = function() {
            return(private$..ranges)
        },


        splits = function() {
            return(private$..splits)
        },


        expressions = function() {
            return(private$..expressions)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exported wrapper for setup API ------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
setup <- function(simulator, cores = NULL, os = NULL, path = NULL, callback = NULL) {
    # Build the design.
    setup <- Setup$new(simulator, cores = cores, os = os, path = path, callback = callback)

    return(setup)
}



# End of file.
