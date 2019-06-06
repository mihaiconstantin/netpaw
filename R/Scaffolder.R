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
#   - contains an R6 class used to scaffold the addition of new graphs    #
#     and models to the package                                           #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Parent graph class ------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Scaffolder <- R6::R6Class("Scaffolder",

    private = list(
        # Meta data fields.
        ..time = NULL,
        ..type = NULL,
        ..template.path = NULL, 
        ..template.content = NULL, 
        ..output.path = NULL,


        ..select.template.path = function() {
            # Select the appropriate template.
            switch(private$..type,
                graph = {
                    file.template <- "graph.Rt"
                },

                model = {
                    file.template <- "model.Rt"
                },

                {
                    stop(..ERRORS..$unsupported.type)
                }
            )

            # Load the template.
            private$..template.path <- paste("./assets/templates/", file.template, sep = "")
        },


        ..determine.output.path = function(name) {
            # Specify the file extension.
            file.ext = ".R"

            # Construct the file name.
            file.name <- paste(capitalize(name), capitalize(private$..type), sep = "")

            # Construct the path.
            path <- paste("./R/", file.name, file.ext, sep = "")

            # Check if the file exists and make the name unique.
            if(file.exists(path)) {
                # Get the timestamp in seconds.
                timestamp <- round(as.numeric(private$..time), 0)

                # Some user feedback.
                cat("File", shQuote(crayon::yellow(path)), "already exists. Appending", shQuote(crayon::yellow(timestamp)), "to its name.\n")

                # Renaming the file and creating the full path.
                path <- paste("./R/", file.name, "_", timestamp ,file.ext, sep = "")
            }

            # Store the output path.
            private$..output.path <- path
        },


        ..load.template = function() {
            # First select the appropriate template path.
            private$ ..select.template.path()
            
            # Load the contents at that path.
            private$..template.content <- readLines(private$..template.path)
        },


        ..update.template = function(name, alias, author, doi, model.type = NULL) {
            # TODO: fix bug resulting from nchar(pattern) != nchar(replacement).

            # Common updates.
            private$..template.content <- gsub("{{name}}",     capitalize(name),  private$..template.content, perl = TRUE)
            private$..template.content <- gsub("{{alias}}",    tolower(alias),    private$..template.content, perl = TRUE)
            private$..template.content <- gsub("{{author}}",   author,            private$..template.content, perl = TRUE)
            private$..template.content <- gsub("{{doi}}",      doi,               private$..template.content, perl = TRUE)
            private$..template.content <- gsub("{{time}}",     private$..time,    private$..template.content, perl = TRUE)
            
            # Specialized updates.
            if(private$..type == "model") {
                # Multiple arguments can be added, followed by assertions and updates.

                # Assertions.
                assert(!is.null(model.type), "Argument `model.type` must be provided.")
            
                # Updates.
                private$..template.content <- gsub("{{parent.generator}}", ifelse(model.type == "cs", "CrossSectional", "TimeSeries"), private$..template.content, perl = TRUE)
            }
        },


        ..write.file = function(open = TRUE) {
            # Write the file.
            writeLines(private$..template.content, private$..output.path)

            # Show some user feedback.
            cat(
                "File ", shQuote(crayon::green$bold(private$..output.path)), " was ", crayon::green("successfully"), " added to the package.",
                "\n\n",

                crayon::black$bgGreen$bold("File details:"), 
                "\n",
                
                "  - type: ", crayon::yellow(private$..type), 
                "\n",
                
                "  - template: ", crayon::yellow(private$..template.path), 
                "\n\n",

                "Have fun implementing it!", 
                "\n",

                sep = ""
            )

            # Open the file in RStudio.
            if(open) file.edit(private$..output.path)
        }
    ),


    public = list(
        # Constructor.
        initialize = function(type, name, alias, author, doi, ...) {
            # Mark the date.
            private$..time <- Sys.time()
            
            # Set type.
            private$..type = type
            
            # Load the appropriate template given the requested type.
            private$..load.template()

            # Determine the output path.
            private$..determine.output.path(name)

            # Update the template content.
            private$..update.template(name, alias, author, doi, ...)

            # Write file.
            private$..write.file()
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exported wrapper for adding files ---------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
make <- function(...) {
    # Make the requested file.
    return(Scaffolder$new(...))
}



# End of file.
