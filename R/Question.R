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
#   - contains a class that defines a question                            #
#   - makes use of utility functions                                      #
#   - internal use only                                                   #
#                                                                         #
# To do:                                                                  #
#   - break into child classes: open-ended and selection questions        #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Question class ----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Question <- R6::R6Class("Question",

    private = list(
        ..answer = NULL,
        ..type = NULL,
        ..question = NULL,
        ..confirmation = NULL,


        # Ask user for confirmation on his or her input or selection.
        ..ask.for.confirmation = function(answer) {
            # Format the answer for display.
            pretty.answer <- ifelse(length(answer) == 0, "none", paste0(crayon::silver(" | "), paste(crayon::yellow(answer), collapse = crayon::silver(" | ")), crayon::silver(" | ")))

            # Prepare the confirmation text.
            title <- paste0("Inferred values:", pretty.answer, "\n\n", "Do you accept these values?") 

            # Set the answer options.
            options <- c("Yes", "No", "Exit")

            # Ask for confirmation and store the answer.
            answer <- menu(options, title = title)

            # Parse the answer.
            answer <- ifelse(answer == 1, TRUE, ifelse(answer == 2, FALSE, NA))

            return(answer)
        },


        # Ask for input in an open ended fashion.
        ..ask.for.input = function(example, separator = " ", duplicates = FALSE) {
            # Assume incorrect answer.
            correct <- FALSE

            # Ask user to specify an answer until he or she confirms that it is a valid specification.
            while(!correct || is.na(correct)) {
                # Break if it is NA.
                if(is.na(correct)) { break }
                
                # Capture the answer.
                answer <- readline(paste(private$..question, " ", "| Answer (e.g., ", example, "):", " ", sep = ""))

                # Decide how to handle the answer.
                if(matches.sequence.format(answer)) {
                    answer <- string.to.sequence(answer)
                } else {
                    answer <- string.to.vector(answer, separator = separator)
                }

                # Should only unique values be kept?
                if(!duplicates) {
                    answer <- unique(answer)
                }

                # Determine if confirmation is needed to proceed.
                if(private$..confirmation) {
                    # The user can now confirm.
                    correct = private$..ask.for.confirmation(answer)

                    # Break the prompt if the user wishes so.
                    if(is.na(correct)) {
                        cat("You declined to answer. Using last known answer: ", shQuote(crayon::yellow(paste(answer, collapse = " "))), ".\n\n", sep = "")
                        break
                    }
                } else {
                    # Enforce positive confirmation since confirmation was not requested.
                    correct <- TRUE
                }
            }

            return(answer)
        },


        # Ask for input using pre-defined options.
        ..ask.for.selection = function(choices, multiple = TRUE) {
            # Assume incorrect answer.
            correct <- FALSE

            # Ask user to select an answer until he or she confirms that it is a valid selection.
            while(!correct || is.na(correct)) {
                # Break if it is NA.
                if(is.na(correct)) { break }

                # Capture the answer.
                answer <- select.list(choices = choices, title = private$..question, multiple = multiple)

                # Determine if confirmation is needed to proceed.
                if(private$..confirmation) {
                    # The user can now confirm.
                    correct = private$..ask.for.confirmation(answer)

                    # Break the prompt if the user wishes so.
                    if(is.na(correct)) {
                        cat("You declined to answer. Using last known answer: ", shQuote(crayon::yellow(paste(answer, collapse = " "))), ".\n\n", sep = "")
                        break
                    }
                } else {
                    # Enforce positive confirmation since confirmation was not requested.
                    correct <- TRUE
                }
            }

            return(answer)
        },


        # Decide whether a selection or input question must be selected.
        ..ask.the.right.question.type = function(...) {
            # Ensure the correct type is provided.
            assert(private$..type %in% c("open", "select"), ..ERRORS..$unsupported.type)

            # Ask the right type of question and store the answer.
            if(private$..type == "open") {
                private$..answer <- private$..ask.for.input(...)
            } else {
                private$..answer <- private$..ask.for.selection(...)
            }
        }
    ),


    public = list(
        # Constructor.
        initialize = function(type, question, ..., confirmation = FALSE) {
            # Set fields.
            private$..type = type
            private$..question = question
            private$..confirmation = confirmation

            # Ask the question.
            private$..ask.the.right.question.type(...)
        }
    ),


    active = list(
        answer = function() {
            return(private$..answer)
        }
    )
)



# End of file.
