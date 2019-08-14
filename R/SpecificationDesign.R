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
#   - contains a class that defines the simulation specification          #
#                                                                         #
# To do:                                                                  #
#   - consider breaking Question into specific question types to          #
#     avoid passing arguments all over the place                          #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include Design.R Question.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# SpecificationDesign class -----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

SpecificationDesign <- R6::R6Class("SpecificationDesign",
    inherit = Design,


    private = list(
        # Set the design using questions.
        ..set.structure = function(multiple = TRUE, separator = " ", duplicates = FALSE) {
            # Capture the answer.
            models <- Question$new("select", "\nWhat model(s) do you want to simulate for?", names(Generator$..ALIASES..), multiple = multiple)$answer

            # For every model create an empty list where the options will be stored.
            for(model in models) {
                private$..structure[["model"]][[model]] <- c(alias = model, private$..gather.model.options(model = model, separator = separator, duplicates = duplicates))

                # If the models makes use of a graph for its architecture.
                if(private$..model.needs.graph(model)) {
                    # What graphs does the user want?
                    graphs <- Question$new("select", "\nWhat graph(s) do you want to use for generating true model parameters?", names(Graph$..ALIASES..), multiple = multiple)$answer

                    # For every graph, get it's options for all the steps involved in creating a graph (i.e., currently only generating graphs).
                    for(graph in graphs) {
                        private$..structure[["model"]][[model]][["graph"]][[graph]] <- c(alias = graph, private$..gather.graph.options(graph = graph, separator = separator, duplicates = duplicates))
                    }
                }
            }
        },


        # Set the design replications using a question.
        ..set.replications = function() {
            # Capture the answer.
            private$..replications <- Question$new("select", "\nHow many times should each cell be replicated?", c(100, 200, 300, 400, 500), multiple = FALSE)$answer
        },


        # Get all the options for all the major steps (i.e., Generator, Sampler and Estimator) for a model alias.
        ..gather.model.options = function(model, separator, duplicates = duplicates) {
            # Announce him or her about what is coming next.
            cat("\nVary simulation options for the", shQuote(crayon::yellow(model)), "model.\n")

            # Temporary storage for all the answers.
            answer <- list()
            
            # Query the options of the generator.
            answer$generator <- private$..query.blueprint.implementation(Generator, model, "..generator", separator = separator, duplicates = duplicates)

            # Query the options of the sampler.
            answer$sampler <- private$..query.blueprint.implementation(Sampler, model, "..sampler", separator = separator, duplicates = duplicates)

            # Query the options of the estimator. 
            # For my future self: I leave the challenge of handling bayesian estimation to you.
            answer$estimator <- private$..query.blueprint.implementation(Estimator, model, "..frequentist", separator = separator, duplicates = duplicates)

            return(answer)
        },


        # Get all the options for all the steps related to a graph (i.e., currently only the `Graph` with `..generator`).
        ..gather.graph.options = function(graph, separator, duplicates) {
            # Announce him or her about what is coming next.
            cat("\nVary simulation options for the", shQuote(crayon::yellow(graph)), "graph.\n")
            
            # Temporary storage for all the answers.
            answer <- list()

            # Query the options of the generator. We can also do `answer$generator`, but let's keep it simple until we add sampling and estimation for the graphs.
            answer <- private$..query.blueprint.implementation(Graph, graph, "..generator", separator = separator, duplicates = duplicates)

            return(answer)
        },


        # Ask questions about the parameters of the implementation of the main simulation blueprints.
        ..query.blueprint.implementation = function(blueprint, model, implementation, separator, duplicates) {
            # Make sure that the blueprint provided is part of what makes up a simulation.
            assert(class(blueprint) == "R6ClassGenerator" && blueprint$classname %in% c("Graph", "Generator", "Sampler", "Estimator"), ..ERRORS..$incorrect.object.type)

            # Announce user what kind of options are requested.
            cat("\nOptions related to the", private$..blueprint.name.to.pretty.text(blueprint$classname), "part:\n")

            # Prepare answer list.
            answer <- list()

            # Get the arguments for the user overwritten function (i.e., aka implementation).
            args <- formals(blueprint$..ALIASES..[[model]]$class$private_methods[[implementation]])

            for (arg in names(args)) {
                # Prepare the question body.
                question <- paste0("What value(s) for '", crayon::yellow(arg) ,"' argument?")

                # Get the example argument.
                example.arg <- ifelse(is.symbol(args[[arg]]), blueprint$..ALIASES..[[model]]$example.args[[arg]], args[[arg]])

                # Prepare a reasonable example answer.
                example.answer <- paste("default is", example.arg)

                # Ask for the option in question and store it.
                question <- Question$new(type = "open", question = question, example = example.answer, separator = separator, duplicates = duplicates)

                # If the answer is empty, provide the example argument.
                if(length(question$answer) > 0) {
                    answer[[arg]] <- question$answer
                } else {
                    answer[[arg]] <- example.arg
                }
            }

            return(answer)
        },


        # Print instructions.
        ..instructions = function(...) {
            # Nice header.
            cat("\n",
                "-  -  -  -  -  -  -  -  -  -  -  -  -  -", "\n",
                "-   Simulation Design Specification    -", "\n",
                "-  -  -  -  -  -  -  -  -  -  -  -  -  -", "\n\n",

                "You are going to answer a series of questions which will be used as input to create the simulation design.\n",
                "The answers mainly involve typing one or more numbers.\n",
                "You can type, for instance, '", crayon::yellow("1 2 3 4 5"), "'.\n",
                "You can also type, '", crayon::yellow("from 0 to 1 by .2"), "' which will evaluate to '0 .2 .4 .6 .8 1'.\n",
                "For 'TRUE' or 'FALSE' values you can type either '", crayon::yellow("TRUE true T t FALSE false F f"), "'.\n",
                "There is no need to use quotation marks. You may press `ESC` to quit at anytime.\n\n",
             sep = "")

            # Assume the user doesn't understand the instructions.
            consent = "no"

            # Make sure the user understands the instructions.
            while(consent != "yes") {
                # Stop the script if the user wants to.
                if(consent == "") { stop("The script stopped at your heart's desire.") }

                # Ask for consent.
                consent = Question$new("select", "Do you understand the instruction above and agree to proceed?", c("yes", "no"), multiple = FALSE)$answer
            }
        },


        # Check if a model needs a graph based on the function definition.
        ..model.needs.graph = function(model) {
            return("..graph" %in% names(Generator$..ALIASES..[[model]]$class$get_inherit()$private_fields))
        },


        # Get a pretty name for a blueprint.
        ..blueprint.name.to.pretty.text = function(blueprint.name) {
            # Decide on the pretty text.
            pretty.text <- switch(blueprint.name,
                "Graph" = "graph generation",
                "Generator" = "true model generation",
                "Sampler" = "data generation",
                "Estimator" = "model estimation",
                NULL
            )

            return(pretty.text)
        }
    ),


    public = list(
        initialize = function(...) {
            # Print the instructions.
            private$..instructions()

            # Call the parent constructor.
            super$initialize(...)
        }
    )
)



# End of file.
