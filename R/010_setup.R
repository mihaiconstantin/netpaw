

#' @title Specify simulation design.
#' @export
build.design <- function(specification = NULL, ...) {
    # Check if R is running interactively and whether the user wants to specify the design interactively.
    if(interactive() && is.null(specification))
    {
        specification = determine.design.specification.from.questions(...)
    }
    
    # Build the design matrix from the specification.
    matrix <- build.design.matrix.from.specification(specification)

    # Result.
    result <- list(
        specification = specification,
        matrix = matrix
    )

    # Set the specification class for printing.    
    class(result) <- c("npdesign", "list")

    return(result)
}



determine.design.specification.from.questions <- function(...) {
    # Nice header for the user.
    cat(crayon::yellow(rep("# ", 10)), "\n")
    cat(crayon::black$bgYellow$bold("#                          #", "\n"))
    cat(crayon::black$bgYellow$bold("#     Simulation Design    #", "\n"))
    cat(crayon::black$bgYellow$bold("#                          #", "\n"))
    cat(crayon::yellow(rep("# ", 10)), "\n")

    # Short description about the purpose of this function.
    cat("\n")
    cat(crayon::italic("You are going to answer a series of questions which will be used as input to create the simulation conditions. The answers mainly involve typing in a sequence of numbers. After each number please add exactly one space (i.e., press the spacebar once)."))
    cat("\n\n")

    # Specify the design skeleton.
    specification <- list()

    # Sample size specification.
    specification$sample.size = prompt.design.factor.with.input("What sample sizes do you want to simulate for?", "50 100", ...)

    # Nodes specification.
    specification$node = prompt.design.factor.with.input("How many nodes you want in your network?", "10 15", ...)

    # Graph specification.
    specification$graph = prompt.design.factor.with.selection("What graphs do you want to simulate for?", SUPPORTED.GRAPHS, ...)

    # Graph options specification.
    for(graph in specification$graph) {
        cat("\nNow you will be asked to specify the generation options for the", shQuote(crayon::yellow(graph)), "graph.\n\n")
        
        if(graph == "random") {
            specification$graph.options$random$p = prompt.design.factor.with.input("What `edge probability` do you want to simulate for?", ".1, .3", ...)
        }

        if(graph == "smallworld") {
            specification$graph.options$smallworld$neighborhood = prompt.design.factor.with.input("What values for the `neighborhood` parameter do you want to simulate for?", "1, 2", ...)
            specification$graph.options$smallworld$p.rewire = prompt.design.factor.with.input("What values for the `rewiring probability` do you want to simulate for?", ".1, .2", ...)
        }

        if(graph == "scalefree") {
            specification$graph.options$scalefree$edge = prompt.design.factor.with.input("What values for the `edge` parameter do you want to simulate for?", "1, 2", ...)
            specification$graph.options$scalefree$attachment = prompt.design.factor.with.input("What values for the `attachment` parameter do you want to simulate for?", "1, 2", ...)
        }
    }

    # Model specification.
    specification$model = prompt.design.factor.with.selection("What models do you want to simulate for?", SUPPORTED.MODELS, ...)

    # Model options specification.
    for(model in specification$model) {
        cat("\nNow you will be asked to specify the generation options for the", shQuote(crayon::yellow(model)), "model.\n\n")

        if(model == "ggm") {
            specification$model.options$ggm$min = prompt.design.factor.with.input("What values for the `min` parameter do you want to simulate for? Default is .5.", ".5", ...)
            specification$model.options$ggm$max = prompt.design.factor.with.input("What values for the `max` parameter do you want to simulate for? Default is 1.", "1", ...)
            specification$model.options$ggm$constant = prompt.design.factor.with.input("What values for the `constant` parameter do you want to simulate for? Default is 1.5", "1.5", ...)
        }

        if(model == "ising") {
            specification$model.options$ising$mean = prompt.design.factor.with.input("What values for the `mean` parameter do you want to simulate for? Default is 0.", "0", ...)
            specification$model.options$ising$sd = prompt.design.factor.with.input("What values for the `sd` parameter do you want to simulate for? Default is 1.", "1", ...)
        }
    }

    # Positive edge ratio specification.
    specification$positive.edge.ratio = prompt.design.factor.with.input("What positive edge ratios do you want to simulate for?", ".5 .8", ...)

    # Replication count specification.
    specification$replication = prompt.design.factor.with.selection("How many times would you like to replicate each combination of design factors?", c(100, 500, 1000), multiple = FALSE, ...)

    return(specification)
}



build.design.matrix.from.specification <- function(specification) {
    # Flatten the specification nested list to first-order vectors.
    specification.flattened <- flatten.nested.list(specification) 

    # Create the design data frame based on factors and all levels.
    design <- expand.grid(specification.flattened)

    # Set every opposite level to NA (e.g., for `random` graph set all `graph.options` that belong to `smallworld` to NA).
    for (level in levels(design$graph)) {
        if(length(levels(design$model)) > 1) {
            design[design$graph == level, grep(paste0("graph.options.", levels(design$graph)[-which(levels(design$graph) == level)], collapse = "|"), names(design))] <- NA
        }
    }

    # Set every opposite level to NA (e.g., for `ggm` model set all `model.options` that belong to `ising` to NA).
    for (level in levels(design$model)) {
        design[design$model == level, grep(paste0("model.options.", levels(design$model)[-which(levels(design$model) == level)], collapse = "|"), names(design))] <- NA
    }

    # Remove redundant rows.
    design <- unique(design)

    # Reset the row names (i.e., indices).
    row.names(design) <- NULL

    # Reorder the columns such that the replication column is always the last one (i.e., because it never changes).
    design <- design[, c(names(design)[-which(names(design) == "replication")], "replication")]

    return(design)
}



prompt.design.factor.with.input <- function(question, answer.example, answer.separator = " ", parse.as.numeric = TRUE, with.confirmation = TRUE) {
    # Assume incorrect specification.
    is.correct <- 2

    # Ask user to specify an answer until he or she confirms that it is a valid specification.
    while(is.correct != 1) {
        # Capture the answer.
        answer <- readline(paste(question, " ", "Answer (e.g., ", answer.example, "):", " ", sep = ""))

        # Convert the string to a vector.
        answer <- unlist(strsplit(answer, answer.separator))

        # Remove empty strings due to multiple spacing.
        answer <- answer[answer != ""] 
        
        if(parse.as.numeric) {
            answer <- as.numeric(answer)
            answer <- answer[!is.na(answer)]
        }
        
        # Determine if confirmation is needed to proceed.
        if(with.confirmation) {
            # The user can now confirm.
            is.correct = menu(c("Yes", "No"), title = paste0("You entered: ", shQuote(crayon::yellow(paste(answer, collapse = " "))), ". Is that correct?"))
            
            # Break the prompt if the user wishes so.
            if(is.correct == 0) 
            {
                cat("You declined to answer. Using last known answer: ", shQuote(crayon::yellow(paste(answer, collapse = " "))), ".\n\n", sep = "")
                break
            }
        } else {
            # Enforce positive confirmation since we don't care about confirmation.
            is.correct <- 1
        }
    }

    return(answer)
}



prompt.design.factor.with.selection <- function(question, choices, multiple = TRUE, with.confirmation = TRUE) {
    # Assume incorrect specification.
    is.correct <- 2

    # Ask user to specify an answer until he or she confirms that it is a valid specification.
    while(is.correct != 1) {
        # Capture the answer.
        answer <- select.list(choices = choices, title = question, multiple = multiple)
        
        # Determine if confirmation is needed to proceed.
        if(with.confirmation) {
            # The user can now confirm.
            is.correct = menu(c("Yes", "No"), title = paste0("You selected: ", shQuote(crayon::yellow(paste(answer, collapse = " "))), ". Is that correct?"))

            # Break the prompt if the user wishes so.
            if(is.correct == 0) 
            {
                cat("You declined to answer. Using last known answer: ", shQuote(crayon::yellow(paste(answer, collapse = " "))), ".\n\n", sep = "")
                break
            }
        } else {
            # Enforce positive confirmation since we don't care about confirmation.
            is.correct <- 1
        }
    }

    return(answer)
}



print.npdesign <- function(object, ...) {
    # Details about the simulation design.
    cat("\n")
    cat(crayon::black$bgGreen$bold("Simulation design details:"))
    cat("\n")
    cat(crayon::silver("  - class(es):", paste(shQuote(class(object)), collapse = ", ")))
    cat("\n")
    cat("  - design matrix dimensions:", paste(nrow(object$matrix), "by", ncol(object$matrix)))
    cat("\n")
    cat("  - sample sizes:", paste(crayon::green(object$specification$sample.size), collapse = crayon::silver(" | ")))
    cat("\n")
    cat("  - nodes:", paste(crayon::green(object$specification$node), collapse = crayon::silver(" | ")))
    cat("\n")
    
    # Plotting the graphs and their respective options.
    for(graph in object$specification$graph) {
        cat("  - graph", shQuote(crayon::green(graph)))
        cat("\n")
        
        if(graph == "random") {
            cat("    - edge probabilities:", paste(crayon::yellow(object$specification$graph.options$random$p), collapse = crayon::silver(" | ")))
            cat("\n")
        }
        
        if(graph == "smallworld") {
            cat("    - neighborhood:", paste(crayon::yellow(object$specification$graph.options$smallworld$neighborhood), collapse = crayon::silver(" | "))) 
            cat("\n")
            cat("    - rewiring probabilities:", paste(crayon::yellow(object$specification$graph.options$smallworld$p.rewire), collapse = crayon::silver(" | ")))
            cat("\n")
        } 

        if(graph == "scalefree") {
            cat("    - edge:", paste(crayon::yellow(object$specification$graph.options$scalefree$edge), collapse = crayon::silver(" | "))) 
            cat("\n")
            cat("    - preferential attachment:", paste(crayon::yellow(object$specification$graph.options$scalefree$attachment), collapse = crayon::silver(" | ")))  
            cat("\n")
        }
    }

    # Plotting the models and their respective options.
    for(model in object$specification$model) {
        cat("  - model", shQuote(crayon::green(model)))
        cat("\n")
        
        if(model == "ggm") {
            cat("    - min:", paste(crayon::yellow(object$specification$model.options$ggm$min), collapse = crayon::silver(" | ")))
            cat("\n")
            cat("    - max:", paste(crayon::yellow(object$specification$model.options$ggm$max), collapse = crayon::silver(" | ")))
            cat("\n")
            cat("    - constant:", paste(crayon::yellow(object$specification$model.options$ggm$constant), collapse = crayon::silver(" | ")))
            cat("\n")
        }
        
        if(model == "ising") {
            cat("    - mean:", paste(crayon::yellow(object$specification$model.options$ising$mean), collapse = crayon::silver(" | ")))
            cat("\n")
            cat("    - sd:", paste(crayon::yellow(object$specification$model.options$ising$sd), collapse = crayon::silver(" | ")))
            cat("\n")
        }
    }

    cat("  - positive edge ratios:", paste(crayon::green(object$specification$positive.edge.ratio), collapse = crayon::silver(" | ")))
    cat("\n")
    cat("  - replications:", paste(crayon::green(object$specification$replication), collapse = crayon::silver(" | ")))
    cat("\n")
}
