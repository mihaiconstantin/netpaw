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
#   - contains utilities used throughout the package                      #
#   - it's a pity R6 doesn't support static class methods                 #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Package specific functions ----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Validate a list of arguments associated with a function against the `...` object.
validate.arguments <- function(args, fun, target.list) {
    # The arguments as specified in the function definition.
    definition.args <- as.list(args(fun))

    result <- sapply(args, function(arg) {
        # Does the argument have a default value?
        has.default <- !is.symbol(definition.args[[arg$name]])

        # Is the argument present in the target list?
        is.missing <- is.null(target.list[[arg$name]])

        # The argument has a default value, but that value is being overwritten by the user.
        # The argument doesn't have a default value and it not missing.
        if((has.default && !is.missing) || (!has.default && !is.missing)) {
            is.conformable <- conformity.check(target.list[[arg$name]], arg)

        # The argument doesn't have an default value and it's also missing.
        } else if(!has.default && is.missing) {
            # Since the argument is missing and no default exists, it is by default non-conformable.
            is.conformable = FALSE

            # Some user feedback.
            message(paste("Missing expected argument '", arg$name ,"'. Code: M.GR#001.", sep = ""))

        } else {
            # If argument has a default value that is not overwritten by the user we are good.
            is.conformable = TRUE    
        }

        return(is.conformable)
    })

    return(all(result))
}



# Argument conformity check.
conformity.check <- function(value, arg) {
    if(arg$type == "int" || arg$type == "double") {
        # If the argument value is integer or double check the bounds.
        is.conformable <- ((value >= min(arg$range)) && (value <= max(arg$range)))

    } else if(arg$type == "bool") {
        # If the argument value is logical check that only logical values are passed.
        is.conformable <- is.logical(value) && !is.na(value)

    } else {
        # Safety net in case I add new argument types and I forget to add conformity checks.
        stop("Unrecognized argument type; currently supported: 'int', 'double', and 'bool'. CODE: E.UT#001")
    }

    # Some user feedback.
    if(!is.conformable) {
        message(paste("Argument '", arg$name, " = ", value ,"' is non-conformable. Code: M.GR#002.", sep = ""))
    }

    return(is.conformable)
}



# Combine arguments from a function definition and a target list (e.g., '...' object).
combine.arguments <- function(fun, target.list, implementation = FALSE) {
    # Get the arguments the definition arguments as a list.
    definition.args <- as.list(args(fun))

    # Remove the last redundant element.
    definition.args <- definition.args[-length(definition.args)]

    overwritten.args <- sapply(names(definition.args), function(arg.name) {
        if(!is.symbol(definition.args[[arg.name]]) && is.null(target.list[[arg.name]])) {
            return(definition.args[[arg.name]])

        } else {
            return(target.list[[arg.name]])
        }
    }, simplify = FALSE)

    # Should we store the implementation (i.e., aka the name of the function that parsed)?
    if(implementation) {
        overwritten.args[["implementation"]] <- sub(".*\\.\\.", "", toString(substitute(fun)))
    }

    return(overwritten.args)
}



# Give a list of arguments (i.e., as defined in the context of `netpaw`) generate numeric values.
generate.arguments <- function(args.list, ...) {
    # Capture the `...` overwrites as a list.
    . <- list(...)

    # Generate or overwrite arguments accordingly.
    parameters <- lapply(args.list, function(arg) {
        # First check to see if we desire to overwrite the generation with specific values.
        if(!is.null(.[[arg$name]])) {
            value <- .[[arg$name]]

        } else {
            # Choose the correct type.
            if(arg$type == "int") {
                value <- floor(runif(1, min(arg$range), max(arg$range) + 1))

            } else if(arg$type == "double") {
                value <- runif(1, min(arg$range), max(arg$range))

            } else if(arg$type == "bool") {
                value <- sample(0:1, 1, TRUE, c(.5, .5)) == TRUE
            }
        }

        return(value)
    })

    return(parameters)
}



# Sample a vector of positive and negative integers according to a ratio. 
sample.positive.parameter.ratio <- function(number.parameters, ratio) {
    positive.ratio <- sample(c(-1, 1), number.parameters, TRUE, prob = c(1 - ratio, ratio))

    return(positive.ratio)
}



# Determine if a column is invariant.
is.invariant <- function(column, tolerance = 1) {
    column <- as.factor(column)
    frequencies <- table(column)
    categories <- length(frequencies)

    if(categories <= 2)
    {
        nobs <- length(column)
        min.frequency <- min(frequencies)
        max.frequency <- max(frequencies)

        if(min.frequency <= tolerance || max.frequency >= nobs - tolerance) {
            return(TRUE) 
        }
        else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}



# Find the oldest ancestor of an `R6ClassGenerator`.
find.ancestor <- function(type) {
    # Evaluate to type.
    blueprint <- eval(as.symbol(type))

    # Type check.
    assert(class(blueprint) == "R6ClassGenerator", ..ERRORS..$incorrect.object.type)

    # Get the parent class.
    inherit <- as.character(blueprint$inherit)

    # If more parents exist, recall, else return type.
    if(length(inherit) > 0) { Recall(inherit) } else { return(type) }
}



# Find the ancestry line of an `R6ClassGenerator`.
find.ancestry <- function(type) {
    # Construct the family.
    ancestry <- c()

    # Find ancestors.
    (function(type) {
        # Evaluate to type.
        blueprint <- eval(as.symbol(type))

        # Type check.
        assert(class(blueprint) == "R6ClassGenerator", ..ERRORS..$incorrect.object.type)

        # Get the parent class.
        inherit <- as.character(blueprint$inherit)

        ancestry <<- c(ancestry, type)

        # If more parents exist, recall, else return type.
        if(length(inherit) > 0) { Recall(inherit) } else { return(type) }
    })(type)

    return(ancestry)
}



# Print the API of an R6 class.
print.class.api <- function(blueprint, excluded.fields = c(), excluded.methods = c("initialize", "clone", "print", "plot"), parent = FALSE) {
    # Determine the excluded fields.
    if(length(excluded.fields)) { excluded.fields <- paste0(excluded.fields, collapse = "|") } else { excluded.fields <- "!" }

    # Determine the excluded methods.
    if(length(excluded.methods)) { excluded.methods <- paste0(excluded.methods, collapse = "|") } else { excluded.methods <- "!" }

    # What fields to print?
    fields <- names(c(blueprint$active, blueprint$public_fields))[!grepl(excluded.fields, names(c(blueprint$active, blueprint$public_fields)))]

    # What methods to print?
    methods <- names(blueprint$public_methods)[!grepl(excluded.methods, names(blueprint$public_methods))]

    # Start printing.
    cat(crayon::bold("API:"))
    cat("\n")

    # Fields.
    if(length(fields)) { cat("  - fields:", paste(fields, collapse = " | ")) } else { cat("  - fields:", crayon::silver("n.a.")) }
    cat("\n")

    # Methods.
    if(length(methods)) {
        cat("  - methods:")
        cat("\n")

        # Print methods.
        for (name in methods) {
            cat("    -", paste(name ,"(", paste(formalArgs(blueprint$public_methods[[name]]), collapse = ", "), ")", sep = ""))
            cat("\n")
        }
    } else {
        cat("  - methods:", crayon::silver("n.a."))
        cat("\n")
    }

    # Parent fields and methods.
    if(parent) {
        # Get all parents.
        parents <- find.ancestry(blueprint$classname)[-1]

        # Storage for the parent fields.
        parent.fields <- c()

        # Storage for the parent methods.
        parent.methods <- list()

        # Get fields and methods for all parents.
        for(parent in parents) {
            # Create the blueprint.
            parent.blueprint <- eval(as.symbol(parent))

            # Append the parent fields.
            parent.fields <- c(parent.fields, names(c(parent.blueprint$active, parent.blueprint$public_fields))[!grepl(excluded.fields, names(c(parent.blueprint$active, parent.blueprint$public_fields)))])

            # Determine the methods.
            methods <- names(parent.blueprint$public_methods)[!grepl(excluded.methods, names(parent.blueprint$public_methods))]

            # Append the private methods.
            for(method in methods) {
                parent.methods[[method]] <- formalArgs(parent.blueprint$public_methods[[method]])
            }
        }

        # Print parent fields.
        if(length(parent.fields)) { cat("  - inherited fields:", paste(parent.fields, collapse = " | ")) } else { cat("  - inherited fields:", crayon::silver("n.a.")) }
        cat("\n")

        # Print parent methods.
        if(length(parent.methods)) {
            cat("  - inherited methods:")
            cat("\n")

            # Print methods.
            for (name in names(parent.methods)) {
                cat("    -", paste(name ,"(", paste(parent.methods[[name]], collapse = ", "), ")", sep = ""))
                cat("\n")
            }
        } else {
            cat("  - inherited methods:", crayon::silver("n.a."))
            cat("\n")
        }
    }
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Generic functions (i.e., may be exported) -------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Conveniently patch function bodies.
patch.function <- function(fun, patch, position = 1) {
    # Deparse the body.
    fun.body <- deparse(body(fun))

    # Append the patch to function body where intended.
    patched.fun.body <- paste0(
        c(fun.body[1:position], patch, fun.body[(position + 1):length(fun.body)]),
        collapse = "\n"
    )

    # Parse and treat as an expression.
    expr <- as.expression(parse(text = patched.fun.body))

    return(expr)
}



# Patch a function binding within an environment (i.e., by reference).
patch.function.within.environment <- function(binding, environment, patch) {
    # Unlock the binding in the environment.
    unlockBinding(binding, environment)

    # Alter the function.
    body(environment[[binding]]) <- patch.function(environment[[binding]], patch)

    # Lock the binding in the `self` environment.
    lockBinding(binding, environment)

    # Prevent return printing NULL.
    invisible() 
}



# Bypass the lock and set a locked binding (i.e., by reference).
set.locked.binding <- function(binding, environment, value) {
    # Unlock the binding in the environment.
    unlockBinding(binding, environment)

    # Alter the function.
    environment[[binding]] <- value

    # Lock the binding in the `self` environment.
    lockBinding(binding, environment)

    # Prevent return printing NULL.
    invisible() 
}



# Assert something or fail with custom error message.
assert <- function(truth, error.message) {
    if(!truth) {
        stop(error.message)
    }
}



# Flatten weird nested lists.
# Copyright Michael (https://stackoverflow.com/a/41882883/5252007).
flatten.nested.list <- function(nested.list) {
    more.lists <- sapply(nested.list, function(x) is.list(x))

    output <- c(nested.list[!more.lists], unlist(nested.list[more.lists], recursive = FALSE))

    if(sum(more.lists)) {
        Recall(output)
    } else {
        return(output)
    }
}



# Flatten weird nested lists. Maintain order, but drop names.
# See https://stackoverflow.com/questions/57448008.
flatten.nested.list.order <- function(nested.list) {
    # Create storage for the flattened list.
    flattened = list()

    # Flatten the list.
    invisible(rapply(nested.list, function(x) {
        flattened <<- c(flattened, list(x))
    }))

    return(flattened)
}



# Create all possible combinations for a list and preserve the order.
list.combine <- function(input) {
    # Create list skeleton.
    skeleton <- rapply(input, head, n = 1, how = "list")

    # Create all possible combinations from list elements.
    combinations <- expand.grid(flatten.nested.list.order(input), stringsAsFactors = FALSE)

    # Create list for storing the output.
    output <- list()

    # Relist and preserve original data type.
    for (i in 1:nrow(combinations)) {
        output[[i]] <- retain.element.type(relist(flesh = combinations[i, ], skeleton = skeleton))
    }

    return(output)
}



# Specific to `list.combine()`.
retain.element.type <- function(input.list) {
    for (name in names(input.list)) {
        # If the element is a list, recall the function.
        if(inherits(input.list[[name]], "list")) {
            input.list[[name]] <- Recall(input.list[[name]])

        # Else, get the first element and preserve the type.
        } else {
            input.list[[name]] <- input.list[[name]][, 1]
        }
    }
    return(input.list)
}



# Get the number of nodes from a graph or weighted matrix.
get.number.nodes <- function(graph) {
    # Determine the dimensions.
    dimensions = dim(graph)

    # Check the dimensions.
    if(dimensions[1] != dimensions[2]) stop('Wrong type of input: the graph dimensions do not match.')

    # Return the number of nodes.
    return(dimensions[1])
}



# Compute the density (i.e., based on non-zero edges) for an unweighted graph.
get.graph.density <- function(graph) {
    # Number of nodes.
    nodes = get.number.nodes(graph)

    # Potential connections.
    potential = (nodes * (nodes - 1)) / 2

    # Actual connections.
    actual = sum(graph[upper.tri(graph)] != 0)

    # Density.
    graph.density = actual / potential

    return(graph.density)
}



# Generate a covariance matrix.
# For details on what the arguments mean check: https://stats.stackexchange.com/a/215647/116619
get.cov <- function(nvars, svec.min = 0, svec.max = 1, pmat = NA, svec = NA) {
    # Construct the `P` matrix.
    if(is.na(pmat)) {
        # Generate the `P` orthogonal matrix.
        pmat <- qr.Q(qr(matrix(rnorm(nvars ^ 2), nvars)))
    }

    # Set the values of the `s` vector.
    if(length(svec) == 1 && is.na(svec)) {
        svec <- runif(nvars, svec.min, svec.max)
        svec <- svec[order(svec, decreasing = TRUE)]

    } else {
        # If user provided `s` vector then check that it is non-negative and in decreasing order.
        check.order = all(svec == svec[order(svec, decreasing = TRUE)])
        check.sign = any(svec < 0) 

        if(!check.order || check.sign) stop('Vector `svec` must contain positive elements in decreasing order.')
    }

    # Check that the dimensions of `P` and `S` match.
    if(ncol(pmat) != length(svec)) stop('Dimensions `svec` length must be equal to the dimensions of `pmat`.')

    # Generate the covariance matrix.
    covmat <- crossprod(pmat, pmat * svec)

    return(covmat)
}



# Generate partial correlation matrices.
get.pcor <- function(nvars) {
    # Generate the covariance matrix.
    covmat <- get.cov(nvars)

    # Compute the precision matrix.
    precmat <- solve(covmat)

    # Compute the partial correlations.
    pcormat <- -cov2cor(precmat)

    # Set the diagonal to 0.
    diag(pcormat) <- 0

    return(pcor.mat)
}



# Dump some contents to a file.
dump.contents <- function(file, contents, sep = "\r\n", open = "wb", verbose = TRUE) {
    # Open connection.
    connection <- file(file, open)

    # Close the connection on exit.
    on.exit(close(connection))

    # Write contents.
    writeLines(contents, connection, sep = sep)

    # Console feedback.
    if(verbose) cat("Successfully created file `", crayon::yellow(file) ,"`.", "\n", sep = "")
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# String manipulations ----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Convert a string input to a vector of values, breaking by separator.
string.to.vector = function(string, separator = " ") {
    # Store the input.
    input <- string

    # Split the string by space into numbers.
    values <- unlist(strsplit(input, separator))

    # Remove empty strings due to multiple spacing.
    values <- values[values != ""] 

    # If there is at least one element provided. Decide based on first element on what to convert to.
    if(length(values) > 0) {
        values <- sapply(values, function(value) {
            # Consider applying `type.convert()` on each element instead of my implementation.
            return(ifelse(character.is.numeric(value), character.to.numeric(value), ifelse(character.is.logic(value), character.to.logic(value), value)))
        })
    }

    # Remove the names.
    names(values) <- NULL

    return(values)
}


# Detect if character or group of characters can be expressed as a numerical value.
character.is.numeric <- function(string) {
    return(grepl("^(\\d+)?(\\.?)(\\d+)?$", string))
}



# Detect if character or group of characters can be expressed as a logical value.
character.is.logic <- function(string) {
    return(grepl("^([Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee]|[Tt]|[Ff])$", string))
}



# Character vector to numerical vector.
character.to.numeric <- function(vector) {
    # Convert elements to numeric and ignore warnings about NAs.
    values <- suppressWarnings(as.numeric(vector))

    # Remove the NAs.
    values <- values[!is.na(values)]

    return(values)
}



# Character vector to logical vector.
character.to.logic <- function(vector) {
    # Map the vector elements.
    values <- sapply(tolower(vector), function(element) {
        result <- switch(element, 
            "t" = TRUE, 
            "f" = FALSE, 
            "true" = TRUE,
            "false" = FALSE,
            # Default value if nothing matches.
            NA 
        )

        return(result)
    })

    # Remove missing values.
    values <- values[!is.na(values)]

    # Remove the names.
    names(values) <- NULL

    return(values)
}



# Convert a string input to a sequence of numbers.
string.to.sequence = function(string) {
    # Break the string by the standard keywords.
    values <- unlist(strsplit(tolower(gsub(" ", "", string)), "from|to|by"))

    # Keep only relevant values and assume everything else is noise.
    values <- as.numeric(values[2:4])

    # Create the sequence.
    values <- seq(from = values[1], to = values[2], by = values[3])

    return(values)
}



# Detect if a string input is suitable for conversion to sequence.
matches.sequence.format = function(string) {
    # Define the expected pattern.
    expected.pattern = "from([0-9]+)?(\\.)?([0-9]+)to([0-9]+)?(\\.)?([0-9]+)by([0-9]+)?(\\.)?([0-9]+)"

    # Check if the pattern exists in the string.
    return(ifelse(grepl(expected.pattern, tolower(gsub(" ", "", string))), TRUE, FALSE))
}



# Capitalize a word.
capitalize <- function(word) {
    capitalized.word <- paste(toupper(substring(word, 1, 1)), substring(word, 2), sep = "")

    return(capitalized.word)
}



# Combine multiple lines.
vector.lines <- function(...) {
    paste(crayon::chr(...), collapse = "\n")
}



# How many lines of R code are in a directory?
how.many.lines <- function(path = "./R") {
    files <- list.files(path = path, recursive = TRUE, full.names = TRUE)

    lines <- sapply(files, function(file) {
        file.lines <- length(readLines(file))
    })

    return(sum(lines))
}



# Parse errors and warnings for logging.
format.exceptions <- function(e) {
    return(paste(as.character(Sys.time()), "|", "call:", paste0(deparse(e$call), sep = "", collapse = "#"), "|", "message:", e$message))
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Decorators --------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Definition of the decorator operator.
`%decorate%` = function(decorator, func) {
    return(decorator(func))
}



# Timer decorator.
timer.decorator <- function(func) {
    wrapper <- function(...) {
        start = proc.time()
        result <- func(...)
        duration <- (proc.time() - start)[[3]]
        cat("Time: ", duration, "s.", sep = "")
        return(result)
    }
    return(wrapper)
}



# Decorated `run.cell` function.
logger.decorator <- function(func) {
    wrapper <- function(..., use.signature = TRUE, msg.before, msg.after) {
        
        # The message before the function is run.
        cat(msg.before)

        if(use.signature) {
            # Capture the `...` arguments.
            . <- list(...)

            # Print the `...` arguments.
            cat("- signature:", paste(., crayon::yellow(paste("(", gsub("\\.", " ", names(unlist(.))), ").", sep = "")), collapse = crayon::silver(" | ")))
        }

        # Run the function.
        result <- func(...)

        # The message after the function is run.
        cat(msg.after)

        return(result)
    }
}



# End of file.
