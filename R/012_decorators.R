# This files contains function decorators, mainly intended for console output, in order to keep functions clean.

# Defining the decorator operator.
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