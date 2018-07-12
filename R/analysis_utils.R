# This file contains a list of functions used for data analysis only. 
# These functions are not relevant for the simulation design.
# They are useful for moving result files around and parsing things.



#' @title .
#' @export
identify_result_files <- function(paths, pattern) {
    files = sapply(paths, FUN = function(path) {
        names = list.files(path, pattern)
        sapply(names, function(name) {
            paste0(path, '/', name)
        }, USE.NAMES = F)

    }, USE.NAMES = F)

    files = unlist(files)

    return(files)
}



#' @title .
#' @export
group_result_files <- function(files, location) {
    # Creating the grouping directory.
    dir.create(location)

    # Copying every file and storing the status.
    status = sapply(files, function(file) {
        file.copy(file, paste0(location, '/', strsplit(file, '/data/')[[1]][3]))
    })

    # Return the names of the files not copied, if any.
    if (all(status))
        return(list.files(location))
    else
        return(FALSE)
}



#' @title .
#' @export
cell_error_parser <- function(error_string, position_in_design = NULL) {
    config = as.numeric(strsplit(sub('(.*)config:(\\s)', '', sub('(\\.)$', '', error_string)), ' ')[[1]])
    names(config) <- c("participants", "nodes", "architecture", "connectedness", "model")
    
    # Append the position of the cell.
    config = c(config, position = position_in_design)
    
    return(
        list(
            config = config,
            results = NA
        )
    )
}



#' @title .
#' @export
extract_from_cell <- function(cell, position_in_design = NULL, correction = TRUE) {
    if(is.list(cell)) {
        # Prepare the config vector.
        config = c(cell$raw$config, position = position_in_design)
        
        # In case the simulation was ran before commit #9fb7c79, leave this correction be applied.
        if (correction) {
            warning('In `$config` vector the names are incorrectly set. $config[3] is `architecture` and  $config[4] is `connectedness`.')
            config_names = names(config)
            names(config)[3] <- config_names[4]
            names(config)[4] <- config_names[3]
        }
        
        cell_results = list(
            config = config,
            results = cell$computed
        )
    } else {
        cell_results = cell_error_parser(cell, position_in_design)
    }
    
    return(cell_results)
}


#' @title .
#' @export
extract_from_replication <- function(replication) {
    # Inform about the number of cells to spot possible missing cells. 
    cat(length(replication), 'cells.', '\n')
    
    # The essential results for all cells for an the entire replication.
    results = list()
    
    # Perform the extraction.
    for (cell in 1:length(replication)) {
        results[[cell]] = extract_from_cell(replication[[cell]], cell)
    }
    
    return(results)
}


#' @title .
#' @export
extract_from_set <- function(set) {
    cat(paste0('Number of replications in this set: ', length(set), '.'), '\n')

    # The essential results for all cells for an the entire replication set.
    results = list()
    
    for (replication in 1:length(set)) {
        cat('\t', '-> replication', replication, 'contains: ')

        results[[replication]] = extract_from_replication(set[[replication]])
    }
    
    return(results)
}



#' @title .
#' @export
combine_replication_sets_from_paths <- function(paths) {
    # The vector used to merge the sets.
    data = c()

    for (file in 1:length(paths)) {
        cat(paste0('Loading set ', file, '. '))
        set = extract_from_set(readRDS(paths[file]))
        data = c(data, set)
    }

    return(data)
}
