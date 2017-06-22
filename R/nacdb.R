#' Builds a community database
#'
#' The key function of the nacdb package. When run with defaults, it
#' will download and build a database of species' traits from all the
#' manuscript sources in the package. This totals XXX
#' manuscripts/databases, XXX species, and XXX traits. Please note
#' that all parameters are interactive; thus specifying \code{species}
#' and \code{traits} constraints will constraint according to both,
#' for example. Please also note that specifying any kind of
#' constraints makes use of the package's built-in cache of what
#' species and traits information are available in each database;
#' making use of this on the GitHub (developer) build of this package
#' is not advisable, and (further) it is impossible for us to verify
#' whether the datasets NATDB searches have been updated since the
#' package was last built.
#' 
#' @param datasets Character vector of datasets to be searched for
#'     trait data. If not specified (the default) all trait datasets
#'     will be downloaded and returned.
#' @param species Character vector of species to be searched for trait
#'     data. If not specified (the default) data for all species will
#'     be downloaded and returned.
#' @param traits Character vector of traits to be searched for
#'     data. If not specified (the default) data for all traits will
#'     be downloaded and returned.
#' @return nacdb.data object. XXX
#' @author Will Pearse; Clint; Raimi; Ethan; etc.
#' #@examples
#' # Limit the scope of these as they have to work online on servers!...
#' #@seealso 
#' @export

nacdb <- function(cache, datasets, delay=5){
    #Check datasets
    if(missing(datasets)){
        datasets <- Filter(Negate(is.function), ls(pattern="^\\.[a-z]*\\.[0-9]+", name="package:nacdb", all.names=TRUE))
    } else {
        datasets <- paste0(".", tolower(datasets))
        datasets <- gsub("..", ".", datasets, fixed=TRUE)
    }
    if(!all(datasets %in% datasets)){
        missing <- setdiff(datasets, ls.funs())
        stop("Error: ", paste(missing, collapse=", "), "not in nacdb")
    }
    
    #Do work and return
    output <- vector("list", length(datasets))
    for(i in seq_along(datasets)){
        prog.bar(i, length(datasets))
        if(!missing(cache)){
            path <- file.path(cache,paste0(datasets[i], ".RDS"))
            if(file.exists(path)){
                output[[i]] <- readRDS(path)
            } else {
                output[[i]] <- eval(as.name(datasets[i]))()
                saveRDS(output[[i]], path)
                Sys.sleep(delay)
            }
        } else {
            output[[i]] <- eval(as.name(datasets[i]))()
            Sys.sleep(delay)
        }
    }

    # Merge the community matrices that have been loaded
    # - probably using something like do.call(rbind, output)

    # Likely alter the output in some way to make it more useful
    
    class(output) <- "nacdb"
    return(output)
}

print.nacdb <- function(x, ...){
    # Argument handling
    if(!inherits(x, "nacdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'nacdb'")
    
    # Create a simple summary matrix of species and sites in x
    
    # Print it to screen
    print(x)
    # Return (invisibly) the output
    #invisible(output)
}

summary.nacdb <- function(x, ...){
    print.nacdb(x, ...)
}

"[.nacdb" <- function(x, spp, traits){
    # Argument handling
    if(!inherits(x, "nacdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'nacdb'")

    # Species
    if(!missing(spp)){
        if(any(x$numeric$species %in% spp))
            x$numeric <- x$numeric[x$numeric$species %in% spp,] else
                                                                    x$numeric <- NULL
        if(any(x$categorical$species %in% spp))
            x$categorical <- x$categorical[x$categorical$species %in% spp,] else
                                                                          x$categorical <- NULL
    }
    
    # Traits
    if(!missing(traits)){
        if(any(x$numeric$variable %in% traits))
            x$numeric <- x$numeric[x$numeric$variable %in% traits,] else
                                                                        x$categorical <- NULL
        if(any(x$categorical$variable %in% traits))
            x$categorical <- x$categorical[x$categorical$variable %in% traits,] else
                                                                              x$categorical <- NULL
    }

    output <- list(categorical=x$categorical, numeric=x$numeric)
    class(output) <- "nacdb"
    return(output)
}

species <- function(x, ...){
    if(!inherits(x, "nacdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'nacdb'")
    # Return a vector of the sites in nacdb (?)
}

sites <- function(x, ...){
    if(!inherits(x, "nacdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'nacdb'")
    # Return a vector of the sites in nacdb (?)
}

citations <- function(x){
    if(!inherits(x, "nacdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'nacdb'")
    
    data(nacdb_citations)
    datasets <- Filter(Negate(is.function), ls(pattern="^\\.[a-z]*\\.[0-9]+[a-d]?", name="package:nacdb", all.names=TRUE))
    nacdb.citations$Name <- with(nacdb.citations, paste0(".", tolower(Author), ".", Year))

    return(as.character(nacdb.citations$BibTeX.citation[match(datasets, nacdb.citations$Name)]))
}
