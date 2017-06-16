#' Takes a matrix of data for a species, checks if its numeric, then puts the table into a long-format dataframe
#'
#' @param x a matrix of data, generally species in the columns and sites in the row
#' @parm row.metadata metadata for the sites; in long format, it will be stored in each row with with the site pertaining to the data
#' @param col.metadata metadata for the species; will be stored in every 'n'th row, where 'n' is the number of rows in the original table
#' @parma total.metadata metadata for table; will include publishing information
#' @importFrom reshape2 melt
#' @return data set in long format, with all metadata included
.matrix.melt <- function(x, year=NA, row.metadata, col.metadata, total.metadata){
    if(!is.numeric(x))
        stop("Error: x is not numeric")
    if(!is.matrix(x))
        stop("Error: x is not a matrix")
    x[is.na(x)] <- 0
  
    # Check if presence/absense matrix by first checking if negatives exist in matrix
    if(any(x < 0))
        if(!all(x < 0))
            stop("'x' contains both presence/absence and abundance data")

    # Reformat data
    output <- melt(x)
    names(output) <- c("species", "sites", "value")
    output$year <- rep(year, ncol(x))
    #... do some more sophisticated meta-data stuff soon...
    return(output)
}

# Takes a data already in long format that will be converted to a string of metadata. Each row will be a single string, and the
# function will return the list of these strings
#
# @param data a dataframe exclusively containing the columns of metadata
# @return a list of metadata strings
.make.metadata <- function(data){
  sapply(1:nrow(data), function(y) {
    char.list <- c(rbind(colnames(data), "=", as.character(data[y,]), ", "))
    char.list <- head(char.list, -1)
    metadata <- paste(char.list, collapse="")
    return(metadata)
  })
}

# Unzips a file from a downloaded zip file
# param file name of file to be extracted from zip
# param zip location and name of zip file (e.g.,
#     ~/Downlaods/a_file.zip)
# param to.save.dir directory to save resulting file (DEFAULT: a new
#     temporary directory will be used)
# param to.save.name name to save the file as (DEFAULT: it will be
#     named paste(zip,file, sep='_'))
# return Complete path to unzipped file
#' @importFrom utils unzip download.file
#' @importFrom reshape2 melt
#' @importFrom httr GET
#' @importFrom stats setNames
.unzip <- function(file, zip, to.save.dir, to.save.name){
    if(missing(to.save.dir))
        to.save.dir <- tempdir()
    if(missing(to.save.name))
        to.save.name <- file
    
    files <- unzip(zip, list=TRUE)
    if(!file %in% files$Name)
        stop("Required file not in zipfile ", zip)

    file <- unzip(zip, file)
    file.rename(file, file.path(to.save.dir, to.save.name))
    return(file.path(to.save.dir, to.save.name))
}

.fac.sim <- function(x){
    x <- Filter(Negate(is.na), x)
    x <- x[x != "" & x != " "]
    x <- unique(x)
    return(paste(x,collapse="_"))
}

#' @importFrom stats model.matrix
.expand.factor <- function(factor_to_expand, name){
    names <- rep(name, length(unique(factor_to_expand)))
    output <- model.matrix(~factor_to_expand-1)
    colnames(output) <- paste(names, gsub("factor_to_expand", "", colnames(output)), sep="_")
    return(as.data.frame(output))
}

.download <- function(url, dir, save.name, cache=TRUE){
    destination <- file.path(dir, save.name)
    suffix <- .file.suffix(url, 4)

    if(cache==TRUE & file.exists(destination)){
        if(!is.na(suffix))
            attr(destination, "suffix") <- suffix
        return(destination)
    }

    result <- download.file(url, destination, quiet=TRUE)
    if(result != 0)
        stop("Error code", result, " downloading file; file may not exist")

    if(!is.na(suffix))
        attr(destination, "suffix") <- suffix
    return(destination)
}

.save.name <- function(doi, save.name, file){
    if(is.na(save.name)){
        save.name <- paste(doi,file, sep="_")
        save.name <- gsub(.Platform$file.sep, "_", save.name, fixed=TRUE)
    }
    return(save.name)
}

.grep.url <- function(url, regexp, which=1){
    html <- as.character(GET(url))
    return(.grep.text(html, regexp, which))
}

.grep.text <- function(text, regexp, which=1){
    links <- gregexpr(regexp, text)
    if(which > length(links[[1]]))
        stop("SI number '", which, "' greater than number of detected SIs (", length(links[[1]]), ")")
    pos <- as.numeric(links[[1]][which])
    return(substr(text, pos, pos+attr(links[[1]], "match.length")[which]-1))
}

.file.suffix <- function(text, max.length=4){
    suffix <- .grep.text(text, "[a-zA-Z]+$")
    if(nchar(suffix) <= max.length & nchar(suffix) > 0)
        return(suffix)
    return(NA)
}

prog.bar <- function(x, y){
    if(y < 100){
        cat(".")} else {
            z <- Filter(function(z) z>=0, seq(1,y,length.out=100)-x)
            if(length(z) > 0)
                tryCatch(if(z[1] < 1) if((length(z) %% 10)==0) cat("|") else cat("."), error=function(z) cat("."))
        }
}    
