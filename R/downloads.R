####################
# ADD DOIs ##########
#####################

#' @importFrom fulltext ft_get_si

.get.addresses <- function (...){
# Here is my first thought to conquer this. It saves a bunch of function calls, at the cost of individual customizability.
# Places all addresses in a single list (or should it be a vector?) Will be accessed and processed by another funciton
# Each quote could have corresponding data for alterations that would need to be made to the csv or table
    
    # If possible, I'd include a webpage search function here that just extracts all the links from the google doc,
    # - or server, if we want to invest in one. Might be too complicated, though. It would look something like this:
    
    links <- array(list(link = "http://esapubs.org/archive/ecol/E088/161/species_list.csv", format = "long", type = "count"))
    links <- cbind(names, list(link = "https://ndownloader.figshare.com/files/5619456", format = "long", type = "count"))
    # The list would go on, with the link first, the "format" is what form the table is currently in and the type is count
    # - vs presence/absense
    
    return(links)
}

# Option 2 is the manual version that looks very similar to downloads.R in natb, taking one publication at a time
# and creating a function call for it. I'll do a few here and for you to see if I'm doing it right.

# the location data is incredibly hard to follow. Couldn't find correlation to the species count. Just took species count.
.adler.2007 <- function(...){
    data <- read.csv("http://esapubs.org/archive/ecol/E088/161/species_list.csv")[c(1,3,2)]
    
    return(data)
}

.anderson.2011 <- function(...){
    data <- read.csv("https://ndownloader.figshare.com/files/5619462")
    data <- data[order(data$species), c(3, 1, 2, 4)
    return (data)
}

.chu.2013 <- function(...){
    #contains plant species and a count, but no location
    species.count <- read.csv("https://ndownloader.figshare.com/files/5626719")
    
    #contains location and cattle count but no corresponding species
    cattle.count <- read.csv("https://ndownloader.figshare.com/files/5626713")
}



.hnselmus.2013 <- function(...){
    library(pez) # This isn't how we declare packages in 'real'
                 # packages for the time being this is sufficient
    data(laja)
    return(.matrix.melt(invert.sites))
}

# Here is another example using the R package fulltext
# - *please* use this package where you can to download data
.thibault.2011 <- function(...){
    # Load data from Internet
    data <- read.csv(ft_get_si("E092-201", "MCDB_communities.csv", "esa-data-archives"))
    # ... transform data into the desired format ...
    return(transformed.data)    
}
