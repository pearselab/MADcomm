####################
# ADD DOIs ##########
#####################


.helmus.2013 <- function(...){
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
