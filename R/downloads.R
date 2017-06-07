####################
# ADD DOIs ##########
#####################

#' @importFrom fulltext ft_get_si

.adler.2007 <- function(...){
  data <- read.csv(ft_get_si("E088-161", "allrecords.csv", from = "esa_archives"))
  metadata <- paste("(", data[,5], ",", data[,6], ")", sep = "")
  data <- data[c(3,1,4)]
  data$metadata <- metadata
  
  #Reordering table to make sort below quicker
  data <- data[order(data$species, data$plotyear),]
  
  #Combines rows of similar species and plotyear into one row
  combined <- data[1,]
  
  m <- 1 #current row of combined data
  p[m] <- 1 #number of combinations made for row m
  
  for(n in 2: nrow(data)){
    if(all(combined[m,(1:2)] == data[n, (1:2)] )){
      combined[m,3] = combined[m,3] + data[n,3]
      combined[m,4] = paste(combined[m,4], ",", data[n,4])
      p[m] = p[m]+1
    }
    else{
      m = m + 1
      combined[m,] <- data[n,]
      p[m] = 1
    }
  }
  
  combined[,4] = paste("num_of_points:", p, ";points:", combined[,4], sep = "")
  
  return(combined)
}

.anderson.2011 <- function(...){
    data <- read.csv("https://ndownloader.figshare.com/files/5619462")
    data <- data[order(data$species), c(3, 1, 2, 4)]
    return (data)
}

.chu.2013 <- function(...){
    #contains plant species and a count, but no location
    species.count <- read.csv(ft_get_si("10.6084/m9.figshare.3556779.v1", "species_list.csv"))
}

# Best approach so far, mainly due to fact that table was already in long format
.lynch.2013 <- function(...){
    full.table <- read.csv(ft_get_si("E094-243", "Antarctic_Site_Inventory_census_data_1994_2012.csv", from = "esa_archives"))
    data <- full.table[c(6,1,9)]
    
    # Compacts remaining unused columns into one column as one big string per entry
    meta.data <- full.table[-c(6,1,9)]
    meta.data <- sapply((1:nrow(meta.data)), function(y) {paste(c(rbind(colnames(meta.data), "=", as.character(meta.data[y,]), ", ")))})
    data$metadata <- meta.data
    
    return(data)
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
