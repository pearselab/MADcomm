####################
# ADD DOIs ##########
#####################

#' @importFrom fulltext ft_get_si

# Plotted land and area covered by various plant species. Repeat species in the same plot were combined into
# - one row and the area covered were summed together
# return a long format table of data
.adler.2007 <- function(...){
    data <- read.csv(ft_get_si("E088-161", "allrecords.csv", from = "esa_archives"))
    comm <- with(data, tapply(area, list(species, plot_year), sum, na.rm=TRUE))
    return(.matrix.melt(comm))
}

# Species counts of different quads on various years
.anderson.2011 <- function(...){
  data <- read.csv(ft_get_si("10.6084/m9.figshare.3551799.v1", "annuals_counts_v2.csv"))
  data$plot_year <- paste(anderson$quad, anderson$year, sep = "_")
  data <- data[order(data$species), c(3, 5, 4)]
  return (data)
}

.chu.2013 <- function(...){
  #contains plant species and a count, but no location
  data <- read.csv(ft_get_si("10.6084/m9.figshare.3556779.v1", "allrecords_cover.csv"))
  colnames(data) <- tolower(colnames(data))
  data$plot_year <- paste(data$quad, data$year, sep = "_")
  #Combines rows of similar species and plotyear into one row
  comm <- with(data, tapply(area, list(species, plot_year), sum, na.rm=TRUE))
  return(.matrix.melt(comm))
}

# Best approach so far, mainly due to fact that table was already in long format
.lynch.2013 <- function(...){
  full.table <- read.csv(ft_get_si("E094-243", "Antarctic_Site_Inventory_census_data_1994_2012.csv", from = "esa_archives"))
  data <- full.table[c(6,1,9)]
    
  # Compacts remaining unused columns into one column as one big string per entry
  meta.data <- full.table[-c(6,1,9)]
  meta.data <- sapply((1:nrow(meta.data)), function(y) {paste(c(rbind(colnames(meta.data), ":", as.character(meta.data[y,]), ", ")))})
  data$metadata <- meta.data
  
  return(data)
}



.hnselmus.2013 <- function(...){
    library(pez) # This isn't how we declare packages in 'real'
                 # packages for the time being this is sufficient
    data(laja)
    return(.matrix.melt(invert.sites))
}

# Data of plant cover in 100m^2 plots from various years. Cover codes 1-9 represented percentage
# - ranges within the data. The median percentages were taken for each of the cover codes and used
# - as the quantity
# return a long format table
.mcglinn.2010 <- function(...){
  # Data of plant cover in the 100m^2 plot
  data <- read.csv(ft_get_si("E091-124", "TGPP_cover.csv", from = "esa_archives"))
  plot.year <- paste(data$plot, data$year, sep = "_")
  data <- data.frame(species = data$spcode, plot_year = plot.year, cover = data$cover)
  
  # Median percentages for cover codes calculated in decimal form
  percents <- c(0, .005, .015, .035, .075, .175, .375, .675, .875)
  data$cover <- percents[data$cover]
  
  #turns given species codes in to "Genus species" format
  spec_codes <- read.csv(ft_get_si("E091-124", "TGPP_specodes.csv", from = "esa_archives"))
  spec_codes <- with(spec_codes, setNames(paste(genus, species, sep = " "), spcode))
  data$species <- spec_codes[data$species]
  return(data)
}

# Here is another example using the R package fulltext
# - *please* use this package where you can to download data
.thibault.2011 <- function(...){
    # Load data from Internet
    data <- read.csv(ft_get_si("E092-201", "MCDB_communities.csv", "esa-data-archives"))
    # ... transform data into the desired format ...
    return(transformed.data)    
}

# - this one is a dump, but seeing as how it works(ish) I'm just popping it up...
.fia <- function(...){
    .get.fia <- function(state, var){
        zip <- file.path("raw_data","fia_zips",paste0(state,"_",var,".zip"))
        csv <- file.path("raw_data","fia_zips",paste0(state,"_",var,".csv"))
        .download(paste0("http://apps.fs.fed.us/fiadb-downloads/",state,"_",var,".zip"), zip)
        .unzip(zip, paste0(state,"_",var,".csv"), csv)
    }

    #Download and load
    states <- c("ME","NH","VT","MA","RI","CT","NY","NJ","DE","MD","VA","NC","SC","GA","FL","MS","AL","TN","KY","WV","OH","IN","IL","WI","MI")
    #PA missed off!
    data <- vector("list", length(states))
    for(i in seq_along(states)){
        #Download/read in data
        tree <- fread(.get.fia(states[i], "TREE"), select=c("CN","PLT_CN","PLOT","SPCD","DIA","INVYR"))
        cond <- fread(.get.fia(states[i], "COND"), select=c("PLT_CN","PLOT","STDAGE","FORTYPCD","CONDID"))
        plot <- fread(.get.fia(states[i], "PLOT"), select=c("PLOT","LAT","LON","ELEV", "CN"))

        #Subset everything, remove sites with multiple/ambiguous codings, merge
        tree <- tree[INVYR==2010 & DIA > 1.96,]
        cond <- cond[PLT_CN %in% names(Filter(function(x) x==1, table(cond$PLT_CN))),]
        data[[i]] <- merge(tree, merge(cond, plot, by.x="PLT_CN", by.y="CN"), by.x="PLT_CN", by.y="PLT_CN")
        data[[i]]$state <- states[i]
        
        #Delete temporary files
        unlink(.get.fia(states[i], "TREE"))
        unlink(.get.fia(states[i], "COND"))
        unlink(.get.fia(states[i], "PLOT"))
    }
    data <- rbindlist(data)
    t <- setNames(seq_along(unique(data$PLT_CN)), unique(data$PLT_CN))
    data$state.ref <- paste0(data$state, ".", t[data$PLT_CN])
}
