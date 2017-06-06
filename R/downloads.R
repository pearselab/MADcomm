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
