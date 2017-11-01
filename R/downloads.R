####################
# ADD DOIs ##########
#####################

#' @importFrom fulltext ft_get_si

# Plotted land and area covered by various plant species. Repeat species in the same plot were combined into
# - one row and the area covered were summed together
# return a long format table of data
.adler.2007 <- function(...){
    data <- read.csv(ft_get_si("E088-161", "allrecords.csv", from = "esa_archives"))
    comm <- with(data, tapply(area, list(plotyear, species), sum, na.rm=TRUE))
    return(.matrix.melt(comm))
}

# Species counts of different quads on various years
.anderson.2011 <- function(...){
    data <- read.csv(ft_get_si("10.6084/m9.figshare.3551799.v1", "annuals_counts_v2.csv"))
    data$plot_year <- paste(anderson$quad, anderson$year, sep = "_")
    data <- data[order(data$species), c(3, 5, 4)]
    return (data)
}

.baldridge.2013 <- function(...){
    #Done. Site IDs in original data is nonconsequential. 
    abundance_data <- read.csv(ft_get_si("10.6084/m9.figshare.769251.v1", "Species_abundances.csv"))
    site_data <- read.csv(ft_get_si("10.6084/m9.figshare.769251.v1", "Sites_table_abundances.csv"))
    new.site_data <- with(site_data, data.frame(Site_ID = Site_ID, Site_Name = paste(Site_Name, Collection_Year, sep = "_")))
    new.site_data <- na.omit(new.site_data)
    data <- with(abundance_data, data.frame(species = paste(Family,Genus, Species, sep = "_"), plot = Site_ID, count = Abundance))
    data$plot<-new.site_data$Site_Name[match(data$plot, new.site_data$Site_ID)]
    new.data <- with(data, tapply(count, list(plot, species), sum))
    new.data[is.na(new.data)] <- 0
    new.data <- new.data[-1,]
    return(.matrix.melt(new.data))
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

.lynch.2013 <- function(...){
    #Accumulation of 19 years of seabird population abundance data collected by the Antarctic Site Inventory. 
    full.data <- read.csv(ft_get_si("E094-243", "Antarctic_Site_Inventory_census_data_1994_2012.csv", from = "esa_archives"))
    data <- full.data[,c(3,6,8,9)]
    data$site<-with(data, paste(Site_name, Season, sep = "_"))
    data$Count[data$Count>0]<-1
    data$Species <- sub("GEPE", "Pygoscelis_papua", data$Species)
    data$Species <- sub("ADPE", "Pygoscelis_adeliae", data$Species)
    data$Species <- sub("CHPE", "Pygoscelis_antarctica", data$Species)
    data$Species <- sub("MCPE", "Eudyptes_chrysolophus", data$Species)
    data$Species <- sub("BESH", "Phalacrocorax_atriceps", data$Species)
    data$Species <- sub("KEGU", "Larus_dominicanus", data$Species)
    data$Species <- sub("SOGP", "Macronectes_giganteus", data$Species)
    new.data <- with(data, tapply(Count, list(site, Species), sum))
    new.data[is.na(new.data)] <- 0
    new.data[new.data > 0] <- 1

    return(.matrix.melt(data))
}

.helmus.2013 <- function(...){
    library(pez) # This isn't how we declare packages in 'real'
                 # packages for the time being this is sufficient
    data(Laja)
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

.thibault.2011 <- function(...){
    #This one is not done
    abundance.data <- read.csv(ft_get_si("E092-201", "MCDB_communities.csv", from = "esa_archives"))
    site.data <- read.csv(ft_get_si("E092-201", "MCDB_sites.csv", from = "esa_archives"))
    return(transformed.data)    
}

.chazot.2014 <- function(...){
    #Abundance data for butterfly species from seven different sites.
    data <- read.xls(ft_get_si("10.5061/dryad.1534j", "Abundance_dataset.xlsx"), skip = 1)
    data <- data[-c(163,164,165),-c(2,3)]
    rownames(data) <- data[,1]
    data[,1] <- NULL
    return(.matrix.melt(t(data)))
}

.chamailleJammes.2016 <- function(...){
    data <- read.csv(ft_get_si("10.1371/journal.pone.0153639", 1))
    data <- aggregate(. ~ WATERHOLE, data = data, FUN=sum)
    rownames(data) <- data$WATERHOLE
    data[,1] <- NULL
    return(.matrix.melt(data))
}

.broadway.2015 <- function(...){
    #Fish abundance data for Wabash River for years 1974 - 2008.
    data <- read.xls(ft_get_si("10.1371/journal.pone.0124954", 1))
    data$Presence <- 1
    new.data <- with(data, tapply(Presence, list(Year, Species), sum))
    new.data[is.na(new.data)] <- 0
    return(.matrix.melt(new.data))
}

.andradiBrown.2016 <- function(...){
    #Abundance data for coral fish at 7 different sites in the Great Barrier Reef.
    data <- read.csv(ft_get_si("10.1371/journal.pone.0156641",3))
    data <- data[,-c(7)]
    data$Genus <- sub(" ", "", data$Genus)
    data$Family <- sub(" ", "", data$Family)
    data$name <- with(data, paste(Family, Genus, Species, sep = "_"))
    data$Site <- with(data, paste(Site, Zone, Transect, sep = "_"))
    data$Presence <- 1
    new.data <- with(data, tapply(Presence, list(Site, name), sum))
    new.data[is.na(new.data)] <- 0
    return(.matrix.melt(new.data))
}

.rodriguezBuritica.2013 <- function(...){
    data <- read.csv(ft_get_si("E094-083","SMCover.csv",from = "esa_archives"))
    species.data <- read.csv(ft_get_si("E094-083","Species.csv",from = "esa_archives"))
    species.data$ReportedName  <- sub(" ", "_", species.data$ReportedName)
    species.data$AcceptedName <- sub(" ", "_", species.data$AcceptedName)
    data$species <- species.data$AcceptedName[match(data$Code, species.data$Code)]
    data$plot_year <- with(data, paste(Plot, Year, sep = "_"))
    transformed.data  <- with(data, tapply(Cover, list(plot_year, species), sum, na.rm=TRUE))
    return(.matrix.melt(transformed.data))
}

.hellmann.2013 <- function(...){
    temp <- tempfile()
    download.file("http://esapubs.org/archive/ecol/E094/126/MosquitoDB.zip", temp)
    data <- read.csv(unz(temp, "MosquitoDB.csv"))
    unlink(temp)
    data$verbatimspecificepithet <- sub(" ", "_", data$verbatimspecificepithet)
    data$site <- with(data, paste(country, stateprovidence, county, year, sep = "_"))
    data$site <- sub(" ", "_", data$site)
    transformed.data  <- with(data, tapply(individualcount, list(site, verbatimspecificepithet), sum, na.rm=TRUE))
    return(.matrix.melt(transformed.data))
}

.anderson.2012 <- function(...){
    data <- read.csv(ft_get_si("E093-132", "allrecords_point_features.csv", from = "esa_archives"))
    data$plotyear <- with(data, paste(quad, year, sep = "_"))
    comm <- with(data, tapply(Canopy_cov, list(plotyear, Species), sum, na.rm=TRUE))
    return(.matrix.melt(comm))
}

.stevens.2011 <- function(...){
    data <- read.csv(ft_get_si("E092-128", "speciesdata.csv", from = "esa_archives"))
    data[is.na(data)] <- 0
    data$Site.number <- with(data, paste(Site.number, Year, sep = "_"))
    data <- aggregate(. ~ Site.number, data = data, FUN=sum)
    rownames(data) <- data[,1]
    data <- data[,-c(1:4)]
    data[data > 0] <- 1
    return(.matrix.melt(data))
}

.raymond.2011 <- function(...){
    data <- read.csv(ft_get_si("E092-097", "diet.csv", from = "esa_archives"))
    data$date <- format(as.Date(data$OBSERVATION_DATE_END, format="%d/%m/%Y"),"%Y")
    data$date[is.na(data$date)] <- "No.Date"
    data$LOCATION <- as.character(data$LOCATION)
    data$LOCATION[data$LOCATION == ""] <- "No.site"
    data$site.year <- with(data, paste(LOCATION, date, sep = "_"))
    data$PREDATOR_NAME_ORIGINAL <- sub(" ", "_", data$PREDATOR_NAME_ORIGINAL)
    transformed.data <- with(data, tapply(PREDATOR_TOTAL_COUNT, list(site.year, PREDATOR_NAME_ORIGINAL), sum, na.rm = TRUE))
    return(.matrix.melt(transformed.data))
}

.sal.2013 <- function(...){
    species.data <- read.csv(ft_get_si("E094-149", "table3.csv", from = "esa_archives"))
    site.data <- read.csv(ft_get_si("E094-149", "table1.csv", from = "esa_archives"))
    site.data$Date <- format(as.Date(site.data$Date, format="%d-%m-%Y"),"%Y")
    site.data$site.year <- with(site.data, paste(SampleID, Date, sep = "_"))
    species.data$X <- site.data$site.year[match(species.data$X, site.data$SampleID)]
    rownames(species.data) <- species.data$X
    species.data[species.data > 0] <- 1
    return(.matrix.melt(species.data))
}

.laverick.2017 <- function(...){
    data <- read.csv(ft_get_si("10.1371/journal.pone.0183075",7))
    rownames(data) <- data$X
    data <- data[,-c(44:46)]
    return(.matrix.melt(data))
}

.jian.2014 <- function(...){
    data <- read.csv(ft_get_si("10.1371/journal.pone.0114301", 5))
    data$site <- with(data, paste(site, date, sep = "_"))
    transformedData <- aggregate(. ~ site, data = data[,-1], FUN=sum)
    rownames(transformedData) <- transformedData$site
    transformedData <- transformedData[,-1]
    return(.matrix.melt(transformedData))
}

.ogutu.2017 <- function(...){
    data <- read.xls(ft_get_si("10.1371/journal.pone.0169730", 3))
    data <- data[,1:3]
    data$site <- "Nakuru.Wildlife.Conservancy"
    data$site <- with(data, paste(site, Date, sep = "_"))
    transformedData <- with(data, tapply(Count, list(site, Species), sum, na.rm = TRUE))
    return(.matrix.melt(transformedData))
}

.gallmetzer.2017 <- function(...){
    #No dates given for collections
    data <- read.xls(ft_get_si("10.1371/journal.pone.0180820", 1))
    data <- data[-c(1,58,114,116,120,122:nrow(data)),-c(2:5, 78)]
    rownames(data) <- data$species
    data <- data[,-1]
    transformedData <- as.data.frame(t(data))
    return(.matrix.melt(transformedData))
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