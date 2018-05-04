####################
# ADD DOIs ##########
#####################

#' @importFrom suppdata suppdata
#' @importFrom utils data head read.csv
#' @importFrom stats aggregate na.omit
.adler.2007 <- function(...){
    data <- read.csv(suppdata("E088-161", "allrecords.csv", from = "esa_archives"))
    comm <- with(data, tapply(area, list(plotyear, species), sum, na.rm=TRUE))
    comm[is.na(comm)] <- 0
    year <- as.numeric(paste0("19",substr(rownames(comm), 7, 8)))
    name <- substr(rownames(comm), 1, 4)
    return(.matrix.melt(comm, 
                        data.frame(units="area"),
                        data.frame(id=rownames(comm),year,name,lat="38.8",long="99.3",address="2 miles west of the town of Hays",area="1m2"), 
                        data.frame(species=colnames(comm),taxonomy=NA)))
}

.anderson.2011 <- function(...){
    data <- read.csv(suppdata("10.6084/m9.figshare.3551799.v1", "annuals_counts_v2.csv"), as.is=TRUE)
    data$plot_year <- with(data, paste(quad, year, sep = "_"))
    year <- as.numeric(paste0("19",as.character(data$year)))[!duplicated(data$plot_year)]
    site.id <- unique(data$plot_year)
    name <- data$quad[!duplicated(data$plot_year)]
    data <- data[order(data$species), c(3, 5, 4)]
    return (.df.melt(data$species, 
                     data$plot_year, 
                     data$count, 
                     data.frame(units="#", treatment="grazing"), 
                     data.frame(id=site.id, year,name,lat=NA,long=NA, address="northern mixed prairie in Miles City, Montana, USA", area="1m2"), 
                     data.frame(species=unique(data$species),taxonomy=NA)))
}

# NEON wrappers
# - all of these could use a bit of DRY love; there's a lot of repetition. A task for another day...
# Mammals
#' @importFrom nneo nneo_sites nneo_product nneo_data
.neon.2018a <- function(...){
    # Internal wrapper
    .site <- function(month, site){
        possible <- nneo_data("DP1.10072.001", site, month, "simple")$data$files
        url <- grep(paste0("mam_pertrapnight\\.",month,"\\.basic"), possible$url, value=TRUE)
        if(length(url) > 0){
            data <- read.csv(url, as.is=TRUE)[,c("scientificName", "weight")]
            data <- data[data$scientificName != "",]
            data$weight[is.na(data$weight)] <- -1
            data$month <- month; data$site <- site
            return(data)
        }
        return(NULL)
    }

    # Get site data
    metadata <- nneo_product("DP1.10072.001")$siteCodes
    output <- vector("list", length(metadata$siteCode))
    for(i in seq_along(metadata$siteCode))
        output[[i]] <- do.call(rbind, lapply(unlist(metadata$availableMonths[i]), .site, site=metadata$siteCode[i]))
    output <- do.call(rbind, output)
    output$id <- with(output, paste(site, month, sep="_"))
    output <- na.omit(output)
    
    # Add meta-data and return
    site.data <- nneo_sites()
    site.df <- output[!duplicated(output$id),]
    site.df$lat <- site.data$siteLatitude[match(site.df$site, site.data$siteCode)]
    site.df$long <- site.data$siteLongitude[match(site.df$site, site.data$siteCode)]
    site.df$address <- NA; site.df$area <- "10 sherman traps"
    names(site.df)[names(site.df)=="month"] <- "year"
    names(site.df)[names(site.df)=="site"] <- "name"
    site.df$scientificName <- site.df$weight <- NULL
    return (.df.melt(output$scientificName, output$id, output$weight, data.frame(units="g"), site.df, data.frame(species=unique(output$scientificName),taxonomy=NA)))
}
# Beetles
.neon.2018b <- function(...){
    # Internal wrapper
    .site <- function(month, site){
        possible <- nneo_data("DP1.10022.001", site, month, "simple")$data$files
        url <- grep(paste0("expertTaxonomistIDProcessed\\.",month,"\\.basic"), possible$url, value=TRUE)
        if(length(url) > 0){
            data <- read.csv(url, as.is=TRUE)[,c("scientificName","plotID"), drop=FALSE]
            data$abundance <- -1
            data <- aggregate(.~plotID+scientificName, data=data, FUN=length)
            data$year <- month; data$neon.site <- site
            return(data)
        }
        return(NULL)
    }

    # Get site data
    metadata <- nneo_product("DP1.10022.001")$siteCodes
    output <- vector("list", length(metadata$siteCode))
    for(i in seq_along(metadata$siteCode))
        output[[i]] <- do.call(rbind, lapply(unlist(metadata$availableMonths[i]), .site, site=metadata$siteCode[i]))
    output <- do.call(rbind, output)
    output$id <- with(output, paste(plotID, year, sep="_"))
    output <- na.omit(output)
    
    # Add meta-data and return
    site.data <- nneo_sites()
    site.df <- output[!duplicated(output$id),]
    site.df$lat <- site.data$siteLatitude[match(site.df$neon.site, site.data$siteCode)]
    site.df$long <- site.data$siteLongitude[match(site.df$neon.site, site.data$siteCode)]
    site.df$address <- NA; site.df$area <- "pitfall trap"
    names(site.df)[names(site.df)=="plotID"] <- "name"
    site.df$scientificName <- site.df$weight <- NULL
    return (.df.melt(output$scientificName, output$id, output$abundance, data.frame(units="#"), site.df, data.frame(species=unique(output$scientificName),taxonomy=NA)))
}
# Plants
.neon.2018c <- function(...){
    # Internal wrapper
    .site <- function(month, site){
        possible <- nneo_data("DP1.10098.001", site, month, "simple")$data$files
        url <- grep(paste0("apparentindividual\\.",month,"\\.basic"), possible$url, value=TRUE)
        if(length(url) > 0){
            data <- read.csv(url, as.is=TRUE)[,c("individualID","plotID", "plantStatus")]
            data <- data[grep("Live",data$plantStatus),]
            if(nrow(data)>0){
                # Load lookup; correct for mistakes by sorting on date (see readme of NEON data)
                lookup <- read.csv(grep("mappingandtagging.basic", possible$url, value=TRUE, fixed=TRUE), as.is=TRUE)
                lookup <- lookup[order(lookup$date,decreasing=TRUE),]
                lookup <- lookup[!duplicated(lookup$individualID),]
                lookup <- lookup[,c("scientificName","individualID")]
                data <- merge(data, lookup, "individualID")
                data$scientificName <- sapply(strsplit(data$scientificName, " "), function(x) paste(x[1:2], collapse="_"))
                data$plantStatus <- NULL; data$abundance <- -1
                data <- aggregate(.~plotID+scientificName, data=data, FUN=length)
                data$neon.site <- site; data$year <- month
                return(data)
            }
        }
        return(NULL)
    }

    # Get site data
    metadata <- nneo_product("DP1.10098.001")$siteCodes
    output <- vector("list", length(metadata$siteCode))
    for(i in seq_along(metadata$siteCode))
        output[[i]] <- do.call(rbind, lapply(unlist(metadata$availableMonths[i]), .site, site=metadata$siteCode[i]))
    output <- do.call(rbind, output)
    output$id <- with(output, paste(plotID, year, sep="_"))
    output <- na.omit(output)
    
    # Add meta-data and return
    site.data <- nneo_sites()
    site.df <- output[!duplicated(output$id),]
    site.df$lat <- site.data$siteLatitude[match(site.df$neon.site, site.data$siteCode)]
    site.df$long <- site.data$siteLongitude[match(site.df$neon.site, site.data$siteCode)]
    site.df$address <- NA; site.df$area <- "1m2"
    names(site.df)[names(site.df)=="plotID"] <- "name"
    site.df$scientificName <- site.df$weight <- NULL
    return (.df.melt(output$scientificName, output$id, output$abundance, data.frame(units="#"), site.df, data.frame(species=unique(output$scientificName),taxonomy=NA)))
}

.chu.2016 <- function(...){
    data <- read.csv(suppdata("10.6084/m9.figshare.3556779.v1", "allrecords_cover.csv"))
    site.info <- read.csv(suppdata("10.6084/m9.figshare.3556779.v1", "quad_info.csv"))
    colnames(data) <- tolower(colnames(data))
    data$plot_year <- paste(data$quad, data$year, sep = ".")
    #Combines rows of similar species and plotyear into one row
    comm <- with(data, tapply(area, list(plot_year, species), sum, na.rm=TRUE))
    plots_years <- unlist(strsplit(rownames(comm), ".", fixed=T))
    plots <- plots_years[seq(1,length(plots_years), 2)]
    years <- plots_years[seq(2,length(plots_years), 2)]
    latitude <- site.info$latitude[match(plots, site.info$quadrat)]
    longitude <- site.info$longitude[match(plots, site.info$quadrat)]
    comm[is.na(comm)] <- 0
    return(.matrix.melt(comm, 
                        data.frame(units="area", treatment="grazing"), 
                        data.frame(id=rownames(comm), year=years, name=plots, lat=latitude, long=longitude, address="Central Plains Experimental Range in Nunn, Colorado, USA", area="1m2"),
                        data.frame(species=colnames(comm), taxonomy=NA)))
}

.lynch.2013 <- function(...){
    full.data <- read.csv(suppdata("E094-243", "Antarctic_Site_Inventory_census_data_1994_2012.csv", from = "esa_archives"))
    full.data$Site_name <- gsub("[^A-Za-z0-9_.]", "", full.data$Site_name)
    data <- full.data[,c(3,6,8,9)]
    data$site<-with(data, paste(Site_name, Season, sep = "_"))
    data$Count[data$Count>0] <- 1
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
    plots_years <- unlist(strsplit(rownames(new.data), "_", fixed=T))
    plots <- plots_years[seq(1,length(plots_years), 2)]
    years <- plots_years[seq(2,length(plots_years), 2)]
    latitude <- full.data$Latitude[match(plots, full.data$Site_name)]
    longitude <- full.data$Longitude[match(plots, full.data$Site_name)]
    return(.matrix.melt(new.data, 
                        data.frame(units="#"), 
                        data.frame(id=rownames(new.data), year=years, name=plots, lat=latitude, long=longitude, address="Antarctic Site Inventory", area=NA),
                        data.frame(species=colnames(new.data), taxonomy="Spheniscidae")))
}

#' @importFrom gdata read.xls
.broadway.2015 <- function(...){
    #Fish abundance data for Wabash River for years 1974 - 2008.
    data <- read.xls(suppdata("10.1371/journal.pone.0124954", 1))
    data$Presence <- 1
    new.data <- with(data, tapply(Presence, list(Year, Species), sum))
    colnames(new.data) <- gsub(" ", "_", colnames(new.data))
    site <- "wabash.river"
    years <- rownames(new.data)
    rownames(new.data) <- paste(site, rownames(new.data), sep="_")
    new.data[is.na(new.data)] <- 0
    return(.matrix.melt(new.data,
                        data.frame(units="#"),
                        data.frame(id=rownames(new.data), year=years, name=site, lat=NA, long=NA, address="Wabash River, Midwest, USA", area=NA),
                        data.frame(species=colnames(new.data), taxonomy="Chordata")))
}

clean.predicts <- function(data) {
    data.temp <- data[which(data$Genus != ""),]
    data.temp$Genus <- as.character(data.temp$Genus)
    data.temp$Species <- as.character(data.temp$Species)
    data.temp$Species[which(data.temp$Species == "")] <- "spp."
    data.temp$species <- with(data.temp, paste(Genus, Species, sep="_"))
    data.temp$sites <- with(data.temp, paste(Site_name, Site_number, sep="_"))
    sites <- with(data.temp, paste(Site_name, Site_number, sep="_"))
    year <- format(as.Date(data.temp$Sample_start_earliest, format="%Y-%m-%d"),"%Y")
    data.temp$site_year <- paste(sites, year, sep="_")
    return(data.temp)
}

.predicts.2016a <- function(...) {
    download.file("http://data.nhm.ac.uk/dataset/902f084d-ce3f-429f-a6a5-23162c73fdf7/resource/1e82548a-5f1e-4d32-861f-e00a740ea296/download/database.rds", "predicts_abundance.RDS")
    data <- readRDS("predicts_abundance.RDS")
    data <- data[which(data[,6] == "abundance"),]
    data <- clean.predicts(data)
    year <- format(as.Date(data$Sample_start_earliest[!duplicated(data$site_year)], format="%Y-%m-%d"),"%Y")
    site.id <- unique(data$site_year)
    name <- data$sites[!duplicated(data$site_year)]
    long <- data$Longitude[!duplicated(data$site_year)]
    lat <- data$Latitude[!duplicated(data$site_year)]
    return(.df.melt(data$species,
                    data$site_year,
                    data$Measurement,
                    data.frame(units="#"), 
                    data.frame(id=site.id, year=year, name=name, lat, long, address=NA, area=NA), 
                    data.frame(species=unique(data$species), taxonomy=NA)))
}

.predicts.2016b <- function(...) {
    download.file("http://data.nhm.ac.uk/dataset/902f084d-ce3f-429f-a6a5-23162c73fdf7/resource/1e82548a-5f1e-4d32-861f-e00a740ea296/download/database.rds", "predicts.RDS")
    data <- readRDS("predicts.RDS")
    data <- data[which(data[,6] == "occurrence"),]
    data <- clean.predicts(data)
    year <- format(as.Date(data$Sample_start_earliest[!duplicated(data$site_year)], format="%Y-%m-%d"),"%Y")
    site.id <- unique(data$site_year)
    name <- data$sites[!duplicated(data$site_year)]
    long <- data$Longitude[!duplicated(data$site_year)]
    lat <- data$Latitude[!duplicated(data$site_year)]
    return(.df.melt(data$species,
                    data$site_year,
                    data$Measurement,
                    data.frame(units="area"), 
                    data.frame(id=site.id, year, name, lat, long, address=NA, area=NA), 
                    data.frame(species=unique(data$species), taxonomy=NA)))
}

.predicts.2016c <- function(...) {
    download.file("http://data.nhm.ac.uk/dataset/902f084d-ce3f-429f-a6a5-23162c73fdf7/resource/1e82548a-5f1e-4d32-861f-e00a740ea296/download/database.rds", "predicts.RDS")
    data <- readRDS("predicts.RDS")
    data <- data[which(data[,6] == "percent cover"),]
    data <- clean.predicts(data)
    year <- format(as.Date(data$Sample_start_earliest[!duplicated(data$site_year)], format="%Y-%m-%d"),"%Y")
    site.id <- unique(data$site_year)
    name <- data$sites[!duplicated(data$site_year)]
    long <- data$Longitude[!duplicated(data$site_year)]
    lat <- data$Latitude[!duplicated(data$site_year)]
    return(.df.melt(data$species,
                    data$site_year,
                    data$Measurement,
                    data.frame(units="biomass"), 
                    data.frame(id=site.id, year, name, lat, long, address=NA, area=NA), 
                    data.frame(species=unique(data$species), taxonomy=NA)))
}

.predicts.2016d <- function(...) {
    download.file("http://data.nhm.ac.uk/dataset/902f084d-ce3f-429f-a6a5-23162c73fdf7/resource/1e82548a-5f1e-4d32-861f-e00a740ea296/download/database.rds", "predicts.RDS")
    data <- readRDS("predicts.RDS")
    data <- data[which(data[,6] == "biomass"),]
    data <- clean.predicts(data)
    year <- format(as.Date(data$Sample_start_earliest[!duplicated(data$site_year)], format="%Y-%m-%d"),"%Y")
    site.id <- unique(data$site_year)
    name <- data$sites[!duplicated(data$site_year)]
    long <- data$Longitude[!duplicated(data$site_year)]
    lat <- data$Latitude[!duplicated(data$site_year)]
    return(.df.melt(data$species,
                    data$site_year,
                    data$Measurement,
                    data.frame(units="p/a"), 
                    data.frame(id=site.id, year, name, lat, long, address=NA, area=NA), 
                    data.frame(species=unique(data$species), taxonomy=NA)))
}

# Data of plant cover in 100m^2 plots from various years. Cover codes 1-9 represented percentage
# - ranges within the data. The median percentages were taken for each of the cover codes and used
# - as the quantity
# return a long format table
.mcglinn.2010 <- function(...){
    # Data of plant cover in the 100m^2 plot
    data <- read.csv(suppdata("E091-124", "TGPP_cover.csv", from = "esa_archives"))
    plot.year <- paste(data$plot, data$year, sep = "_")
    data <- data.frame(species = data$spcode, plot_year = plot.year, cover = data$cover)
  
    # Median percentages for cover codes calculated in decimal form
    percents <- c(0, .005, .015, .035, .075, .175, .375, .675, .875)
    data$cover <- percents[data$cover]
  
    #turns given species codes in to "Genus species" format
    spec_codes <- read.csv(suppdata("E091-124", "TGPP_specodes.csv", from = "esa_archives"))
    spec_codes <- with(spec_codes, setNames(paste(genus, species, sep = "_"), spcode))
    data$species <- spec_codes[data$species]
    data$species[is.na(data$species)] <- "spp."
    site.id <- unique(data$plot_year)
    temp <- strsplit(levels(drop.levels(site.id)), "_")
    year <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,2]
    name <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,1]

    # This function has metadata that could be added for burn 
    return(.df.melt(data$species, 
                    data$plot_year, 
                    data$cover, 
                    data.frame(units="area"), 
                    data.frame(id=site.id, year, name, lat=NA, long=NA, address="Tallgrass Prairie Preserve in Osage County, Oklahoma, USA", area=NA),
                    data.frame(species=unique(data$species), taxonomy="plantae")))
}

.chazot.2014 <- function(...){
    #Abundance data for butterfly species from seven different sites.
    data <- read.xls(suppdata("10.5061/dryad.1534j", "Abundance_dataset.xlsx"), skip = 1)
    data <- data[-c(163,164,165),-c(2,3)]
    rownames(data) <- data[,1]
    data[,1] <- NULL
    data <- t(data)
    return(.matrix.melt(data,
                        data.frame(units="#"),
                        data.frame(id=rownames(data), year=NA, name=rownames(data), lat=NA, long=NA, address=NA, area=NA),
                        data.frame(species=colnames(data), taxonomy="Lepidoptera")))
}

.rodriguezBuritica.2013 <- function(...){
    data <- read.csv(suppdata("E094-083","SMCover.csv",from = "esa_archives"))
    species.data <- read.csv(suppdata("E094-083","Species.csv",from = "esa_archives"))
    species.data$ReportedName  <- sub(" ", "_", species.data$ReportedName)
    species.data$AcceptedName <- sub(" ", "_", species.data$AcceptedName)
    data$species <- species.data$AcceptedName[match(data$Code, species.data$Code)]
    data$plot_year <- with(data, paste(Plot, Year, sep = "_"))
    transformed.data  <- with(data, tapply(Cover, list(plot_year, species), sum, na.rm=TRUE))
    transformed.data[is.na(transformed.data)] <- 0
    temp <- strsplit(rownames(transformed.data), "_")
    year <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,2]
    name <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,1]
    return(.matrix.melt(transformed.data, 
                        data.frame(units="#"), 
                        data.frame(id=rownames(transformed.data), year, name, lat=NA, long=NA, address=NA, area=NA),
                        data.frame(species=colnames(transformed.data), taxonomy=NA)))
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
    transformed.data[is.na(transformed.data)] <- 0
    year <- gsub("[^0-9.]", "", rownames(transformed.data))
    name <- gsub("[^A-Za-z_.]", "", rownames(transformed.data))
    return(.matrix.melt(transformed.data,
                        data.frame(units="#"),
                        data.frame(id=rownames(transformed.data), year, name, lat=NA, long=NA, address=NA, area=NA),
                        data.frame(species=colnames(transformed.data), taxonomy="Diptera")))
}

.anderson.2012 <- function(...){
    data <- read.csv(suppdata("E093-132", "allrecords_point_features.csv", from = "esa_archives"))
    data$plotyear <- with(data, paste(quad, year, sep = "_"))
    comm <- with(data, tapply(Canopy_cov, list(plotyear, Species), sum, na.rm=TRUE))
    comm[is.na(comm)] <- 0
    temp <- strsplit(rownames(comm), "_")
    year <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,2]
    name <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,1]
    return(.matrix.melt(comm, 
                        data.frame(units="area", treatment="grazing"),
                        data.frame(id=rownames(comm), year, name, lat=NA, long=NA, address="Santa Rita Experimental Range, Arizona, USA", area="1m2"),
                        data.frame(species=colnames(comm), taxonomy="Plantae")))
}

.andradiBrown.2016 <- function(...){
    #Abundance data for coral fish at 7 different sites in the Great Barrier Reef.
    data <- read.csv(suppdata("10.1371/journal.pone.0156641",3))
    data <- data[,-c(7)]
    data$Genus <- sub(" ", "", data$Genus)
    data$Family <- sub(" ", "", data$Family)
    data$name <- with(data, paste(Family, Genus, Species, sep = "_"))
    data$Site <- with(data, paste(Site, Zone, Transect, sep = "_"))
    data$Presence <- 1
    new.data <- with(data, tapply(Presence, list(Site, name), sum))
    new.data[is.na(new.data)] <- 0
    site <- substr(rownames(new.data), 1,3)
    return(.matrix.melt(new.data,
                        data.frame(units="#"),
                        data.frame(id=rownames(new.data), year=NA, name=site, lat=NA, long=NA, address="Utila, Bay Islands, Hondura", area=NA),
                        data.frame(species=colnames(new.data), taxonomy="Chordata")))
}

.stevens.2011 <- function(...){
    data <- read.csv(suppdata("E092-128", "speciesdata.csv", from="esa_archives"), encoding="latin1")
    metadata <- read.csv(suppdata("E092-128", "environmentaldata.csv", from="esa_archives"), encoding="latin1")
    data[is.na(data)] <- 0
    data$Site.number <- with(data, paste(Site.number, Quadrat, Year, sep = "_"))
    data <- aggregate(. ~ Site.number, data = data, FUN=sum)
    rownames(data) <- data[,1]
    data <- data[,-c(1:4)]
    data[data > 0] <- 1
    data <- as.data.frame(data, row.names=rownames(data), colnames=colnames(data))
    name <- substr(rownames(data), 1, 5)
    year <- substr(rownames(data), 9, 12)
    lat <- metadata$Latitude[match(name, metadata$Site.no.)]
    long <- metadata$Longitude[match(name, metadata$Site.no.)]
    return(.matrix.melt(as.matrix(data),
                        data.frame(units="p/a"),
                        data.frame(id=rownames(data), year, name, lat, long, address=NA, area=NA),
                        data.frame(species=colnames(data), taxonomy=NA)))
}

.laverick.2017 <- function(...){
    data <- read.csv(suppdata("10.1371/journal.pone.0183075",7), stringsAsFactors=FALSE)
    rownames(data) <- data$X
    data <- data[,-c(1,44:46)]
    return(.matrix.melt(as.matrix(data),
                        data.frame(units="#"),
                        data.frame(id=rownames(data), year=NA, name=NA, lat=NA, long=NA, address="Island of Utila, Honduras", area=NA),
                        data.frame(species=colnames(data), taxonomy=NA)))
}

.jian.2014 <- function(...){
    data <- read.csv(suppdata("10.1371/journal.pone.0114301", 5))
    data$site <- with(data, paste(site, date, sep = "_"))
    transformedData <- aggregate(. ~ site, data = data[,-1], FUN=sum)
    rownames(transformedData) <- transformedData$site
    transformedData <- transformedData[,-1]
    temp <- strsplit(rownames(transformedData), "_")
    year <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,2]
    year <- format(as.Date(year, format="%d/%m/%Y"),"%Y")
    name <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,1]
    latValues <- c(-78.1675, -77.9966, -78.6112)
    longValues <- c(34.0588, 34.2157, 33.9296)
    lat <- name
    long <- name
    for (i in unique(lat))
        lat[which(lat == i)]  <- latValues[which(unique(lat) == i)]
    for (j in unique(long))
        long[which(long == j)]  <- longValues[which(unique(long) == j)]
    for (i in seq(ncol(transformedData)-1))
        transformedData[,i] <- as.numeric(transformedData[,i])
    return(.matrix.melt(as.matrix(transformedData), 
                        data.frame(units="#"),
                        data.frame(id=rownames(transformedData), year, name, lat, long, address="Brunswick County, North Carolina, USA", area=NA),
                        data.frame(species=colnames(transformedData), taxonomy=NA)))
}

.heidi.2018 <- function(...) {
    data <- read.xls("Heidi_Species_Cover_2017_Final_121817.xlsx", sheet=2, stringsAsFactors=FALSE)
    metadata <- read.xls("SiteSpeciesList_argon.xlsx", fileEncoding="latin1", stringsAsFactors=FALSE)
    data$geo <- metadata$Lat[match(data$Site, metadata$Site.Name)]
    data$lat <- NA
    data$long <- NA
    temp <- strsplit(data$geo, split=",")
    data$lat[1:471] <- matrix(unlist(temp[1:471]), ncol=2, byrow=TRUE)[,1]
    data$long[1:471] <- matrix(unlist(temp[1:471]), ncol=2, byrow=TRUE)[,2]
    data$Date <- format(as.Date(data$Date, format="%Y-%m-%d"),"%Y")
    data$site_plot <- with(data, paste(Site, Plot, Date, sep="_"))
    site.id  <- unique(data$site_plot)
    year <- data$Date[!duplicated(data$site_plot)]
    name <- data$Site[!duplicated(data$site_plot)]
    return(.df.melt(data$Species.Ground.Cover,
                    data$site_plot,
                    data$Count,
                    data.frame(units="#"),
                    data.frame(id=unique(data$site_plot), year, name, lat=data$lat[!duplicated(data$site_plot)], long=data$long[!duplicated(data$site_plot)], address=NA, area=NA),
                    data.frame(species=unique(data$Species.Ground.Cover), taxonomy=NA)))
}

.ogutu.2017 <- function(...){
    data <- read.xls(suppdata("10.1371/journal.pone.0169730", 3))
    data <- data[,1:3]
    data$site <- "Nakuru.Wildlife.Conservancy"
    data$site <- with(data, paste(site, Date, sep = "_"))
    transformedData <- with(data, tapply(Count, list(site, Species), sum, na.rm = TRUE))
    transformedData[is.na(transformedData)] <- 0
    temp <- strsplit(rownames(transformedData), "_")
    year <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,2]
    year <- format(as.Date(year, format="%Y-%m-%d"),"%Y")
    name <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,1]
    return(.matrix.melt(transformedData, 
                        data.frame(units="#"),
                        data.frame(id=rownames(transformedData), year, name, lat="-0.3667", long="36.0833", address="Nakuru Wildlife Conservancy, Kenya, Africa", area=NA),
                        data.frame(species=colnames(transformedData), taxonomy=NA)))
}

.baldridge.2013 <- function(...){
    abundance_data <- read.csv(suppdata("10.6084/m9.figshare.769251.v1", "Species_abundances.csv"))
    site_data <- read.csv(suppdata("10.6084/m9.figshare.769251.v1", "Sites_table_abundances.csv"))
    new.site_data <- with(site_data, data.frame(Site_ID = Site_ID, Site_Name = Site_Name, year=Collection_Year))
    new.site_data <- na.omit(new.site_data)
    new.site_data$plot_year <- with(new.site_data, paste(Site_Name, year, sep="_")) 
    data <- with(abundance_data, data.frame(species = paste(Family,Genus, Species, sep = "_"), plot = Site_ID, count = Abundance))
    data$plot <- new.site_data$Site_Name[match(data$plot, new.site_data$Site_ID)]
    data$plot_year <- new.site_data$plot_year[match(data$plot, new.site_data$Site_Name)]
    data$year <- new.site_data$year[match(data$plot, new.site_data$Site_Name)]
    data$count[is.na(data$count)] <- 0
    data <- na.omit(data) 
    return(.df.melt(data$species, 
                    data$plot_year,
                    data$count, 
                    data.frame(units="#"), 
                    data.frame(id=unique(data$plot_year), year=data$year[!duplicated(data$plot_year)], name=data$plot[!duplicated(data$plot_year)], lat=NA, long=NA, address=NA, area=NA),
                    data.frame(species=unique(data$species), taxonomy=NA)))
}

.gallmetzer.2017 <- function(...){
    data <- read.xls(suppdata("10.1371/journal.pone.0180820", 1), stringsAsFactors=FALSE)
    data <- data[-c(1,58,114,116,120,122:nrow(data)),-c(2:5, 78)]
    rownames(data) <- data$species
    data <- data[,-1]
    transformedData <- t(data)
    transformedData[is.na(transformedData)] <- 0
    transformedData <- drop.levels(transformedData)
    mode(transformedData) <- "numeric"
    return(.matrix.melt(transformedData, 
                        data.frame(units="#"), 
                        data.frame(id=rownames(transformedData), year=NA, name=NA, lat="45.7354", long="13.6005", address="Northern Adriatic Sea", area=NA),
                        data.frame(species=colnames(transformedData), taxonomy="Mollusca")))
}

.sal.2013 <- function(...){
    species.data <- read.csv(suppdata("E094-149", "table3.csv", from = "esa_archives"), stringsAsFactors=FALSE)
    site.data <- read.csv(suppdata("E094-149", "table1.csv", from = "esa_archives"), stringsAsFactors=FALSE)
    site.data$Date <- format(as.Date(site.data$Date, format="%d-%m-%Y"),"%Y")
    site.data$site.year <- with(site.data, paste(SampleID, Date, sep = "_"))
    species.data$site.year <- site.data$site.year[match(species.data$X, site.data$SampleID)]
    year <- site.data$Date[match(species.data$X, site.data$SampleID)]
    name <- site.data$SampleID[match(species.data$X, site.data$SampleID)]
    lat <- site.data$Lat[match(species.data$X, site.data$SampleID)]
    long <- site.data$Lon[match(species.data$X, site.data$SampleID)]
    rownames(species.data) <- species.data$site.year
    species.data$X <- NULL
    species.data$site.year <- NULL
    return(.matrix.melt(as.matrix(species.data),
                        data.frame(units="area"),
                        data.frame(id=rownames(species.data), year, name, lat, long, address=NA, area="cells/mL"),
                        data.frame(species=colnames(species.data), taxonomy="Chromista")))
}

.raymond.2011 <- function(...){
    data <- read.csv(suppdata("E092-097", "diet.csv", from = "esa_archives"), encoding="latin1")
    data$date <- format(as.Date(data$OBSERVATION_DATE_END, format="%d/%m/%Y"),"%Y")
    data$date[is.na(data$date)] <- "No_Date"
    data$LOCATION <- as.character(data$LOCATION)
    data$LOCATION[data$LOCATION == ""] <- "No_site"
    data$site.year <- with(data, paste(LOCATION, date, sep = "_"))
    data$PREDATOR_NAME_ORIGINAL <- sub(" ", "_", data$PREDATOR_NAME_ORIGINAL)
    transformed.data <- with(data, tapply(PREDATOR_TOTAL_COUNT, list(site.year, PREDATOR_NAME_ORIGINAL), sum, na.rm = TRUE))
    year <- data$date[match(rownames(transformed.data), data$site.year)]
    name <- data$LOCATION[match(rownames(transformed.data), data$site.year)]
    long <- data$WEST[match(rownames(transformed.data), data$site.year)]
    lat <- data$NORTH[match(rownames(transformed.data), data$site.year)]
    transformed.data[is.na(transformed.data)] <- 0
    return(.matrix.melt(transformed.data,
                        data.frame(units="#"),
                        data.frame(id=rownames(transformed.data), year, name, lat, long, address=NA, area=NA),
                        data.frame(species=colnames(transformed.data), taxonomy=NA)))
}

#' @importFrom reshape2 melt
.boyle.2015 <- function(...) {
    # This dataset is combining 23 years of observations at one site. They combine
    # the counts (which are not in the dataset) to get the mean abundance across 
    # <= 23 years...
    # They also give the mean counts during selected years (89-94 and 06-11) so 
    # I have used those instead...
    data <- read.csv(suppdata("10.5061/dryad.65v10", "LaSelvaBirdTrendsDatatable.csv"), stringsAsFactors=FALSE)
    data <- melt(data, id.vars=1,11:12)
    data$site <- gsub("MeanCount", "LaSelvaBiologicalStation_CostaRica", data$variable)
    return(.df.melt(data$ScientificName, data$site, data$value,
                    data.frame(units="#"),
                    data.frame(id=unique(data$site), year=c("89-94", "06-11"), name=NA, lat="10.422", long="-84.015", address="LaSelvaBiologicalStation_CostaRica", area=NA),
                    data.frame(species=unique(data$ScientificName), taxonomy="Aves")))
}

.myster.2010 <- function(...){
    addr  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-luq/100/246250/f718e683c7c425207c7d1f7adeddf85f"
    addr <- sub("^https","http", addr)
    data <-read.csv(addr, header=F, skip=1, sep=",", col.names=c("date", "plot", "species", "percent.cover"), check.names=TRUE)
    data$date <- format(as.Date(data$date, format="%d/%m/%Y"),"%Y")
    data$plot.year <- with(data, paste(plot, date, sep="_"))
    site.id <- unique(data$plot.year)
    year <- data$date[!duplicated(data$plot.year)]
    name <- data$plot[!duplicated(data$plot.year)]
    return(.df.melt(data$species, data$plot.year, data$percent.cover,
                    data.frame(units="area"),
                    data.frame(id=site.id, year, name, lat="-65.8257", long="18.3382", address="Luquillo Experimental Forest, Puerto Rico, USA", area="2mX5m"),
                    data.frame(species=unique(data$species), taxonomy="Plantae")))
}

.schmitt.2012 <- function(...){
    addr  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/46/3/4ded739e78e50552837cf100f251f7ab"
    addr <- sub("^https","http",addr)
    data <-read.csv(addr,header=F, skip=1, sep=",", quot='"',
                    col.names=c("YEAR", "MONTH", "DATE", "SITE", "DEPTH", "REP",
                                "SP_CODE", "COUNT", "COMMENTS", "Common_Name",
                                "taxon_GROUP", "SURVEY", "taxon_PHYLUM",
                                "taxon_CLASS", "taxon_ORDER", "taxon_FAMILY",
                                "taxon_GENUS", "taxon_SPECIES"), check.names=TRUE)
    data$species <- with(data, paste(taxon_GENUS, taxon_SPECIES, sep="_"))
    data$site.year.depth <- with(data, paste(SITE, YEAR, DEPTH, sep="_"))
    site.id <- unique(data$site.year.depth)
    year <- data$YEAR[!duplicated(data$site.year.depth)]
    name <- data$SITE[!duplicated(data$site.year.depth)]
    return(.df.melt(data$species, data$site.year.depth, data$COUNT, 
                    data.frame(units="#"),
                    data.frame(id=site.id, year, name, lat=NA, long=NA, address="Santa Cruz Island, CA, USA", area=NA),
                    data.frame(species=unique(data$species), taxonomy="Pycnopodia")))
}

.reed.2017a <- function(...) {
    data <-read.csv("http://pasta.lternet.edu/package/data/eml/knb-lter-sbc/17/30/a7899f2e57ea29a240be2c00cce7a0d4", as.is=TRUE)
    names(data) <- tolower(names(data))
    data$count[data$count < 0] <- 0
    data$taxon_species[data$taxon_species == -99999] <- NA
    data$taxon_genus[data$taxon_genus == -99999] <- NA
    
    data$species <- with(data, paste(taxon_genus, taxon_species, sep="_"))
    data$site <- with(data, paste(site, transect, sep="_"))
    data$site_year <- with(data, paste(site, year, sep="_"))
    data <- with(data, tapply(count, list(site_year, species), sum, na.rm = TRUE))
    data[is.na(data)] <- 0
    temp <- strsplit(rownames(data), "_")
    year <- matrix(unlist(temp), ncol=3, byrow=TRUE)[,3]
    name <- matrix(unlist(temp), ncol=3, byrow=TRUE)[,1]
    return(.matrix.melt(data,
                    data.frame(units="#"),
                    data.frame(id=rownames(data), year, name, lat=NA, long=NA, address=NA, area=NA),
                    data.frame(species=colnames(data), taxonomy=NA)))
}

.reed.2017b <- function(...) {
    data <-read.csv("https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/19/23/5daf0da45925ba9014872c6bc9f6c8bb")
    names(data) <- tolower(names(data))
    data$count[data$count < 0] <- 0
    data$taxon_species[data$taxon_species == -99999] <- NA
    data$species <- with(data, paste(taxon_genus, taxon_species, sep="_"))
    data$site <- with(data, paste(site, transect, sep="_"))
    data$site_year <- with(data, paste(site, year, sep="_"))
    data <- with(data, tapply(count, list(site_year, species), sum, na.rm = TRUE))
    data[is.na(data)] <- 0
    temp <- strsplit(rownames(data), "_")
    year <- matrix(unlist(temp), ncol=3, byrow=TRUE)[,3]
    name <- matrix(unlist(temp), ncol=3, byrow=TRUE)[,1]
    return(.matrix.melt(data,
                    data.frame(units="#"),
                    data.frame(id=rownames(data), year, name, lat=NA, long=NA, address=NA, area=NA),
                    data.frame(species=colnames(data), taxonomy=NA)))
}

.nichols.2006 <- function(...) {
    addr  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/61/3/wgnhs_macrophyte_aquaplt2" 
    addr <- sub("^https","http",addr) 
    abundanceData <-read.csv(addr, header=F, skip=1, sep=",", quot='"',
                             col.names=c("mwbc", "lake_unique", "lakename", 
                                         "county", "county_id", "month", "year4", 
                                         "spcode", "aqstano", "visual_abundance"),
                             check.names=TRUE)
    specAddr  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/61/3/wgnhs_macrophyte_pltname" 
    specAddr <- sub("^https","http",specAddr) 
    specData <-read.csv(specAddr, header=F, skip=1, sep=",", quot='"', 
                   col.names=c("spcode", "spec_no", "scientific_name", 
                                "common_name", "lifeform", "spec_category", 
                                "genus"), check.names=TRUE)
    abundanceData$site.year <- with(abundanceData, paste(lakename, year4, sep=">"))
    abundanceData$species <- specData$scientific_name[match(abundanceData$spcode, specData$spcode)]
    data <- with(abundanceData, tapply(visual_abundance, list(site.year, species), sum, na.rm = TRUE))
    data[is.na(data)] <- 0
    temp <- unlist(strsplit(rownames(data), ">", fixed=T))
    name <- temp[seq(1,length(temp), 2)]
    year <- temp[seq(2,length(temp), 2)]
    return(.matrix.melt(data, 
                        data.frame(units="#"), 
                        data.frame(id=rownames(data), year, name, lat=NA, long=NA, address=NA, area=NA),
                        data.frame(species=colnames(data), taxonomy=NA)))
}

.wearn.2016a <- function(...) {
    addr <- "https://zenodo.org/record/44545/files/Wearn2016_cameratrap_species-abundance_matrix.csv"
    addr <- sub("^https","http",addr)
    data <- read.csv(addr)
    data <- aggregate(.~Location, data, sum)
    rownames(data) <- data$Location
    data <- data[,-1]
    data <- as.matrix(data, row.names=rownames(data), colnames=colnames(data))
    return(.matrix.melt(data, 
                        data.frame(units="#", method="cameraTrap"),
                        data.frame(id=rownames(data), year=NA, name=rownames(data), lat=NA, long=NA, address="Sabah, Malaysian Borneo", area=NA),
                        data.frame(species=colnames(data), taxonomy="Mammalia")))
}

.wearn.2016b <- function(...) {
    addr <- "https://zenodo.org/record/44545/files/Wearn2016_livetrap_species-abundance_matrix.csv"
    addr <- sub("^https","http",addr)
    data <- read.csv(addr)
    data <- aggregate(.~Location, data, sum)
    rownames(data) <- data$Location
    data <- data[,-1]
    data <- as.matrix(data, row.names=rownames(data), colnames=colnames(data))
    return(.matrix.melt(data, 
                        data.frame(units="#", method="liveTrap"),
                        data.frame(id=rownames(data), year=NA, name=rownames(data), lat=NA, long=NA, address="Sabah, Malaysian Borneo", area=NA),
                        data.frame(species=colnames(data), taxonomy="Mammalia")))
}

.kaspari.2016 <- function(...) {
    addr  <- "https://pasta.lternet.edu/package/data/eml/msb-tempbiodev/1111170/1/cfd3a55deef52e3a93469057053f5404" 
    addr <- sub("^https", "http", addr)
    data <-read.csv(addr, header=F, skip=1, sep=",", 
                    col.names=c("location", "distance", "direction", 
                                "plotcode", "taxon", "abundance"), 
                                check.names=TRUE)
    return(.df.melt(data$taxon, data$plotcode, data$abundance,
                    data.frame(units="#"),
                    data.frame(id=unique(data$plotcode), year="2016", name=unique(data$plotcode), lat=NA, long=NA, address=NA, area=NA),
                    data.frame(species=unique(data$taxon), taxonomy="Arthropoda")))
}

.phillips.2003 <- function(...) {
    temp <- tempfile()
    download.file("https://www.forestplots.net/upload/data-packages/phillips-et-al-2015/PeruTransectData.zip", temp)
    data <- read.csv(unzip(temp, files="PeruTransectData/DataPeruTransects/IndividualData.csv"))
    temp <- tempfile()
    download.file("https://www.forestplots.net/upload/data-packages/phillips-et-al-2015/PeruTransectData.zip", temp)
    plotData <- read.csv(unzip(temp, files="PeruTransectData/DataPeruTransects/PlotInformationforRPackage.csv"))
    # The data is a census of all trees in a 0.1-ha plot that have a diameter at breast height > 10cm
    # I turned this into an abundance matrix given that they have records for each of the trees of a particular size.
    data$DBH1 <- 1
    data$year <- substr(data$Census.Date, 1,4)
    data$plot.year <- with(data, paste(Plot.Code, year, sep="_"))
    tData <- with(data, tapply(DBH1, list(plot.year, Species), sum, na.rm=TRUE))
    tData[is.na(tData)] <- 0
    temp <- strsplit(rownames(tData), "_")
    year <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,2]
    name <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,1]
    lat <- plotData$LatitudeDecimal[match(plotData$PlotCode, name)]
    long <- plotData$LongitudeDecimal[match(plotData$PlotCode, name)]
    return(.matrix.melt(tData, 
                        data.frame(units="#"),
                        data.frame(id=rownames(tData), year, name, lat, long, address="Eastern Madre de Dios, Peru, South America", area="0.1-ha"),
                        data.frame(species=colnames(tData), taxonomy="Plantae")))
} 

.biotime.2018 <- function(...) {
    data <- read.csv(url("https://zenodo.org/record/1095628/files/BioTIMEQuery_06_12_2017.csv"))
    metadata <- read.csv(url("https://zenodo.org/record/1095628/files/BioTIMEMetadata_06_12_2017.csv"))
    countStudies <- metadata$STUDY_ID[which(metadata$ABUNDANCE_TYPE == "Count")]
    countData <- data[which(data$STUDY_ID %in% countStudies),]
    countData$name <- with(countData, paste("Biotime", STUDY_ID, PLOT, sep="_"))
    countData$plot.year <- with(countData, paste("Biotime", STUDY_ID, PLOT, YEAR, sep="_"))
    countData$taxa <- metadata$TAXA[match(countData$STUDY_ID, metadata$STUDY_ID)]
    countData <- .df.melt(countData$GENUS_SPECIES,
                          countData$plot.year,
                          countData$sum.allrawdata.ABUNDANCE,
                          data.frame(units="#"),
                          data.frame(id=unique(countData$plot.year), year=countData$YEAR[!duplicated(countData$plot.year)], 
                                     name=countData$name[!duplicated(countData$plot.year)], lat=countData$LATITUDE[!duplicated(countData$plot.year)], 
                                     long=countData$LONGITUDE[!duplicated(countData$plot.year)], address="NA", area="NA"),
                          data.frame(species=unique(countData$GENUS_SPECIES), taxonomy=countData$taxa[!duplicated(countData$GENUS_SPECIES)]))
    paStudies <- metadata$STUDY_ID[which(metadata$ABUNDANCE_TYPE == "Presence/Absence")]
    paData <- data[which(data$STUDY_ID %in% paStudies),]
    paData$name <- with(paData, paste("Biotime", STUDY_ID, PLOT, sep="_"))
    paData$plot.year <- with(paData, paste("Biotime", STUDY_ID, PLOT, YEAR, sep="_"))
    paData$taxa <- metadata$TAXA[match(paData$STUDY_ID, metadata$STUDY_ID)]
    paData <- .df.melt(paData$GENUS_SPECIES,
                       paData$plot.year,
                       paData$sum.allrawdata.ABUNDANCE,
                       data.frame(units="p/a"),
                       data.frame(id=unique(paData$plot.year), year=paData$YEAR[!duplicated(paData$plot.year)], 
                                  name=paData$name[!duplicated(paData$plot.year)], lat=paData$LATITUDE[!duplicated(paData$plot.year)], 
                                  long=paData$LONGITUDE[!duplicated(paData$plot.year)], address="NA", area="NA"),
                       data.frame(species=unique(paData$GENUS_SPECIES), taxonomy=paData$taxa[!duplicated(paData$GENUS_SPECIES)]))
    return(mapply(rbind, countData, paData))
}

.thibault.2011 <- function(...){
    abundance.data <- read.csv(suppdata("E092-201", "MCDB_communities.csv", from = "esa_data_archives"), as.is=TRUE)
    site.data <- read.csv(suppdata("E092-201", "MCDB_sites.csv", from = "esa_data_archives"), as.is=TRUE)
    species <- read.csv(suppdata("E092-201", "MCDB_species.csv", from="esa_data_archives"), as.is=TRUE)
    abundance.data$species <- paste(species$Genus, species$Species)[match(abundance.data$Species_ID, species$Species_ID)]
    abundance.data$species <- gsub("  ", " ", abundance.data$species)
    abundance.data$Abundance <- as.numeric(abundance.data$Abundance)
    abundance.data <- na.omit(abundance.data)
    plot.year <- with(abundance.data, paste(Site_ID,Initial_year, sep="_"))
    
    site.metadata <- abundance.data[,c("Site_ID","Initial_year")]
    site.metadata$plot.year <- with(site.metadata, paste(Site_ID,Initial_year, sep="_"))
    site.metadata <- site.metadata[!duplicated(site.metadata$plot.year),]
    site.metadata$lat <- site.data$Latitude[match(site.metadata$Site_ID,site.data$Site_ID)]
    site.metadata$long <- site.data$Longitude[match(site.metadata$Site_ID,site.data$Site_ID)]
    names(site.metadata) <- c("name","year","id","lat","long")
    site.metadata <- site.metadata[,c("id","year","name","lat","long")]
    site.metadata$address <- NA
    site.metadata$area <- "sherman.trap"

    return (.df.melt(abundance.data$species, 
                     plot.year,
                     abundance.data$Abundance, 
                     data.frame(units="#"), 
                     site.metadata, 
                     data.frame(species=unique(abundance.data$species),taxonomy=NA)))
}

.branstetter.2018 <- function(...) {
    data <- read.csv("TableA3.csv")
    metadata <- read.csv("TableA2.csv")
    rownames(data) <- data[,1]
    data[,1] <- NULL
    colnames(data) <- gsub(".", "-", colnames(data), fixed=TRUE)
    data <- t(data)
    rownames(data) <- paste(rownames(data), year, sep="_")
    metadata$year <- format(as.Date(metadata$datecollected, format="%d-%b-%Y"),"%Y")
    year <- metadata$year[!duplicated(metadata$site)]
    name <- unique(metadata$site)
    lat <- metadata$latitude[!duplicated(metadata$site)]
    long <- metadata$longitude[!duplicated(metadata$site)]
    return(.matrix.melt(data, 
                        data.frame(units="#"),
                        data.frame(id=rownames(data), year, name, lat, long, 
                                    address=NA, area=NA),
                        data.frame(species=colnames(data), taxonomy="Hymenoptera")))
}

.collins.2018 <- function(...) {
    # The species in this dataset are not named; Generic identifiers are given (e.g. 'sp1')
        # Species codes were added due to people not wanting scott to publish their data.
    data <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/15/5/f69c8fe563067164191d61b6e33eff03",  as.is=TRUE)
    names(data) <- tolower(names(data))
    metadata <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/15/5/8284876afe3a1cb0a919d37e1164357f", as.is=TRUE)
    names(metadata) <- tolower(names(metadata))
    data$site_year <- with(data, paste(data$sitesubplot, experiment_year, sep="_"))
    data$latitude <- metadata$lat[match(data$site_project_comm, metadata$site_project_comm)]
    data$longitude <- metadata$long[match(data$site_project_comm, metadata$site_project_comm)]
    data$address <- metadata$location[match(data$site_project_comm, metadata$site_project_comm)]
    data$area <- metadata$plot_size[match(data$site_project_comm, metadata$site_project_comm)]
    return(.df.melt(data$species, 
                    data$site_year, 
                    data$relcover,
                    data.frame(units="%"),
                    data.frame(id=unique(data$site_year), 
                               year=data$experiment_year[!duplicated(data$site_year)], 
                               name=data$sitesubplot[!duplicated(data$site_year)], 
                               lat=data$latitude[!duplicated(data$site_year)], 
                               long=data$longitude[!duplicated(data$site_year)], 
                               address=data$address[!duplicated(data$site_year)], 
                               area=data$area[!duplicated(data$site_year)]),
                    data.frame(species=unique(data$species), taxonomy="Plantae")))
}

.franklin.2018 <- function(...) {
    data <- read.xls("CopyofWESTCOSPPCOVER.xlsx", as.is=TRUE)
    ground_data <- read.xls("WEST CO GROUND COVER.xlsx")
    data$R4_SPP <- NULL
    colnames(data) <- colnames(ground_data)
    combined.data <- rbind(data, ground_data)
    combined.data$year <- NA
    for(i in seq_len(nrow(combined.data))){
        t <- as.numeric(regexpr("[0-9]{4}", combined.data$SITE_ID[i]))[1]
        combined.data$year[i] <- substr(combined.data$SITE_ID[i], t, t+4)
    }
    metadata <- read.xls("WEST CO SAGEBRUSH PLOTS.xlsx", as.is=TRUE)
    combined.data$SITE_ID <- gsub(" ", "", combined.data$SITE_ID)
    combined.data$lat <- metadata$LATITUDE[match(combined.data$SITE_ID, metadata$SITE_ID)]
    combined.data$long <- metadata$LONGITUDE[match(combined.data$SITE_ID, metadata$SITE_ID)]
    combined.data$elevation.ft <- metadata$Elev..ft.[match(combined.data$SITE_ID, metadata$SITE_ID)]
    combined.data$aspect <- metadata$Aspect[match(combined.data$SITE_ID, metadata$SITE_ID)]
    combined.data$pct.slope <- metadata$Pct_Slope[match(combined.data$SITE_ID, metadata$SITE_ID)]
    combined.data$project <- metadata$PROJECT[match(combined.data$SITE_ID, metadata$SITE_ID)]
    combined.data$COVER_PERCENT <- as.numeric(combined.data$COVER_PERCENT)
    combined.data$COVER_PERCENT[is.na(combined.data$COVER_PERCENT)] <- 0
    return(.df.melt(combined.data$NAME,
                    combined.data$SITE_ID,
                    combined.data$COVER_PERCENT,
                    data.frame(units="area"),
                    data.frame(id=unique(combined.data$SITE_ID), 
                               year=combined.data$year[!duplicated(combined.data$SITE_ID)], 
                               name=unique(combined.data$SITE_ID), 
                               lat=combined.data$lat[!duplicated(combined.data$SITE_ID)], 
                               long=combined.data$long[!duplicated(combined.data$SITE_ID)], 
                               address=NA, 
                               area="0.1ha",
                               elevation.ft=combined.data$elevation.ft[!duplicated(combined.data$SITE_ID)],
                               aspect=combined.data$aspect[!duplicated(combined.data$SITE_ID)], 
                               pct.slope=combined.data$pct.slope[!duplicated(combined.data$SITE_ID)],
                               project=combined.data$project[!duplicated(combined.data$SITE_ID)]),
                    data.frame(species=unique(combined.data$NAME), 
                               taxonomy=NA, 
                               other="Plant study; Percent cover of species and ground")))
}

if(FALSE){
.lightfoot.2016 <- function(...) {
    data <- read.table("http://sev.lternet.edu/sites/default/files/data/sev-106/sev106_hopperdynamics_20150826.txt", header=T, sep=",")
    data$month.year <- format(as.Date(data$DATE, format="%m/%d/%Y"),"%m/%Y")
    spec_codes <- c("ACPI","AGDE","AMCO","ARCO","ARPS","AUEL","AUFE","BOAR",
                 "BRMA","CIPA","COCR","COOC","COTE","DABI","ERSI","HATR",
                 "HERU","HEVI","HICA","LAAZ","LEWH","MEAR","MEAZ","MEBO",
                 "MEGL","MELA","MEOC","METE","OPOB","PAPA","PHQU","PHRO",
                 "PSDE","PSTE","SCNI","SYMO","TRCA","TRFO","TRKI","TRPA",
                 "TRPI","XACO","XAMO")
    species <- c("Acantherus piperatus","Ageneotettix deorum",
                "Amphitornus coloradus","Arphia conspersa",
                "Arphia pseudonietana","Aulocara elliotti",
                "Aulocara femoratum","Bootettix argentatus",
                "Brachystola magna","Cibolacris parviceps",
                "Cordillacris crenulata","Cordillacris occipitalis",
                "Conozoa texana","Dactylotum bicolor",
                "Eritettix simplex","Hadtrotettix trifasciatus",
                "Heliaula rufa","Hesperotettix viridis",
                "Hippopedon capito","Lactista azteca","Leprus wheeleri",
                "Melanoplus aridus","Melanoplus arizonae",
                "Melanoplus bowditchi","Melanoplus gladstoni",
                "Melanoplus lakinus","Melanoplus occidentalis",
                "Mermeria texana","Opeia obscura","Paropomala pallida",
                "Phlibostroma quadrimaculatum","Phrynotettix robustus",
                "Psoloessa delicatula","Psoloessa texana",
                "Schistocerca nitens","Syrbula montezuma",
                "Trimerotropis californicus","Tropidolophus formosus",
                "Trachyrhachis kiowa","Trimerotropis pallidipennis",
                "Trimerotropis pistrinaria","Xanthippus corallipes",
                "Xanthippus montanus")
    metadata <- data.frame(spec_codes, species)
    data$SPECIES <- metadata$species[match(data$SPECIES, metadata$spec_codes)]
    data <- with(data, tapply(CNT, list(site_year, SPECIES), sum, na.rm=TRUE))
    data$site_year <- with(data, paste(SITE, year, sep="_"))
    temp <- strsplit(rownames(data), "_")
    year <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,2]
    year <- format(as.Date(data$DATE, format="%m/%Y"),"%Y")
    name <- matrix(unlist(temp), ncol=2, byrow=TRUE)[,1]
    burned <- 
    return(.df.melt(data$species, data$SITE, data$CNT,
                    data.frame(units="#"),
                    data.frame(id=unique(data$plot_year), 
                               year, 
                               name, 
                               lat=NA, 
                               long=NA, 
                               address="Sevilleta National Wildlife Refuge, New Mexico", 
                               area=NA,
                               burned),
                    data.frame(species=unique(data$species), taxonomy="Orthoptera")))
}

.cobb.2016 <- function(...) {
    data <- read.xls("COMPLETE Dataset as of 4_recovery2.xlsx")
    data$name <- with(data, paste("study.area", Study.Area, "site", Site, sep="_"))
    data$month.year <- with(data, paste(Month, Year, sep="-"))
    data$site.year <- with(data, paste(name, month.year, sep="_"))
    metadata <- data[,c(1:11, 148, 149, 150)]
    metadata$Longitude <- gsub("\342\200\223", "-", metadata$Longitude)
    data[,1] <- data$site.year
    data[,c(2:11, 148, 149, 150)] <- NULL
    data <- aggregate(.~Sample.., data=data, FUN=sum)
    rownames(data) <- data[,1]
    data[,1] <- NULL
    rownames <- rownames(data)
    data <- apply(data, 2, as.numeric)
    rownames(data) <- rownames
    name <- metadata$name[match(rownames(data), metadata$site.year)]
    year <- metadata$Year[match(rownames(data), metadata$site.year)]
    lat <- metadata$Latitude[match(rownames(data), metadata$site.year)]
    long <- metadata$Longitude[match(rownames(data), metadata$site.year)]
    veg.type <- metadata$Veg.type[match(rownames(data), metadata$site.year)]
    burned <- metadata$Burn[match(rownames(data), metadata$site.year)]
    monsoon <- metadata$Monsoon[match(rownames(data), metadata$site.year)]
    return(.matrix.melt(data,
                        data.frame(units="STD.#"),
                        data.frame(id=rownames(data), 
                                   year, 
                                   name, 
                                   lat, 
                                   long, 
                                   address=NA, 
                                   area=NA,
                                   veg.type,
                                   burned,
                                   monsoon),
                        data.frame(species=colnames(data), taxonomy="Arthropoda")))
}

.mooney.2018 <- function(...) {
    #will need to loop through and do this for each year (sheet) in the dataset.
    data <- read.xls("Insect Abundance Population Summaries.xlsx", sheet="#")
    data <- data[which(data$Response == "Total"),]
    #remove all rows that contain only NA values
    data <- data[ ,!apply(data, 2, function(x) all(is.na(x)))]
    data <- melt(data, id=c("Population", "Response"))
}

.dyer.2017 <- function(...) {
    # Location is not always GPS coordinates in this dataset. Some are descriptions or titles of the locations.
    # Some of the values in data are blank. These do not mean that the value is zero but that the data is not complete. (Lee has the code to complete it).
    data <- read.xls("SWRS_plots_updated_nov_3_2017.xlsx")
    data<-data[,1:24]
    data$year <- format(as.Date(data$Date..D.M.Y., format="%Y-%m-%d"),"%Y")
    data$plot.year <- with(data, paste(X.number, year, sep="."))
    return(.df.melt(data$plant.sp,
                    data$plot.year,
                    data$Leaf.area..cm.2., 
                    data.frame(units="area"), 
                    data.frame(id=unique(data$plot.year),
                               year=data$year[!duplicated(data$plot.year)], 
                               name=data$X.number[!duplicated(data$plot.year)], 
                               lat=NA, 
                               long=NA, 
                               address=NA, 
                               area="cm2"),
                    data.frame(species=unique(data$plant.sp), taxonomy="Plantae")))
}

.albouy.2015 <- function(...) {
    temp <- tempfile()
    download.file("http://esapubs.org/archive/ecol/E096/203/Presence_absence_data.zip", temp)
    data <- read.csv(unz(temp, "Observed_grid_1980.csv"))
    unlink(temp)

}

.helmus.2013 <- function(...){
    library(pez) # This isn't how we declare packages in 'real'
                 # packages for the time being this is sufficient
    data(laja)
    return(.matrix.melt(invert.sites))
}

.chamailleJammes.2016 <- function(...){
    data <- read.csv(suppdata("10.1371/journal.pone.0153639", 1), stringsAsFactors=FALSE)
    year <- (1992:2005)[-6] # Study excluded the year of 1997
    data <- aggregate(. ~ WATERHOLE, data = data, FUN=sum)
    species <- colnames(data)
    data <- reshape(data, varying = list(names(data)[2:ncol(data)]), v.names = "Count", 
                      idvar = "WATERHOLE", times = c("ELEPHANT", "GIRAFFE", "IMPALA","KUDU",
                      "ROAN", "SABLE", "WILDEBEEST", "ZEBRA"), timevar = "species", direction = "long")
    rownames(data) <- NULL
    id <- unique(data$WATERHOLE)
    year <- rep(year, each=length(id))
    temp <- paste(data$WATERHOLE, year, sep="_")
    return(.df.melt(data$species, 
                    data$WATERHOLE, 
                    data$Count, 
                    data.frame(units="#"),
                    data.frame(id=, lat="18", long="26", address="Hwange National Park, Zimbabwe, Africa", area=NA),
                    data.frame(species=unique(data$species, taxonomy="Mammalia"))))
}

.coblentz.2015 <- function(...){
    # This won't work on Windows OS. I might be wrong but I think that it has
    # something to do with the spaces in the file name.
    data <- read.xls(suppdata("10.5061/dryad.j2c13", "Invert Community Data 2012 RAW.xlsx"), stringsAsFactors=FALSE)
    colnames(data) <- with(data, paste(colnames(data), data[3,], sep="_"))
    data <- data[-1:-3,]
    species <- data[,1]
    data  <- data[,-1]
    data <- sapply(data, as.numeric)
    rownames(data) <- species
    return(.matrix.melt(data))
}

.stapp.2013 <- function(...) {
    infile  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sgs/137/17/c4cc7f18abefa0d98a356c6936710ab1" 
    infile <- sub("^https","http",infile) 
    data <-read.csv(infile, header=F, skip=1, sep="\t", 
                    col.names=c("Sample", "Session", "Veg", "Day", "Month", 
                                "Year", "Web", "Night", "Trap", "Capt", "Tag", 
                                "Spp", "Age", "Sex", "Reprod", "WT", "notes", 
                                "Stapp's.comments"), check.names=TRUE)
}

# - this one is a dump, but seeing as how it works(ish) I'm just popping it up...
}
if(FALSE){
.fia.2018 <- function(...){
    .get.fia <- function(state, var, select){
        t.zip <- tempfile()
        download.file(paste0("https://apps.fs.usda.gov/fia/datamart/CSV/",state,"_",var,".zip"), t.zip)
        unzip(t.zip)
        data <- fread(paste0(state,"_",var,".csv"), select=select)
        unlink(paste0(state,"_",var,".csv"))
        return(data)
    }
    
    states <- c("PR","VI")#c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY", "AS","GU","MP","PW","PR","VI")
    data <- vector("list", length(states))
    
    for(i in seq_along(states)){
        #Download/read in data
        tree <- .get.fia(states[i], "TREE", c("CN","PLT_CN","PLOT","SPCD","DIA","INVYR"))
        cond <- .get.fia(states[i], "COND", c("PLT_CN","PLOT","STDAGE","FORTYPCD","CONDID"))
        plot <- .get.fia(states[i], "PLOT", c("PLOT","LAT","LON","ELEV", "CN"))
        
        #Subset everything, remove sites with multiple/ambiguous codings, merge
        tree <- tree[tree$DIA > 1.96,]
        cond <- cond[cond$PLT_CN %in% names(Filter(function(x) x==1, table(cond$PLT_CN))),]
        data[[i]] <- merge(tree, merge(cond, plot, by.x="PLT_CN", by.y="CN"), by.x="PLT_CN", by.y="PLT_CN")
        data[[i]]$state <- states[i]
    }
    data <- rbindlist(data)
    t <- setNames(seq_along(unique(data$PLT_CN)), unique(data$PLT_CN))
    data$state.ref <- paste0(data$state, ".", t[data$PLT_CN])
}
}

if(FALSE){

    .lamotte.1936 <- function(...){
    species <- read.xls("http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/49Camp%20NV.xls", as.is=TRUE, fileEncoding="latin1", skip=4)[,2]
    .df.melt(species, 
                     rep("49_Camp_NV_-15750000", length(species)),
                     rep(1,length(species)), 
                     data.frame(units="p/a", treatment="paleoecology"), 
                     data.frame(id="49_Camp_NV_-15750000", year=-15750000, name="49_Camp_NV",lat="40.66",long="-199.66", address=NA, area="paleo"), 
                     data.frame(species=species,taxonomy=NA))

    }
}

if(FALSE)
.kuhlman.2017 <- function(...){
    #Missoula County, Montana, USA
    #count them up, all fine
    #point out it's a species list, essentially
}



.sandau.2017 <- function(...){
  tmp.file <- tempfile()
  download.file("https://www.datadryad.org/bitstream/handle/10255/dryad.129944/BB_all_4_SimilMatrices_Dryad.xlsx?sequence=1", tmp.file)
  data <- read.xls(tmp.file, sheet=2)
  lookup <- read.xls(suppdata("10.5061/dryad.44bm6", "BB_all_4_SimilMatrices_Dryad.xlsx"), sheet=1, skip=5, header=FALSE, as.is=TRUE)[-1:-8,]
  lookup[,2] <- sanitize_text(lookup[,2])  
  lookup[,2] <- sapply(strsplit(lookup[,2], " "), function(x) paste(x[1:2],collapse="_"))
  lookup <- setNames(lookup[,2], lookup[,1])
  names(data)[names(data) %in% names(lookup)] <- lookup[names(data)[names(data) %in% names(lookup)]]
  site_year <- with(data, paste(data$PlotID, Year, sep="_"))
  data <- cbind(site_year, data)
  comm.mat <-data[-1:-11]
  #This sets the row names to the unique plot_year identifier
  rownames(comm.mat) <-data[,1]
  site.metadata <- data[!duplicated(data$site_year),]
  return(.matrix.melt(comm.mat,
                      data.frame(units="%", treatment=""),
                      data.frame(id=site.metadata$site_year, name=site.metadata$PlotID, year=site.metadata$Year, lat=NA, long=NA, address="Grandcour", treatment=site.metadata$Treat, area="20 × 20 m"),
                      data.frame(species=unique(lookup, taxonomy="Plantae"))))
}



.kormann.2018 <- function(...){
  data <- read.table(.unzip("Data/PointCounts.txt",suppdata("10.5061/dryad.0t9d3/1", "Data.zip")), header=TRUE)
  site_pc <- with(data, paste(data$Site, PC, sep="_"))
  rownames(data) <- site_pc
  comm.mat <- as.matrix(data[-1:-13])
  site.metadata <- data[,1:13]
  species.meta <- data.frame(species=colnames(comm.mat), taxonomy="Aves")
  return(.matrix.melt(comm.mat,
                      data.frame(units="#"),
                      data.frame(id=site_pc, name=site.metadata$Site, year=2011, lat=site.metadata$X, long=site.metadata$Y, address="Southern Costa Rica, around the Las Cruces Biological Station ", area=site.metadata$Area),
                      species.meta))
}

