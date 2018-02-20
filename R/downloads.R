####################
# ADD DOIs ##########
#####################

#' @importFrom suppdata suppdata

# Plotted land and area covered by various plant species. Repeat species in the same plot were combined into
# - one row and the area covered were summed together
# return a long format table of data
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

# Species counts of different quads on various years
.anderson.2011 <- function(...){
    data <- read.csv(suppdata("10.6084/m9.figshare.3551799.v1", "annuals_counts_v2.csv"), as.is=TRUE)
    data$plot_year <- with(data, paste(quad, year, sep = "_"))
    year <- as.numeric(paste0("19",as.character(data$year)))[!duplicated(data$plot_year)]
    site.id <- unique(data$plot_year)
    name <- data$quad[!duplicated(data$plot_year)]
    data <- data[order(data$species), c(3, 5, 4)]
<<<<<<< HEAD
    return (.df.melt(data$species, 
                     data$plot_year, 
                     data$count, 
                     data.frame(units="#", treatment="grazing"), 
                     data.frame(id=site.id, year,name,lat=NA,long=NA, address="northern mixed prairie in Miles City, Montana, USA", area="1m2"), 
                     data.frame(species=unique(data$species),taxonomy=NA)))
=======
    return (.df.melt(data$species, data$plot_year, data$count, data.frame(units="#",treatment="grazing"), data.frame(id=site.id,year,name,lat=NA,long=NA,address="northern mixed prairie in Miles City, Montana, USA",area="1m2"), data.frame(species=unique(data$species),taxonomy=NA)))
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


if(FALSE){
    # Do work
    site.metadata <- nneo_sites(); sites <- site.metadata$siteCode
    output <- vector("list", length(sites))
    for(i in seq_along(sites)){
        site.data <- nneo_site(sites[i])$dataProducts
        if(length(site.data)>0){
            site.data <- site.data[site.data$dataProductCode %in% "DP1.10072.001",]
            output[[i]] <- vector("list", nrow(site.data))
            for(j in seq_len(nrow(site.data)))
                output[[i]][[j]] <- do.call(rbind, lapply(unlist(site.data$availableMonths[j]), product.codes[site.data$dataProductCode[j]][[1]], site=sites[i]))
        }
}

    


.neon.mammals <- function(month, site){
    possible <- nneo_data("DP1.10072.001", site, month, "simple")$data$files
    url <- grep(paste0("mam_pertrapnight\\.",month,"\\.basic"), possible$url, value=TRUE)
    data <- read.csv(url, as.is=TRUE)[,c("scientificName", "weight")]
    data <- data[data$scientificName != "",]
    data$weight[is.na(data$weight)] <- -1
    data$month <- month; data$site <- site
    return(data)
}
.neon.beetles <- function(month, site){
    site <- "ABBY"; month <- "2016-09"
    possible <- nneo_data("DP1.10022.001", site, month, "simple")$data$files
    url <- grep(paste0("expertTaxonomistIDProcessed\\.",month,"\\.basic"), possible$url, value=TRUE)
    data <- read.csv(url, as.is=TRUE)[,c("scientificName"), drop=FALSE]
    data$month <- month; data$site <- site
    return(data)
}
.neon.plants <- function(month, site){
    site <- "SJER"; month <- "2015-04"
    possible <- nneo_data("DP1.10098.001", site, month, "simple")$data$files
    url <- grep(paste0("apparentindividual\\.",month,"\\.basic"), possible$url, value=TRUE)
    data <- read.csv(url, as.is=TRUE)[,c("individualID","plotID", "plantStatus")]
    data <- data[grep("Live",data$plantStatus),]
    # Load lookup; correct for mistakes by sorting on date (see readme of NEON data)
    lookup <- read.csv(grep("mappingandtagging.basic", possible$url, value=TRUE, fixed=TRUE), as.is=TRUE)
    lookup <- lookup[order(lookup$date,decreasing=TRUE),]
    lookup <- lookup[!duplicated(lookup$individualID),]
    lookup <- lookup[,c("scientificName","individualID")]
    data <- merge(data, lookup, "individualID")
    data$scientificName <- sapply(strsplit(data$scientificName, " "), function(x) paste(x[1:2], collapse="_"))
    data <- aggregate(.~plotID+scientificName, data=data, FUN=length)
    names(data)[3] <- "abundance"
    return(data[,1:3])
}

site.metadata <- nneo_sites()
sites <- site.metadata$siteCode
product.codes <- setNames(c(.neon.beetles,.neon.mammals,.neon.plants), c("DP1.10022.001","DP1.10072.001","DP1.10098.001"))
output <- vector("list", length(sites))
for(i in seq_along(sites)){
    site.data <- nneo_site(sites[i])$dataProducts
    if(length(site.data)>0){
        site.data <- site.data[site.data$dataProductCode %in% names(product.codes),]
        output[[i]] <- vector("list", nrow(site.data))
        for(j in seq_len(nrow(site.data)))
            output[[i]][[j]] <- do.call(rbind, lapply(unlist(site.data$availableMonths[j]), product.codes[site.data$dataProductCode[j]][[1]], site=sites[i]))
    }
}

"DP1.10098.001"
"DP1.10098.001"

# NEON Plants
#' @importFrom nneo nneo_data
.neon.2018a <- function(...){

}
}

if(FALSE){
.baldridge.2013 <- function(...){
    #Done. Site IDs in original data is nonconsequential. 
    abundance_data <- read.csv(suppdata("10.6084/m9.figshare.769251.v1", "Species_abundances.csv"))
    site_data <- read.csv(suppdata("10.6084/m9.figshare.769251.v1", "Sites_table_abundances.csv"))
    new.site_data <- with(site_data, data.frame(Site_ID = Site_ID, Site_Name = paste(Site_Name, Collection_Year, sep = "_")))
    new.site_data <- na.omit(new.site_data)
    data <- with(abundance_data, data.frame(species = paste(Family,Genus, Species, sep = "_"), plot = Site_ID, count = Abundance))
    data$plot<-new.site_data$Site_Name[match(data$plot, new.site_data$Site_ID)]
    new.data <- with(data, tapply(count, list(plot, species), sum))
    new.data[is.na(new.data)] <- 0
    new.data <- new.data[-1,]
    return(.matrix.melt(new.data))
>>>>>>> 0731629d28c5c9fb9fb4a35c5add894f1706780b
}

.chu.2013 <- function(...){
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
                        data.frame(units="area"), 
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
if(FALSE){
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
    spec_codes <- with(spec_codes, setNames(paste(genus, species, sep = " "), spcode))
    data$species <- spec_codes[data$species]
    data$species[is.na(data$species)] <- "spp."
    site.id <- unique(plot.year)
    plots <- site.id[seq(1,length(site.id), 2)]
    years <- site.id[seq(2,length(site.id), 2)]
    return(.df.melt(data$species, data$plot_year, data$cover, 
                    data.frame(units="area"), 
                    data.frame(id=site.id, year=year, name=plots, lat=NA, long=NA, address="Tallgrass Prairie Preserve in Osage County, Oklahoma, USA", area=NA),
                    data.frame(species=unique(data$species), taxonomy="plantae")))
}

.baldridge.2013 <- function(...){
    #Done. Site IDs in original data is nonconsequential. 
    abundance_data <- read.csv(suppdata("10.6084/m9.figshare.769251.v1", "Species_abundances.csv"))
    site_data <- read.csv(suppdata("10.6084/m9.figshare.769251.v1", "Sites_table_abundances.csv"))
    new.site_data <- with(site_data, data.frame(Site_ID = Site_ID, Site_Name = paste(Site_Name, Collection_Year, sep = "_")))
    new.site_data <- na.omit(new.site_data)
    data <- with(abundance_data, data.frame(species = paste(Family,Genus, Species, sep = "_"), plot = Site_ID, count = Abundance))
    data$plot<-new.site_data$Site_Name[match(data$plot, new.site_data$Site_ID)]
    new.data <- with(data, tapply(count, list(plot, species), sum))
    new.data[is.na(new.data)] <- 0
    new.data <- new.data[-1,]
     return(.matrix.melt(new.data, data.frame(units="#"),
                         data.frame(id=rownames(new.data),)))
}

.helmus.2013 <- function(...){
    library(pez) # This isn't how we declare packages in 'real'
                 # packages for the time being this is sufficient
    data(laja)
    return(.matrix.melt(invert.sites))
}

.thibault.2011 <- function(...){
    #This one is not done
    abundance.data <- read.csv(suppdata("E092-201", "MCDB_communities.csv", from = "esa_archives"))
    site.data <- read.csv(suppdata("E092-201", "MCDB_sites.csv", from = "esa_archives"))
    return(transformed.data)    
}

.chazot.2014 <- function(...){
    #Abundance data for butterfly species from seven different sites.
    data <- read.xls(suppdata("10.5061/dryad.1534j", "Abundance_dataset.xlsx"), skip = 1)
    data <- data[-c(163,164,165),-c(2,3)]
    rownames(data) <- data[,1]
    data[,1] <- NULL
    return(.matrix.melt(t(data)))
}

.chamailleJammes.2016 <- function(...){
    data <- read.csv(suppdata("10.1371/journal.pone.0153639", 1), stringsAsFactors=FALSE)
    years <- (1992:2005)[-6] # Study excluded the year of 1997
    for (year in seq_along(years))
        site_year <- sapply(data$WATERHOLE, paste, years[year], sep="_")
    data <- aggregate(. ~ WATERHOLE, data = data, FUN=sum)
    species <- colnames(data)
    data <- reshape(data, varying = list(names(data)[2:ncol(data)]), v.names = "Count", 
                      idvar = "WATERHOLE", times = c("ELEPHANT", "GIRAFFE", "IMPALA","KUDU",
                      "ROAN", "SABLE", "WILDEBEEST", "ZEBRA"), timevar = "species", direction = "long")
    return(.df.melt(data$species, data$WATERHOLE, data$Count, 
                    data.frame(units="#"),
                    data.frame(id=, lat=, long=, address="Hwange National Park, Zimbabwe"),
                    data.frame(species=unique(data$species, taxonomy="Mammalia"))))
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
    return(.matrix.melt(new.data))
}

.rodriguezBuritica.2013 <- function(...){
    data <- read.csv(suppdata("E094-083","SMCover.csv",from = "esa_archives"))
    species.data <- read.csv(suppdata("E094-083","Species.csv",from = "esa_archives"))
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
    data <- read.csv(suppdata("E093-132", "allrecords_point_features.csv", from = "esa_archives"))
    data$plotyear <- with(data, paste(quad, year, sep = "_"))
    comm <- with(data, tapply(Canopy_cov, list(plotyear, Species), sum, na.rm=TRUE))
    return(.matrix.melt(comm))
}

.stevens.2011 <- function(...){
    data <- read.csv(suppdata("E092-128", "speciesdata.csv", from = "esa_archives"))
    data[is.na(data)] <- 0
    data$Site.number <- with(data, paste(Site.number, Year, sep = "_"))
    data <- aggregate(. ~ Site.number, data = data, FUN=sum)
    rownames(data) <- data[,1]
    data <- data[,-c(1:4)]
    data[data > 0] <- 1
    return(.matrix.melt(data))
}

.raymond.2011 <- function(...){
    data <- read.csv(suppdata("E092-097", "diet.csv", from = "esa_archives"))
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
    species.data <- read.csv(suppdata("E094-149", "table3.csv", from = "esa_archives"))
    site.data <- read.csv(suppdata("E094-149", "table1.csv", from = "esa_archives"))
    site.data$Date <- format(as.Date(site.data$Date, format="%d-%m-%Y"),"%Y")
    site.data$site.year <- with(site.data, paste(SampleID, Date, sep = "_"))
    species.data$X <- site.data$site.year[match(species.data$X, site.data$SampleID)]
    rownames(species.data) <- species.data$X
    species.data[species.data > 0] <- 1
    return(.matrix.melt(species.data))
}

.laverick.2017 <- function(...){
    data <- read.csv(suppdata("10.1371/journal.pone.0183075",7))
    rownames(data) <- data$X
    data <- data[,-c(44:46)]
    return(.matrix.melt(data))
}

.jian.2014 <- function(...){
    data <- read.csv(suppdata("10.1371/journal.pone.0114301", 5))
    data$site <- with(data, paste(site, date, sep = "_"))
    transformedData <- aggregate(. ~ site, data = data[,-1], FUN=sum)
    rownames(transformedData) <- transformedData$site
    transformedData <- transformedData[,-1]
    return(.matrix.melt(transformedData))
}

.ogutu.2017 <- function(...){
    data <- read.xls(suppdata("10.1371/journal.pone.0169730", 3))
    data <- data[,1:3]
    data$site <- "Nakuru.Wildlife.Conservancy"
    data$site <- with(data, paste(site, Date, sep = "_"))
    transformedData <- with(data, tapply(Count, list(site, Species), sum, na.rm = TRUE))
    return(.matrix.melt(transformedData))
}

.gallmetzer.2017 <- function(...){
    #No dates given for collections
    data <- read.xls(suppdata("10.1371/journal.pone.0180820", 1))
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
}
