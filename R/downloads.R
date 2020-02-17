#####################
# ADD DOIs ##########
#####################
#' @importFrom suppdata suppdata
#' @importFrom utils data head read.csv read.delim read.table
#' @importFrom stats aggregate na.omit reshape
#' @importFrom gdata drop.levels read.xls
#' @importFrom readxl read_xlsx read_xls read_excel
#' @importFrom neonUtilities loadByProduct
#' @importFrom bit64 as.integer64
#' @importFrom data.table data.table rbindlist fread
#' @importFrom stringr str_extract
#' @importFrom picante matrix2sample
#' @importFrom reshape2 melt
#' @export
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

#' @export
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

#' @export
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

#' @export
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

#' @export
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

#' @export
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

#' @export
.boyle.2015a <- function(...) {
    data <- read.csv(suppdata("10.5061/dryad.65v10", "LaSelvaBirdTrendsDatatable.csv"), stringsAsFactors=FALSE)
    data <- melt(data, id.vars=1,11:12)
    data$site <- gsub("MeanCount", "LaSelvaBiologicalStation_CostaRica", data$variable)
    return(.df.melt(data$ScientificName, data$site, data$value,
                    data.frame(units="#"),
                    data.frame(id=unique(data$site), year=c("89-94", "06-11"), name=NA, lat="10.422", long="-84.015", address="LaSelvaBiologicalStation_CostaRica", area=NA),
                    data.frame(species=unique(data$ScientificName), taxonomy="Aves")))
}

#' @export
.boyle.2015b <- function(...){
    species <- read.csv(suppdata("10.5061/dryad.bf486", "BritishColumbiaHighElevationBirdDataset.csv"))
    species$SiteCombo <- paste0(species$MountainRange, "-", species$SiteName)
    comm <- with(species, tapply(NBirds, list(SiteCombo, CommonName), sum))
    comm[is.na(comm)] <- 0
    site.metadata <- species[!duplicated(species$SiteCombo),]
    site.metadata <- site.metadata[,c("Date", "Wind", "Temp", "Cloud", "AM.PM", "HaSurveyed", "SiteCombo")]
    return(.matrix.melt(comm,
                        data.frame(units="#", treatment=NA),
                        data.frame(id=rownames(comm), year=site.metadata$Date, name=site.metadata$SiteCombo, lat=NA, long=NA, address = "British Columbia", area=site.metadata$HaSurveyed),
                        data.frame(species=colnames(comm), taxonomy=NA)))
}

#' @export
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

#' @export
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

#' @export
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

#' @export
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

#' @export
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

#' @export
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

#' @export
.ellison.2017 <- function(...){
    data <- read.csv(file="https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-hfr.97.23&entityid=a840ed1f4c891cd7e6abe660aecb797a", header=TRUE)

    species <- data$species
    data$id <- rep(paste(data$plot,data$date))
    site.metadata <- data[!duplicated(data$id),]
    site.metadata <- with(site.metadata,
                          data.frame(id=id, year=date, name=plot, lat=NA,long=NA, address="North of West Point, New York, USA",area=NA)
                          )
    site <- rep(paste(data$plot,data$date), 3120)
    abundance <- as.vector(data$no.ants)
    abundance[is.na(abundance)] <- 0

    return(.df.melt(species, site, abundance,
                    study.metadata=data.frame(units="#"),
                    site.metadata,
                    species.metadata=data.frame(species=unique(species), taxonomy=NA)
                    ))
}

#' @export
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

#' @export
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

#' @export
.harrower.2017<-function(...){
    
    birddata<-read.csv("https://datadryad.org/bitstream/handle/10255/dryad.159186/bird_data.csv?sequence=1",as.is = TRUE)
    envdata<-read.csv("https://datadryad.org/bitstream/handle/10255/dryad.159187/envr_data.csv?sequence=1",as.is=TRUE)
    
    envdata$name<-paste(envdata$block,envdata$transect,sep="_")
    
    birddata$id<-paste(birddata$block,birddata$transect,birddata$year,sep="_")
    birddata$name<-paste(birddata$block,birddata$transect,sep="_")
    birddata$lat<-"50o39'59\" N" 
    birddata$long<-"120o19'09\" W" 
    birddata$address<- "Lac du Bois Provincial Park near Kamloops, British Columbia, Canada"
    birddata$area<-"20ha"
    birddata$binom<-paste(birddata$genus,birddata$species,sep=".")
    
    comm <- with(birddata, tapply(binom, list(binom, site), length))
    comm[is.na(comm)] <- 0
    comm<-t(comm)
    
    birdsub<-birddata[!duplicated(birddata$site),]
    envsub<-envdata[,c(3,8)]
    envtest<-merge(birdsub,envsub,by="name")
    envtest<-envtest[,-c(6:11,17)]
    
    return(.matrix.melt(comm,
                        data.frame(units="#"),
                        envtest,
                        data.frame(species=birddata$binom, taxonomy=NA)
                        )
           )
}

#' @export
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

#' @export
.heinen.2017 <- function(...){
    # Extinct and extant occurrences of frugivorous birds, mammals and reptiles on 
    # 74 tropical and subtropical oceanic islands within 20 archipelagos worldwide
    data <- read.table(suppdata("10.5061/dryad.s522m", "Data_Occurrences_IslandFrugivores.txt"), header=T, sep='\t')
    subdata <- data[,c(5,9,11)]
    colnames(subdata)[c(1,3)] <- c('Site','Status')
    subdata$Species <- gsub(' ','_',subdata$Species)
    subdata <- with(subdata, tapply(Status, list(Site, Species), sum))
    subdata[is.na(subdata)] <- 0
    plots_years <- unlist(strsplit(rownames(subdata), "_", fixed=T))
    plots <- plots_years[seq(1,length(plots_years), 2)]
    # .df.melt(species=subdata$Species, site.id=subdata$Site,value=subdata$Status)
    return(.matrix.melt(subdata, 
                        data.frame(units="#"), 
                        data.frame(id=rownames(subdata), year=NA, name=plots, lat=NA, long=NA, address="74 tropical and subtropical oceanic islands within 20 archipelagos worldwide", area=NA),
                        data.frame(species=colnames(subdata), taxonomy="Animalia")))
}

#' @export
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

#' @export
.hollibaugh.2017 <- function(...){
    data <- read.csv(file = "https://pasta.lternet.edu/package/data/eml/knb-lter-pal/114/2/3ab81d869107c4b3a7f0fb76fed55ed4", header = TRUE)
    names(data)[7:8] <- c("latitude","longitude")

    taxon <- rep(c("Eub","AOB","Archaea","Cren","AOA", "AOB","Eub","AOB","Archaea","Cren","AOA","AOB"), nrow(data))
    data$id <- paste(data$Station,data$Datetime.GMT)
    site.metadata <- data[!duplicated(data$id),]
    site.metadata <- with(site.metadata,
                          data.frame(id=id, year=Datetime.GMT, name=Station, lat=latitude, long=longitude, address=NA, area=NA)
                          )
    site <- rep(paste(data$Station,data$Datetime.GMT), 12)
    abundance <- unname(unlist(data[,10:21]))

    return(.df.melt(taxon, site , abundance,
                    study.metadata=data.frame(units="#"),
                    site.metadata,
                    species.metadata=data.frame(species=unique(taxon), taxonomy=NA)))
}

#' @export
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

#' @export
.johnson.2017 <- function(...){
    datam<-read.csv("https://datadryad.org/bitstream/handle/10255/dryad.145772/Species_x_SiteMatrix.csv?sequence=1", as.is=TRUE)
    sitedataA<-read.csv("https://datadryad.org/bitstream/handle/10255/dryad.148018/RawSoilData.csv?sequence=1",as.is = TRUE)
    sitedataB<-read.csv("https://datadryad.org/bitstream/handle/10255/dryad.145776/VacantLot_DemolitionDate.csv?sequence=1",as.is = TRUE)
    sppdata<-read.csv("https://datadryad.org/bitstream/handle/10255/dryad.145777/Species_x_TraitsMatrix.csv?sequence=1",as.is = TRUE)
    
    comm<-datam[,-(1:2)]
    
    sitedataB <- rbind(sitedataB, sitedataB)
    sitedataB$new.code <- paste(sitedataB$Code, rep(c("BF","RG"), each=nrow(sitedataB)/2), sep=".")
    sitedataA$new.code <- paste(sitedataA$LotID, rep(c("BF","RG"), each=nrow(sitedataA)/2), sep=".")
    
    sitedata<-merge(sitedataA,sitedataB,by="new.code",all.x=TRUE,all.y = TRUE)
    names(sitedata)[c(1,27)] <- c("id","address")
    sitedata$lat <- NA;sitedata$long <-NA; sitedata$area <- NA
    sitedata$year <- "2012-2013"
    sitedata$name <- sitedata$id
    names(sppdata)[1:2] <- c("species","taxonomy")
    
    return(.matrix.melt(comm,
                        data.frame(units="percent"),
                        sitedata,
                        sppdata)
           )
}

#' @export
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

#' @export
.kay.2017 <- function(...){
    # Species table used in the calculation of co-occurrence networks
    data <- read.csv(text=paste0(head(readLines(suppdata("10.5061/dryad.3j7f6", "Dryad_data.csv")), -10), collapse="\n"))
    # data$Site <- gsub("-", "_", data$Site)
    colnames(data)[(11:69)] <- c('Acritoscincus_duperreyi','Acritoscincus_platynotum','Amalosia_rhombifer',"Amalosia_tryoni",
                                 'Amphibolurus_burnsi','Amphibolurus_muricatus','Anomalopus_leuckartii','Aprasia_parapulchella','Carlia_pectoralis',
                                 'Carlia_tetradactyla','Carlia_vivax','Chelodina_longicollis','Christinus_marmoratus','Cryptoblepharus_pannosus',
                                 'Cryptoblepharus_pulcher','Cryptophis_nigrescens','Ctenotus_orientalis','Ctenotus_spaldingi','Ctenotus_taeniolatus',
                                 'Delma_inornata','Delma_plebeia','Delma_tincta','Demansia_psammophis_ssp_psammophis','Diplodactylus_vittatus',
                                 'Diporiphora_nobbi','Egernia_cunninghami','Egernia_striolata','Furina_diadema','Gehyra_dubia','Gehyra_variegata',
                                 'Hemiergis_talbingoensis','Heteronotia_binoei','Lampropholis_delicata','Lampropholis_guichenoti','Lerista_bougainvillii',
                                 'Lerista_fragilis','Lerista_timida','Liopholis_whitii','Lucasium_steindachneri','Lygisaurus_foliorum','Menetia_greyii',
                                 'Menetia_timlowi','Morethia_boulengeri','Nebulifera_robusta','Parasuta_dwyeri','Pogona_barbata','Pseudechis_guttatus',
                                 'Pseudechis_porphyriacus','Pseudonaja_textilis','Ramphotyphlops_nigrescens','Ramphotyphlops_wiedii','Saiphos_equalis',
                                 'Strophurus_intermedius','Suta_suta','Tiliqua_rugosa_ssp_aspera','Tiliqua_scincoides_ssp_scincoides','Underwoodisaurus_milii',
                                 'Unidentified_skink','Varanus_varius')
    # Site name, species, season, count
    data <- melt(data,measure.vars=c('Acritoscincus_duperreyi','Acritoscincus_platynotum','Amalosia_rhombifer',"Amalosia_tryoni",
                                     'Amphibolurus_burnsi','Amphibolurus_muricatus','Anomalopus_leuckartii','Aprasia_parapulchella','Carlia_pectoralis',
                                     'Carlia_tetradactyla','Carlia_vivax','Chelodina_longicollis','Christinus_marmoratus','Cryptoblepharus_pannosus',
                                     'Cryptoblepharus_pulcher','Cryptophis_nigrescens','Ctenotus_orientalis','Ctenotus_spaldingi','Ctenotus_taeniolatus',
                                     'Delma_inornata','Delma_plebeia','Delma_tincta','Demansia_psammophis_ssp_psammophis','Diplodactylus_vittatus',
                                     'Diporiphora_nobbi','Egernia_cunninghami','Egernia_striolata','Furina_diadema','Gehyra_dubia','Gehyra_variegata',
                                     'Hemiergis_talbingoensis','Heteronotia_binoei','Lampropholis_delicata','Lampropholis_guichenoti','Lerista_bougainvillii',
                                     'Lerista_fragilis','Lerista_timida','Liopholis_whitii','Lucasium_steindachneri','Lygisaurus_foliorum','Menetia_greyii',
                                     'Menetia_timlowi','Morethia_boulengeri','Nebulifera_robusta','Parasuta_dwyeri','Pogona_barbata','Pseudechis_guttatus',
                                     'Pseudechis_porphyriacus','Pseudonaja_textilis','Ramphotyphlops_nigrescens','Ramphotyphlops_wiedii','Saiphos_equalis',
                                     'Strophurus_intermedius','Suta_suta','Tiliqua_rugosa_ssp_aspera','Tiliqua_scincoides_ssp_scincoides','Underwoodisaurus_milii',
                                     'Unidentified_skink','Varanus_varius'))
    # Site name, species, season, count
    subdata <- data[,c(3:4,11:12)]
    subdata$Site_Year<-with(data, paste(Site, Year, sep = "_"))
    colnames(subdata)[3:4]<- c('Species','Count')
    subdata$Count[subdata$Count>0] <- 1
    subdata <- with(subdata, tapply(Count, list(Site_Year, Species), sum))
    # .df.melt(value=subdata$value, species=subdata$variable, site=paste(subdata$Site,subdata$Year), ...othershit...)
    subdata[is.na(subdata)] <- 0
    subdata[subdata > 0] <- 1
    plots_years <- unlist(strsplit(rownames(subdata), "_", fixed=T))
    plots <- plots_years[seq(1,length(plots_years), 2)]
    years <- plots_years[seq(2,length(plots_years), 2)]
    return(.matrix.melt(subdata, 
                        data.frame(units="#"), 
                        data.frame(id=rownames(subdata), year=years, name=plots, lat=NA, long=NA, address="Southern Queensland to Southern New South Wales", area=NA),
                        data.frame(species=colnames(subdata), taxonomy="Animalia")))
}

#' @export
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

#' @export
.laverick.2017 <- function(...){
    data <- read.csv(suppdata("10.1371/journal.pone.0183075",7), stringsAsFactors=FALSE)
    rownames(data) <- data$X
    data <- data[,-c(1,44:46)]
    return(.matrix.melt(as.matrix(data),
                        data.frame(units="#"),
                        data.frame(id=rownames(data), year=NA, name=NA, lat=NA, long=NA, address="Island of Utila, Honduras", area=NA),
                        data.frame(species=colnames(data), taxonomy=NA)))
}

#' @export
.lorite.2017<-function(...){
    expdata<-read.delim("https://doi.org/10.1371/journal.pone.0182414.s003", nrows=410)
    lookup <- read.delim("https://doi.org/10.1371/journal.pone.0182414.s003", skip=414, nrows=34,as.is = TRUE,header = FALSE)
    lookup<-lookup[,1:2]
    expdata$new.site<-paste(expdata$Site,expdata$transect,expdata$quadrat,sep="_")
    names(expdata)[7:40]<-lookup[,2]
    
    comm<-cbind(id=expdata[,41],expdata[,7:40])
    
    #needs meta data, loc: scattered through paper/tables but existant.
    
    return(.matrix.melt(comm,
                        data.frame(units="percent"),
                        data.frame(id=comm$id,year=NA),
                        data.frame(species=lookup[,2],taxonomy=NA)
                        
                        ))
}

#' @export
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

#' @export
.martins.2018 <- function(...){
    data <- read.csv(.unzip("bee_community.csv", suppdata("10.5061/dryad.841vq48", "Martins_et_al_2018_data_files.zip")))
    names(data) <- tolower(names(data))
    comm <- as.matrix(data[,-1:-2])
    rownames(comm) <- paste(data$x, data$fruit, sep="_")

    lookup <- read.csv(.unzip("bee_names.csv", suppdata("10.5061/dryad.841vq48", "Martins_et_al_2018_data_files.zip")), as.is=TRUE)
    lookup <- setNames(lookup[,2], tolower(lookup[,1]))
    colnames(comm) <- unname(lookup[colnames(comm)])

    return(.matrix.melt(comm,
                        data.frame(units="#"),
                        data.frame(id=rownames(comm), name=rownames(comm), year=NA, lat=NA, long=NA, address=NA, area=NA),
                        data.frame(species=colnames(comm), taxonomy=NA)))
}

#' @export
.matos.2017 <- function(...){
    usa <- as.matrix(read.xls(suppdata("10.5061/dryad.86h2k", "PMATOS_DATA_DRYADES.xlsx"))[,-1])
    eu <- as.matrix(read.xls(suppdata("10.5061/dryad.86h2k", "PMATOS_DATA_DRYADES.xlsx"), 2)[,-1])
    eum <- as.matrix(read.xls(suppdata("10.5061/dryad.86h2k", "PMATOS_DATA_DRYADES.xlsx"), 3)[,-1])

    lookup <- read.xls(suppdata("10.5061/dryad.86h2k", "PMATOS_DATA_DRYADES.xlsx"), 4, as.is=TRUE)
    lookup$Species.name <- .sanitize.text(lookup$Species.name)
    lookup$Species.name <- sapply(strsplit(lookup$Species.name, " "), function(x) paste(x[1:2], collapse="_"))
    lookup <- setNames(lookup$Species.name, lookup$Code)

    colnames(usa) <- lookup[colnames(usa)]
    colnames(eu) <- lookup[colnames(eu)]
    colnames(eum) <- lookup[colnames(eum)]

    rownames(usa) <- paste("usa",seq_len(nrow(usa)), sep="_")
    rownames(eu) <- paste("eu",seq_len(nrow(eu)), sep="_")
    rownames(eum) <- paste("eum",seq_len(nrow(eum)), sep="_")
    
    data <- rbind(matrix2sample(usa), matrix2sample(eu), matrix2sample(eum))

    return(.df.melt(data$id, data$plot, data$abund,
                    data.frame(units="p/a"),
                    data.frame(id=unique(data$plot), name=unique(data$plot), year=2013, lat=NA, long=NA, address=NA, area="NW America; European Union"),
                    data.frame(species=unique(data$id), taxonomy=NA)))
}

#' @export
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

# YEAR UNKNOWN
#' @export
.mcknight.2000 <- function(...){
    data <- read.csv(file="https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/12/3/7f8537c0f0f80a255551ad61d9d512dc",header=TRUE)

    species <- unique(data$Species)
    data$id <- rep(paste(data$Location,data$Date))
    site.metadata <- data[!duplicated(data$id),]
    site.metadata <- with(site.metadata,
                          data.frame(id=id, year=Date, name=Location, lat=NA,long=NA, address="antarctica",area=NA)
                          )
    site <- rep(paste(data$Location,data$Date), 27)
    abundance <- as.vector(data[,10])
    abundance[is.na(abundance)] <- 0

    return(.df.melt(species, site, abundance,
                    study.metadata=data.frame(units="#"),
                    site.metadata,
                    species.metadata=data.frame(species=unique(species), taxonomy=NA)
                    ))
}

#' @export
.mcmahon.2017 <- function(...){
    abun <- read.csv(file = "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.349.2&entityid=da11cbc268d91fef78c78bd2813adbf6", header = TRUE)
    site_meta1 <- read.csv(file = "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.349.2&entityid=a508f609c7d45f1c10604a4722acfd04", header = TRUE)
    site_meta2 <- read.csv(file = "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.349.2&entityid=d35b86dbfcf7bf6eab90a2fd5539809c", header = TRUE)
    org_meta <- read.csv(file = "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.349.2&entityid=5c558e387eadadf707a3f84742b0d3e1", header = TRUE)
    colnames(abun)[1] <- "OTU"
    data_meta1 <- merge(abun, site_meta1, by = "Sample_Name")
    site_data <- merge(data_meta1, site_meta2, by = "Sample_Name")

    data <- merge(org_meta, site_data, by = "OTU")
    comm <- with(data, tapply(value, list(paste(Lake,Collection_Date,sep="_", OTU), length)))
    site.names <- sapply(strsplit(rownames(comm), "_"), function(x) x[1])
    years <- sapply(strsplit(rownames(comm), "_"), function(x) x[2])
    comm[is.na(comm)] <- 0
    unique <- data[!duplicated(data$OTU),]
    colnames(unique)[1] <- 'Species'
    unique <- unique[,-9:-20]
    return(.matrix.melt(comm,
                        data.frame(units="p/a"),
                        data.frame(id=rownames(comm),years,site.names,lat=NA,long=NA,address="North of Minocqua, Wisconsin USA",area="Depth"),
                        data.frame(species=colnames(comm),taxonomy=unique, )))
}

#' @export
.miller.2013 <- function(...){
    data<- read.csv(file = "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-and.2739.7&entityid=1743caa458ea7bb640833d884576f51c", header = TRUE)

    species <- data$ENTITY
    data$id <- rep(paste(data$TRAPID,data$YEAR))
    site.metadata <- data[!duplicated(data$id),]
    site.metadata <- with(site.metadata,
                          data.frame(id=id, year=YEAR, name=TRAPID, lat=NA,long=NA, address="Willamette National Forest Oregon USA",area=NA)
                          )
    site <- rep(paste(data$TRAPID,data$YEAR), 17663)
    abundance <- as.vector(data$NO_INDIV)
    abundance[is.na(abundance)] <- 0

    return(.df.melt(species, site, abundance,
                    study.metadata=data.frame(units="#"),
                    site.metadata,
                    species.metadata=data.frame(species=unique(species), taxonomy=NA)
                    ))
}

#' @export
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

#' @export
.nichols.2006 <- function(...) {
    addr  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/61/3/wgnhs_macrophyte_aquaplt2"
    addr <- sub("^https","http",addr)
    abundanceData <-read.csv(addr, header=F, skip=1, sep=",", quote='"',
                             col.names=c("mwbc", "lake_unique", "lakename",
                                         "county", "county_id", "month", "year4",
                                         "spcode", "aqstano", "visual_abundance"),
                             check.names=TRUE)
    specAddr  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/61/3/wgnhs_macrophyte_pltname"
    specAddr <- sub("^https","http",specAddr)
    specData <-read.csv(specAddr, header=F, skip=1, sep=",", quote='"',
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

#' @export
.nps.1998 <- function(...){
    temp <- tempfile()
    download.file("https://irma.nps.gov/DataStore/DownloadFile/567456", temp)
    data <- read.xls(temp, as.is=TRUE)

    species <- data$Local.Taxon.Name
    data$id <- data$Plot_Event
    site.metadata <- data[!duplicated(data$id),]
    site.metadata <- with(site.metadata,
                          data.frame(id=id,
                                     year=sapply(strsplit(site.metadata$Plot_Event, "."), function(x) x[2]),
                                     name=sapply(strsplit(site.metadata$Plot_Event, "."), function(x) x[1]),
                                     lat=NA, long=NA, address=NA,area=NA)
                          )
    site <- rep(data$Plot_Event, 1091)
    abundance <- as.vector(data[,11])
    abundance[is.na(abundance)] <- 0

    return(.df.melt(species, site, abundance,
                    study.metadata=data.frame(units="%"),
                    site.metadata,
                    species.metadata=data.frame(species=unique(species), taxonomy=NA)
                    ))
}

#' @export
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

#' @export
.oswald.2015 <- function(...){
    data <- as.data.frame(read_xlsx(suppdata("10.5061/dryad.56p0f", "Oswald_et_al_2015_dryad.56p0f.xlsx"))) 
    comm <- data[,-13]
    comm <- comm[,-2:-10]
    names(comm) <- c("Locality", "Species", "Numbers")
    comm$Numbers <- as.numeric(comm$Numbers)
    comm <- with(comm, tapply(Numbers, list(Locality, Species), sum))
    comm[is.na(comm)] <- 0
    return(.matrix.melt(comm, 
                        data.frame(units="#"),
                        data.frame(id=rownames(comm),year="2009-2011",
                                   name=data$Locality,
                                   lat=with(data, tapply(Latitude.Decimal.Degrees, Locality, mean)),
                                   long=with(data, tapply(Longitude.Decimal.Degrees, Locality, mean)),
                                   address="Northwestern Peru, Tumbes, Mara\u00F1\u00F3n Valley",area="na"), 
                        data.frame(species=colnames(comm),taxonomy="Aves")))
}

#' @export
.petermann.2016 <- function(...){
    data <- read.xls(suppdata("10.5061/dryad.9ts28", "Petermann16PLOSONE_data_for_dryad.xlsx"))

    comm <- as.matrix(data[,16:55])
    rownames(comm) <- with(data, paste(Code_NEW, Date_of_sampling))
    
    return(.matrix.melt(comm,
                        data.frame(units="p/a", treatment="artificial treehole communities"),
                        data.frame(id=rownames(comm), name=data$Code_NEW, year=data$Date_of_sampling, lat=NA, long=NA, address=NA, area="artificial treehole"),
                        data.frame(species=colnames(comm), taxonomy=NA)))
}

#' @export
.price.2015 <- function(...){
    # Effects of mountaintop removal mining and valley filling 
    # on the occupancy and abundance of salamander species
    data <- read.csv(suppdata('10.5061/dryad.5m8f6','Price et al. JAPPL-2015-count-data.csv'))
    subdata <- data[,c(1,16:43)]
    subdata$Study.Site <- gsub(' ','',subdata$Study.Site)
    rownames(subdata) <- subdata$Study.Site
    subdata <- subdata[,-c(1)]
    # subdata <- as.matrix(subdata[,-1])
    subdata <- as.matrix(sapply(split.default(subdata, 0:(length(subdata)-1) %/% 4), rowSums))
    colnames(subdata) <- c('Gyrinophilus_porphyriticus','Pseudotriton_ruber','Desmognathus_monticola','Eurycea_cirrigera_adult','Eurycea_cirrigera_larvae','Desmognathus_larvae', 'Desmognathus_fuscus')
    subdata[is.na(subdata)] <- 0
    subdata[subdata > 0] <- 1
    return(.matrix.melt(subdata, 
                        data.frame(units="#"), 
                        data.frame(id=rownames(subdata), year=2013, name=rownames(subdata), lat='144091.438', long='307635.435', address="Edwin S. George Reserve (Michigan, USA)", area=NA),
                        data.frame(species=colnames(subdata), taxonomy="Amphibia")))
}

#' @export
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

#' @export
.russo.2015 <- function(...){
    species <- read.xls(suppdata("10.5061/dryad.6cr82", "DataforDryad_netmaludome.xlsx"), header=FALSE, as.is=TRUE, nrow=2)[2:1,]
    species <- unname(apply(as.matrix(species), 2, paste, collapse="_"))[-1]

    data <- read.xls(suppdata("10.5061/dryad.6cr82", "DataforDryad_netmaludome.xlsx"), as.is=TRUE, skip=3)
    comm <- as.matrix(data[,-1])
    colnames(comm) <- species; rownames(comm) <- data[,1]
    return(.matrix.melt(comm,
                        data.frame(units="#"),
                        data.frame(id=rownames(comm), name=colnames(comm), year="2008-2013", lat=NA, long=NA, address=NA, area="New York state, USA"),
                        data.frame(species=colnames(comm), taxonomy=NA)
                        ))
}

#' @export
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

#' @export
.sandau.2017 <- function(...){
    tmp.file <- tempfile()
    download.file("https://www.datadryad.org/bitstream/handle/10255/dryad.129944/BB_all_4_SimilMatrices_Dryad.xlsx?sequence=1", tmp.file)
    data <- read.xls(tmp.file, sheet=2)
    lookup <- read.xls(suppdata("10.5061/dryad.44bm6", "BB_all_4_SimilMatrices_Dryad.xlsx"), sheet=1, skip=5, header=FALSE, as.is=TRUE)[-1:-8,]
    lookup[,2] <- .sanitize.text(lookup[,2])  
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
                        data.frame(id=site.metadata$site_year, name=site.metadata$PlotID, year=site.metadata$Year, lat=NA, long=NA, address="Grandcour", treatment=site.metadata$Treat, area="20 x 20 m"),
                        data.frame(species=unique(lookup, taxonomy="Plantae"))))
}

#' @export
.schmitt.2012 <- function(...){
    addr  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/46/3/4ded739e78e50552837cf100f251f7ab"
    addr <- sub("^https","http",addr)
    data <-read.csv(addr,header=F, skip=1, sep=",", quote='"',
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

#' @export
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

#' @export
.truxa.2015 <- function(...){
    data <- as.data.frame(read_xlsx(suppdata("10.5061/dryad.fg8f6/1", "Appendix_3.xlsx"), skip=1)) #use skip to skip any rows that you don't want/aren't useful
    comm <- data[,-1:-3] #get rid of columns you don't want
    rownames(comm) <- data$Species #name the rows what you want
    comm <- t(comm) #t=transpose, flip the rows and columns
    return(.matrix.melt(comm, 
                        data.frame(units="#"),
                        data.frame(id=rownames(comm),year="2006-2008",
                                   name=c("Danube non-flooded", "Danude flooded", "Leitha non-flooded", "Leitha flooded", "Morava non-flooded", "Morava flooded"),
                                   lat=c("16\u00BA41'24", "16\u00BA42'20", "16\u00BA51'32", "16\u00BA53'26", "16\u00BA53'22"),
                                   long=c("48\u00BA08'41", "48\u00BA07'53", "48\u00BA00'19", "48\u00BA03'28", "48\u00BA17'00", "48\u00BA17'96"),
                                   address="Eastern Austria",area="na"), 
                        data.frame(species=colnames(comm),taxonomy="Lepidoptera")))
}

#' @export
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

#' @export
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

#' @export
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

#' @export
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

#' @export
.ross.2014 <- function(...){
    data <- read.csv(file = "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-and.3136.5&entityid=88e40dc185bd3f00e7464398b61f40fc", header = TRUE)

    species <- data$SCI_NAME
    data$id <- rep(paste(data$BIOGEOGRAPHY,data$DATE))
    site.metadata <- data[!duplicated(data$id),]
    site.metadata <- with(site.metadata,
                          data.frame(id=id, year=DATE, name=BIOGEOGRAPHY, lat=NA,long=NA, address=NA,area=NA)
                          )
    site <- rep(paste(data$BIOGEOGRAPHY,data$DATE), 856)
    abundance <- as.vector(data$INDIVIDUALS)
    abundance[is.na(abundance)] <- 0

    return(.df.melt(species, site, abundance,
                    study.metadata=data.frame(units="#"),
                    site.metadata,
                    species.metadata=data.frame(species=unique(species), taxonomy=NA)
                    ))
}

#' @export
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

#' @export
.valtonen.2017 <- function(...){
    species <- read.xls(suppdata("10.5061/dryad.9m6vp/1", "valtonen_etal_JAE.xlsx"))
    comm <- as.matrix(species[,-1:-2])
    rownames(comm) <- paste(species$Site, species$Year)
    
    return(.matrix.melt(comm,
                        data.frame(units="#", treatment="light_trap"),
                        data.frame(id=rownames(comm), year=species$Year, name=species$Site, lat=NA, long=NA, address = paste0(species$Site, ", Hungary"), area="light_trap"),
                        data.frame(species=colnames(comm), taxonomy=NA)))
}

#' @export
.wang.2017a <- function(...){
    tmp <- tempfile()
    download.file("https://bdj.pensoft.net/article/download/suppl/3909480/", tmp)
    tree_data <- read.xls(tmp, 1) 
    tree_data <- tree_data[!is.na(tree_data$diameter.at.breast.height..cm.),]
    plot_ids <- tree_data$Plot.number
    # lat/long data
    download.file("https://bdj.pensoft.net/article/22167/download/csv/3909467/", tmp)
    ll_data <- read.csv(tmp, sep = ";")
    names(ll_data) <- c("forest_type", "id", "lat", "long")
    ll_data$year <- 2017; ll_data$name <- ll_data$id
    ll_data$forest_type <- NULL
    ll_data$address <- "Liangshui National Natural Reserve"; ll_data$area <- "25m*25m"
    return(.df.melt(tree_data$species_name,
                    plot_ids,
                    tree_data$diameter.at.breast.height..cm.,
                    data.frame(units = "dbh"),
                    ll_data,
                    data.frame(species= unique(tree_data$species_name), taxonomy="Plantae")
                    ))
}

#' @export
.wang.2017b <- function(...){
    tmp <- tempfile()
    download.file("https://bdj.pensoft.net/article/download/suppl/3909480/", tmp)
    shrub_data <- read.xls(tmp, 2)
    shrub_data <- shrub_data[!is.na(shrub_data$coverage),]
    plot_ids <- shrub_data$Plot.number
    plot_ids <- gsub("-S[0-9]+", "", plot_ids)
    # lat/long data
    download.file("https://bdj.pensoft.net/article/22167/download/csv/3909467/", tmp)
    ll_data <- read.csv(tmp, sep = ";")
    names(ll_data) <- c("forest_type", "id", "lat", "long")
    ll_data$year <- 2017; ll_data$name <- ll_data$id
    ll_data$forest_type <- NULL
    ll_data$address <- "Liangshui National Natural Reserve"; ll_data$area <- "5m*5m"
    ll_data$id <- gsub("-T", "", ll_data$id, fixed=TRUE)
    return(.df.melt(shrub_data$species_name,
                    plot_ids,
                    shrub_data$coverage,
                    data.frame(units = "%"),
                    ll_data,
                    data.frame(species = unique(shrub_data$species_name), taxonomy = "Plantae")
                    )
           )
}

#' @export
.wang.2017c <- function(...){
    tmp <- tempfile()
    download.file("https://bdj.pensoft.net/article/download/suppl/3909480/", tmp)
    herb_data <- read.xls(tmp, 3)
    herb_data <- herb_data[!is.na(herb_data$coverage),]
    plot_ids <- herb_data$Plot.number
    plot_ids <- gsub("-H[0-9]+", "", plot_ids)
    # lat/long data
    download.file("https://bdj.pensoft.net/article/22167/download/csv/3909467/", tmp)
    ll_data <- read.csv(tmp, sep = ";")
    names(ll_data) <- c("forest_type", "id", "lat", "long")
    ll_data$year <- 2017; ll_data$name <- ll_data$id
    ll_data$forest_type <- NULL
    ll_data$address <- "Liangshui National Natural Reserve"; ll_data$area <- "1m*1m"
    ll_data$id <- gsub("-T", "", ll_data$id, fixed=TRUE)
    ll_data <- ll_data[ll_data$id!="14",]
    return(.df.melt(herb_data$species_name,
                    plot_ids,
                    herb_data$coverage,
                    data.frame(units = "%"),
                    ll_data,
                    data.frame(species = unique(herb_data$species_name), taxonomy = "Plantae")
                    )
           )
}

#' @export
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

#' @export
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

#' @export
.werner.2014 <- function(...){
    # abundance and occupancy patterns for amphibian species 
    # on the ES George Reserve (Michigan, USA) over multiple years
    data <- read.xls(suppdata('10.5061/dryad.js47k','Werner_etal_ESGR_PLOS_data_Files.xlsx'),sheet=2)
    subdata <- data[,-c(3:9)]
    colnames(subdata) <- c('Year','Site','Ambystoma_laterale','Ambystoma_maculatum', 'Ambystoma_tigrinum','Pseudacris_crucifer', 'Pseudacris_triseriata',  'Rana_pipiens','Rana_sylvatica')
    subdata$Site <- gsub(' ','',subdata$Site)
    subdata$Site <- gsub('#','',subdata$Site)
    subdata$Site <- gsub('\'','',subdata$Site)
    subdata$Site_Year<-with(subdata, paste(Site, Year, sep = "_"))
    rownames(subdata) <- subdata[,10]
    subdata <- subdata[,-c(1,2,10)]
    subdata[is.na(subdata)] <- 0
    subdata[subdata > 0] <- 1
    subdata <- as.matrix(subdata)
    plots_years <- unlist(strsplit(rownames(subdata), "_", fixed=T))
    plots <- plots_years[seq(1,length(plots_years), 2)]
    years <- plots_years[seq(2,length(plots_years), 2)]
    return(.matrix.melt(subdata, 
                        data.frame(units="#"), 
                        data.frame(id=rownames(subdata), year=years, name=plots, lat='42.4585', long='84.0115', address="Edwin S. George Reserve (Michigan, USA)", area=NA),
                        data.frame(species=colnames(subdata), taxonomy="Amphibia")))
}

#' @export
.westgate.2015 <- function(...){
    # Frog species occurences across urban-rural sites in australia between 2002-2014
    data <- read.xls(suppdata('10.5061/dryad.75s51','Frogwatch_dataset.xlsx'),sheet=2)
    subdata <- data[,c(1:2,11:18)]
    colnames(subdata) <- c('Site','Year','Crinia_parinsignifera','Crinia_signifera','Limnodynastes_dumerilii','Limnodynastes_peronii','Limnodynastes_tasmaniensis','Litoria_peronii','Litoria_verreauxii','Uperoleia_laevigata')
    subdata$Site_Year<-with(subdata, paste(Site, Year, sep = "_"))
    # rownames(subdata) <- subdata[,c(11)]
    subdata <- subdata[,-c(1,2)]
    subdata<-aggregate(subdata[,1:8], list(subdata[,9]), function(x) sum(unique(x)))
    rownames(subdata) <- subdata[,1]
    subdata <- subdata[,-1]
    subdata <- as.matrix(subdata)
    subdata[is.na(subdata)] <- 0
    subdata[subdata > 0] <- 1
    plots_years <- unlist(strsplit(rownames(subdata), "_", fixed=T))
    plots <- plots_years[seq(1,length(plots_years), 2)]
    years <- plots_years[seq(2,length(plots_years), 2)]
    return(.matrix.melt(subdata, 
                        data.frame(units="#"), 
                        data.frame(id=rownames(subdata), year=years, name=plots, lat='-37.300000', long='149.100000', address="Surrounding areas of New South Wales (NSW), southeastern Australia", area=NA),
                        data.frame(species=colnames(subdata), taxonomy="Amphibia")))
}

#' @export
.ylanne.2018 <- function(...){
    data <- read.xls(suppdata("10.5061/dryad.bb49h", "Ylanne_etal_2017_FunctEcol_data.xlsx"), skip=1, sheet=3)

    species <- rep(names(data)[9:82], each=nrow(data))
    year <- '2014'
    data$id <- rep(paste(data$Plot,data$year))
    site.metadata <- data[!duplicated(data$id),]
    site.metadata <- with(site.metadata,
                          data.frame(id=id, year=year, name=Plot, lat=NA,long=NA, address=NA,area=Area)
                          )
    site <- rep(paste(data$Plot,data$Year), 74)
    abundance <- unname(unlist(data[,9:82]))
    abundance[is.na(abundance)] <- 0

    return(.df.melt(species, site , abundance,
                    study.metadata=data.frame(units="#"),
                    site.metadata,
                    species.metadata=data.frame(species=unique(species), taxonomy=NA)))
}

