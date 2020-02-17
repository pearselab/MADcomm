newbold.clean <- function(data) {
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

#' @export
.newbold.2016a <- function(...) {
    tmp.file <- tempfile()
    download.file("http://data.nhm.ac.uk/dataset/902f084d-ce3f-429f-a6a5-23162c73fdf7/resource/1e82548a-5f1e-4d32-861f-e00a740ea296/download/database.rds", tmp.file)
    data <- readRDS(tmp.file)
    data <- data[which(data[,6] == "abundance"),]
    data <- newbold.clean(data)
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

#' @export
.newbold.2016b <- function(...) {
    tmp.file <- tempfile()
    download.file("http://data.nhm.ac.uk/dataset/902f084d-ce3f-429f-a6a5-23162c73fdf7/resource/1e82548a-5f1e-4d32-861f-e00a740ea296/download/database.rds", tmp.file)
    data <- readRDS(tmp.file)
    data <- data[which(data[,6] == "occurrence"),]
    data <- newbold.clean(data)
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

#' @export
.newbold.2016c <- function(...) {
    tmp.file <- tempfile()
    download.file("http://data.nhm.ac.uk/dataset/902f084d-ce3f-429f-a6a5-23162c73fdf7/resource/1e82548a-5f1e-4d32-861f-e00a740ea296/download/database.rds", tmp.file)
    data <- readRDS(tmp.file)
    data <- data[which(data[,6] == "percent cover"),]
    data <- newbold.clean(data)
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

#' @export
.newbold.2016d <- function(...) {
    tmp.file <- tempfile()
    download.file("http://data.nhm.ac.uk/dataset/902f084d-ce3f-429f-a6a5-23162c73fdf7/resource/1e82548a-5f1e-4d32-861f-e00a740ea296/download/database.rds", tmp.file)
    data <- readRDS(tmp.file)
    data <- data[which(data[,6] == "biomass"),]
    data <- newbold.clean(data)
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
