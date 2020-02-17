# NEONMammals
#' @export
.neon.2018a <- function(...){
  dat <- loadByProduct("DP1.10072.001", site ="all", startdate = 2013, enddate = 2018)
  trapnight <- dat$mam_pertrapnight
  trapnight$collectDate <- substr(trapnight$collectDate,0,7)
  trapnight$id <- paste0(trapnight$plotID, "_", trapnight$collectDate)
  data <- trapnight[,c("id", "plotID", "collectDate", "decimalLatitude", "decimalLongitude", "scientificName"),]
  data <- data[which(data$scientificName!=""),]
  names(data) <- c("id", "name", "year", "lat", "long", "species")
  temp <- with(data, paste(id, species))
  temp.counts <- table(temp)
  data$value <- temp.counts[temp]
  data$value <- as.numeric(data$value)
  data <- unique(data)
  site.df <- data[, c("id", "name", "year", "lat", "long")]
  site.df <- unique(site.df)
  site.df$address <- NA; site.df$area <- "10 sherman traps"
  return(.df.melt(data$species, data$id, data$value, data.frame(units="g"), site.df, data.frame(species=unique(data$species),taxonomy=NA)))
}

# Beetles
#' @export
.neon.2018b <- function(...){
  dat <- loadByProduct("DP1.10022.001", site ="all", startdate = 2014, enddate = 2018)
  vst <- dat$bet_archivepooling
  vst$collectDate <- substr(vst$collectDate,0,7)
  vst$id <- paste0(vst$plotID, "_", vst$collectDate)
  data <- vst[,c("id", "plotID", "collectDate", "scientificName"),]
  data <- data[which(data$scientificName!=""),]
  names(data) <- c("id", "name", "year", "species")
  temp <- with(data, paste(id, species))
  temp.counts <- table(temp)
  data$value <- temp.counts[temp]
  data$value <- as.numeric(data$value)
  data <- unique(data)
  site.df <- data[, c("id", "name", "year")]
  site.df <- unique(site.df)
  site.df$lat <- NA; site.df$long <- NA
  site.df$address <- NA; site.df$area <- "pitfall trap"
  return(.df.melt(data$species, data$id, data$value, data.frame(units="#"), site.df, data.frame(species=unique(data$species),taxonomy=NA)))
}

# Plants
#' @export
.neon.2018c <- function(...){
  dat <- loadByProduct("DP1.10098.001", site ="all", startdate = 2013, enddate = 2018)
  vst <- dat$vst_mappingandtagging
  vst$date <- substr(vst$date,0,7)
  vst$id <- paste0(vst$plotID, "_", vst$date)
  data <- vst[,c("id", "plotID", "date", "scientificName"),]
  names(data) <- c("id", "name", "year", "species")
  data$species <- str_extract(data$species, "[^ ]+ [^ ]+")
  temp <- with(data, paste(id, species))
  temp.counts <- table(temp)
  data$value <- temp.counts[temp]
  data$value <- as.numeric(data$value)
  data <- unique(data)
  site.df <- data[, c("id", "name", "year")]
  site.df <- unique(site.df)
  site.df$lat <- NA; site.df$long <- NA
  site.df$address <- NA; site.df$area <- "1m2"
  return(.df.melt(data$species, data$id, data$value, data.frame(units="#"), site.df, data.frame(species=unique(data$species),taxonomy=NA)))
}

# Birds
#' @export
.neon.2018d <- function(...){
  dat <- loadByProduct("DP1.10003.001", site ="all", startdate = 2013, enddate = 2018)
  vst <- dat$brd_countdata
  vst$date <- substr(vst$startDate,0,7)
  vst$id <- paste0(vst$plotID, "_", vst$date)
  data <- vst[,c("id", "plotID", "date", "scientificName", "clusterSize"),]
  names(data) <- c("id", "name", "year", "species", "value")
  data <- aggregate(data$value, by=list(data$id, data$name, data$year, data$species), FUN=sum, na.rm=TRUE)
  names(data) <- c("id", "name", "year", "species", "value")
  site.df <- data[, c("id", "name", "year")]
  site.df <- unique(site.df)
  site.df$lat <- NA; site.df$long <- NA
  site.df$address <- NA; site.df$area <- "?"
  return(.df.melt(data$species, data$id, data$value, data.frame(units="#"), site.df, data.frame(species=unique(data$species),taxonomy=NA)))
}
