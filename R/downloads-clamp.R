################################################
########## CLAMP WRAPPERS ####### ##############
########## Have fun, but be careful! ###########
################################################
## Issue: Figure out handling of duplicate df.names (e.g. a,b,c?) 
## Issue: UCMP/USNM df.name formatting (e.g. 1,2,3?)

### CLAMP Neogene Wrapper:: 
.clamp.neogene <- function(dfname){
  
  dflist<-as.vector(c('.lamotte.1936',".axelrod.1956a",".axelrod.1950",".oliver.1936",".axelrod.1991",".ucmp.1",".axelrod.1956b",
                      ".ucmp.2",".chaney.1920",".axelrod.1985a",".axelrod.1956c",".usnm.1",".wolfe.1964a",
                      ".ucmp.3",".usnm.2",".wolfe.1994a",".wolfe.1994b",".ucmp.4",".wolfe.1994c",".chaney.1959",".smiley.1963",
                      ".axelrod.1985b",".wolfe.1994d",".usnm.3",".ucmp.5",".condit.1938",".axelrod.1992",".condit.1944a",".usnm.4",
                      ".wolfe.1980",".axelrod.1944",".wolfe.1964b",".chaney.axelrod",".wahrhaftig.1969",".condit.1944b",".axelrod.1939",
                      ".renney.1972",".axelrod.1964c",".macginitie.1933",".chaney.1944",".axelrod.1980",".schorn.ucmp",".wolfe.1994e",
                      ".macginitie.1937",".ucmp.6",".wolfe.1994f"))
  linklist <- as.vector(c('http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/49Camp%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Aldrich%20Station%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Anaverde%20CA.xls',
                          "http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Blue%20Mountains%20OR.xls",
                          "http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Buffalo%20Canyon%20NV.xls",
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Burlington%20Ridge%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Chloropagus%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Clarkia%20ID.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Eagle%20Creek%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Eastgate%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Fallon%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Faraday%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Fingerrock%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Goldyke%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Hidden%20Lake%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Liberal%20OR.xls',
                          "http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Lower%20Clamgulchian%20AK.xls",
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Lower%20Gillam%20Springs%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Lower%20Homarian%20AK.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Mascall%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Mid%20Ellensburg%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Middlegate%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Middle%20Homerian%20AK.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Mohawk%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Mollala%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Neroly%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Pyramid%20Lake%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Remington%20Hill%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Scio%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Seldovia%20Point%20AK.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Sonoma%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Stewart%20Spring%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Stinking%20Water%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Suntrana%20AK.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Table%20Mountain%20CA.xls',
                          "http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Tehachapi%20CA.xls",
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Temblor%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Trapper%20Creek%20ID.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Trout%20Creek%20ID.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Troutdale%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Turlock%20Lake%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Upper%20Gillam%20Springs%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Upper%20Homerian%20AK.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Weaverville%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Webber%20Lake%20CAxls.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Neogene_Scoresheets/Weyerhauser%20OR.xls'))
  catalog <- data.frame(dflist,linklist)
  catalog$linklist <- as.character(catalog$linklist)
  dfnum <- which(catalog$dflist==deparse(substitute(dfname)))
  link <- catalog$linklist[dfnum]
  
  data <- read.xls(link, as.is=TRUE, fileEncoding="latin1", skip=1)
  age <- data[1,9]
  age <- as.numeric(sub(".*assumed age *(.*?) * Ma.*", "\\1", age))
  age <- -1000000*age
  name <- data[1,2]
  name <- gsub(' ','_',name)
  lat <- data[1,4]
  lat <- as.numeric(gsub("\u00B0", "", lat, fixed = TRUE))
  lon <- data[1,5]
  lon <- as.numeric(gsub("\u00B0", "", lon, fixed = TRUE))
  
  if(dfnum==1){ 
    data <- read.xls(link, as.is=TRUE, fileEncoding="latin1", skip=4)[,2]
    data <- gsub(' ','_',data)
    data <- gsub('\\\\','',data)
    data[c(2,3,6,7,11,16,17,18,19,20,21,22,23,24,25)] <- c('Populus_washoensis_lindgrenii','Salix_arbutus_pp',
                                                           'Q_simulata_castanop_umb','Fagus_incl_sorbus',
                                                           'Nordenskioldia_cebatha','Prunus_masoni',
                                                           'Acer_medianum','Acer_smileyi','Acer_busamarum',
                                                           'Acer_collawashense','Oreopanax','Acer_negundo',
                                                           'Phyllites_succosa','Arbutus_pp','Cedrela')
  }
  else {
    data <- read.xls(link, as.is=TRUE, fileEncoding="latin1", skip=6)[,2]
    data <- gsub("[^[:alnum:][:space:]]","",data)
    data <- gsub(' ','_',data)
  }
  sp <- character()
  for (row in 1:length(data)){
    if(nchar(data[row]) > 2){ 
      sp[row] <- data[row]
    }
    else {
      break
    }
  }
  
  sp <- as.matrix(sp)
  subdata <- matrix(1,nrow=1,ncol=length(sp[,1]))
  colnames(subdata) <- sp[,1]
  colnames(subdata) <- make.unique(colnames(subdata),sep='_')
  rownames(subdata) <- name
  subdata <- as.matrix(subdata)
  
  return(.matrix.melt(subdata,data.frame(units="p/a"),
                      data.frame(id=rownames(subdata), year=age,name=name,lat=lat,long=lon, address=NA, area=NA),
                      data.frame(species=colnames(subdata), taxonomy="NA")))
}

### CLAMP Paleogene Wrapper:: 

.clamp.paleogene <- function(dfname){
  
  dflist<-as.vector(c('.usgsusnm.1','.uwburke.2737','.mcginitie.1941','.gscusgs.1','.sanborn.1935','.axelrod.1966','.wolfe.1991',
                      '.mcginitie.1953','.chaney.1933','.chaney.1927','.macginitie.1969','.clarno','.macginitie.1974','.uwburke.4132',
                      '.wolfe.1994a','.potbury.1935','.wolfe.1998a','.ucmpusnm','.usgs.8637','.burke','.usgs.9676','.usgs.9678','.usgs.9680',
                      '.puget.9694','.puget.9731','.usgsusnm.2','.wolfe.1994b','.usgs.9841','.uwburke','.lakhanpal.1958','.wolfe.1998b',
                      '.usnm.1','.usgsusnm.2','.wolfe.1994c','.ucmpusgs','.wolfe.1998c'))
  linklist <- as.vector(c('http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Bilyeu%20Creek%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Boot%20Hill%20WA%20(Republic).xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Chalk%20Bluffs%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Chua%20Chua%20BC.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Comstock%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Copper%20Basin%20NV.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Creede%20CO.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Florissant%20CO.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Goshen%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Grays%20Ranch%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Green%20River%20CO_UT.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/John%20Day%20Gulch%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Kissinger%20Lakes%20WY.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Knob%20Hill%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Kulthieth%20AK.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/La%20Porte%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Little%20Mountain%20WY.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Lyons%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Mitchell%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/One%20Mile%20Creek%20BC.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Puget%209676%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Puget%209678%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Puget%209680%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Puget%209694%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Puget%209731%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Puget%209833%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Puget%209835%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Puget%209841%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Republic%20WA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Rujada%20Point%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Salmon%20ID.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Sandstone%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Susanville%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Sweet%20Home%20CA.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Willamette%20OR.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Paleogene_Scoresheets/Yaquina%20OR.xls'))
  
  catalog <- data.frame(dflist,linklist)
  catalog$linklist <- as.character(catalog$linklist)
  dfnum <- which(catalog$dflist==deparse(substitute(dfname)))
  link <- catalog$linklist[dfnum]
  data <- read.xls(link, as.is=TRUE, fileEncoding="latin1", skip=1)
  age <- data[1,9]
  age <- as.numeric(sub(".*assumed age *(.*?) * Ma.*", "\\1", age))
  age <- -1000000*age
  name <- data[1,2]
  name <- gsub(' ','_',name)
  lat <- data[1,4]
  lat <- as.numeric(gsub("\u00B0", "", lat, fixed = TRUE))
  lon <- data[1,5]
  lon <- as.numeric(gsub("\u00B0", "", lon, fixed = TRUE))
  data <- read.xls(link, as.is=TRUE, fileEncoding="latin1", skip=6)[,2]
  data <- gsub("[^[:alnum:][:space:]]","",data)
  data <- gsub(' ','_',data)
  
  sp <- character()
  for (row in 1:length(data)){
    if(nchar(data[row]) > 2){ 
      sp[row] <- data[row]
    }
    else {
      break
    }
  }
  
  sp <- as.matrix(sp)
  subdata <- matrix(1,nrow=1,ncol=length(sp[,1]))
  colnames(subdata) <- sp[,1]
  colnames(subdata) <- make.unique(colnames(subdata),sep='_')
  rownames(subdata) <- name
  subdata <- as.matrix(subdata)
  
  return(.matrix.melt(subdata,data.frame(units="p/a"),
                      data.frame(id=rownames(subdata), year=age,name=name,lat=lat,long=lon, address=NA, area=NA),
                      data.frame(species=colnames(subdata), taxonomy="NA")))
}

### CLAMP Cretaceous Wrapper:: 
.clamp.cretaceous <- function(dfname){
  
  dflist<-as.vector(c('.ginras.1','.spicer.2002','.ginras.2','.ginras.3','.ginras.4','.rasusnm.1',
                      '.budantsev.1968','.craggs.2005','.spicer.2008a','.spicer.2008b'))
  linklist <- as.vector(c('http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/Arman%20(Cenomanian).xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/Grebenka.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/Kamchatkan%20Penzlina.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/Kamchatkan%20Kaysayam.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/Kazakhstan%20(Cenomanian).xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/North%20Slope%20Coniacian.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/Novaya%20Sibir%20(Turonian).xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/Tylpeggrgynai%20Flora.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/Vilui%20123A123%20Cenomanian.xls',
                          'http://clamp.ibcas.ac.cn/Fossil_scoresheets/Cretaceous_Scoresheets/Vilui%20123B123%20(Cenomanian).xls'))
  
  linklist <- gsub('123','\'',linklist)
  
  catalog <- data.frame(dflist,linklist)
  catalog$linklist <- as.character(catalog$linklist)
  dfnum <- which(catalog$dflist==deparse(substitute(dfname)))
  link <- catalog$linklist[dfnum]
  data <- read.xls(link, as.is=TRUE, fileEncoding="latin1", skip=1)
  name <- data[1,2]
  name <- gsub("[^[:alnum:][:space:]]","",name)
  name <- gsub(' ','_',name)
  data <- read.xls(link, as.is=TRUE, fileEncoding="latin1", skip=6)[,2]
  data <- gsub("[^[:alnum:][:space:]]","",data)
  data <- gsub(' ','_',data)
  
  sp <- character()
  for (row in 1:length(data)){
    if(nchar(data[row]) > 2){ 
      sp[row] <- data[row]
    }
    else {
      break
    }
  }
  
  sp <- as.matrix(sp)
  subdata <- matrix(1,nrow=1,ncol=length(sp[,1]))
  colnames(subdata) <- sp[,1]
  colnames(subdata) <- make.unique(colnames(subdata),sep='_')
  rownames(subdata) <- name
  subdata <- as.matrix(subdata)
  
  return(.matrix.melt(subdata,data.frame(units="p/a"),
                      data.frame(id=rownames(subdata), year=NA,name=name,lat=NA,long=NA, address=NA, area=NA),
                      data.frame(species=colnames(subdata), taxonomy="NA")))
}
