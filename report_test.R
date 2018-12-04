#------- head ---------------------------------------
library(shiny)
library(DT)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(prodlim)
library(sparkline)
library(RSQLite)
library(data.table)
library(magrittr)


source("ReadIndicatorType.R")
source("classoutputtable.R")
source("Aggregation.R")
source("Assessment.R")
source("helpfunctions.R")
source("extrapolation.R")

source("IndicatorFunctions.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")
source("ReadBounds.R")
source("report.R")

# ---- database functions -----------------------------------


yearrange<-function(periodlist){
  periods<-data.frame(period=periodlist)
  periods<-periods %>% separate(period,into=c("from","to"),remove=T)
  yrmin=min(as.numeric(periods$from))
  yrmax=max(as.numeric(periods$to))+1
  #from<-as.Date(paste0(yrmin,"-01-01"))
  #to<-as.Date(paste0(yrmax,"-01-01"))
  return(c(yrmin,yrmax))
}

readdb <- function(dbname,strSQL){
  db <- dbConnect(SQLite(), dbname=dbname)
  df <- dbGetQuery(db, strSQL)
  dbDisconnect(db)
  return(df)
}

getres <- function(dbpath,table,wb,periodlist){
  periodlist<-paste(paste0("'",periodlist,"'"),collapse = ",")
  sql<-paste0("SELECT * FROM ",table," WHERE period IN (",periodlist,") AND WB='",wb,"'")
  df<-readdb(dbpath, sql)
  return(df)
}
getobs <- function(dbpath,table,wblist,periodlist){
  years<-yearrange(periodlist)
  sql<-paste0("SELECT * FROM data WHERE WB='",wb,"' AND Year >=",years[1]," AND Year<=",years[2])
  df<-readdb(dbpath, sql)
  df$date<-as.Date(df$date,origin="1970-01-01")
  return(df)
}

#----------------- parameters -------------------

pressureList<-c("Nutrient loading","Organic loading","Acidification","Harmful substances","Hydrological changes","Morphological changes","General pressure")
indicatorList<-c('CoastChlaEQR','CoastBiovolEQR','CoastBQI','CoastMSMDI','CoastSecchiEQR','CoastDINwinterEQR','CoastTNsummerEQR','CoastTNwinterEQR','CoastDIPwinterEQR','CoastTPsummerEQR','CoastTPwinterEQR','CoastOxygen','CoastHypoxicArea','CoastBottomOxygen')
wb<-"SE613500-172500"

dbpath<-"../efs/ekostat/ekostat2.db"
periodlist<-c("2013-2018")

obsparams <- data.frame(Indicator=indicatorList,stringsAsFactors=F) %>%
  left_join(select(dfind,Indicator,Parameter),by="Indicator") %>%
  distinct(Parameter)

obsColumnList <- c("station","date","year","month","period","sali","depth","station_depth",obsparams$Parameter)

# ------------ get data --------------------

dfind<-ReadIndicatorType()
dfAvg <- getres(dbpath,"resAvg",wb,periodlist)
dfMC <- getres(dbpath,"resMC",wb,periodlist)
dfObs <- getobs(dbpath,"data",wb,periodlist)
dfObs <- dfObs[,obsColumnList]

df<-report(dfAvg,dfMC,dfObs)

