library(tidyverse)
library(RSQLite)

source("IndicatorFunctions.R")
source("helpFunctions.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")

#---------------------------------------------------------------------------------
dbpath<-"data/ekostat.db"
values <- list()
values$wbselected<-"SE584695-175315"
values$watertypeselected<-"Coastal"
values$typeselected <- "12n"
#---------------------------------------------------------------------------------
readdb <- function(dbname,strSQL){
  db <- dbConnect(SQLite(), dbname=dbname)
  df <- dbGetQuery(db, strSQL)
  dbDisconnect(db)
  return(df)
} 
#---------------------------------------------------------------------------------
dfwb_lan <- readdb(dbpath, "SELECT * FROM WB_Lan") %>%
  mutate(TypeName = Type) %>%
  mutate(Type = substr(Type,1,3)) %>%
  mutate(Type = gsub(" ","",Type)) %>%
  mutate(Type = gsub("\\.","",Type)) %>%
  mutate(Type = gsub(":","",Type)) %>%
  mutate(TypeNum = gsub("n",".0",Type)) %>%
  mutate(TypeNum = gsub("s",".5",TypeNum))
dfwb_lan$TypeNum <- as.numeric(dfwb_lan$TypeNum)

dfind<-ReadIndicatorType()
dfvar<-ReadVariances()

pressure_list<-function(){
  c("Nutrient loading","Organic loading","Acidification","Harmful substances","Hydrological changes","Morphological changes","General pressure")
}
#---------------------------------------------------------------------------------
# exisiting code in UpdateIndTable()
# updates the list of WBs having the same type as the selected WB
sql<-paste0("SELECT * FROM resAvg WHERE Type ='",values$typeselected,"'")
dftype <- readdb(dbpath, sql)
dftype<-CleanSubTypes(dftype)

pname<-pressure_list()[1]
pname<-gsub(" ",".",pname)
df2 <- dfind %>% filter(Water_type==values$watertypeselected)
df2 <- df2[df2[,pname]=="X",]
df2<-df2 %>% select(Indicator)

dftype <- dftype %>% left_join(select(dfwb_lan,WB_ID,Name),by=c("WB"="WB_ID")) %>%
  filter(Code==0) 
dftype<- df2 %>% left_join(dftype,by="Indicator")

#---------------------------------------------------------------------------------

ind<-"CoastSecchiEQR"

WB_type_list<-dftype %>%
  filter(Indicator==ind) %>%
  distinct(WB)

# Get list of variance components from water type (e.g. Coastal) and waterbody type (e.g. 12n)
indvar<-dfind %>% 
  filter(Indicator==ind) %>%
  select(Measurement) %>% 
  left_join(dfvar,by="Measurement") %>%
  filter(Water_type==values$watertypeselected,Type==values$typeselected) %>%
  select(V_station,V_obspoint,V_year,V_yearmonth,V_stationdate,V_stationyear,V_stationmonth,V_institution,V_replication) %>%
  lapply(function(x) ifelse (is.na(x),0,x))




load("../ekostat_calc/from_JAC/20180508/test_cm/un.Rda")

# Testing indicator calculation for non-monitored water bodies
variance_list <- list(V_station=0.0909487321117623,V_obspoint=0,
                      V_year=0.001168302,V_yearmonth=0,
                      V_stationdate=0.278888006,
                      V_stationyear=0,V_stationmonth=0.092962407,
                      V_institution=0.062103034,V_replication=0)
MonthInclude <- c(11,12,1,2)
RefCond_sali <- c(rep(0.9,36))

CalculateIndicatorType("CoastTNwinter",un,list(V_WBperiod=9,V_WBannual=12),30,2007,2012)

