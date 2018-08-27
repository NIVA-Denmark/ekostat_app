library(tidyr)
library(RSQLite)
library(dplyr)


source("ReadIndicatorType.R")
source("classoutputtable.R")
dfind<-ReadIndicatorType()

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
values<-list()

dbpath<-"data/ekostat.db"
values$wbselected<-"SE580325-113500"
values$typeselected<-"1n"

Choices<-c("CoastChlaEQR",
           "CoastBiovolEQR",
           "CoastTNsummerEQR",
           "CoastTNwinterEQR",
           "CoastTPsummerEQR",
           "CoastTPwinterEQR",
           "CoastDINwinterEQR",
           "CoastDIPwinterEQR",
           "CoastSecchiEQR",
           "CoastBQI",
           "CoastMSMDI",
           "CoastOxygen")

period<-c("2004-2009","2010-2015")

db <- dbConnect(SQLite(), dbname=dbpath)
sql<-paste0("SELECT * FROM resAvg WHERE WB ='",values$wbselected,"'")
df <- dbGetQuery(db, sql)
sql<-paste0("SELECT * FROM resAvg WHERE Type ='",values$typeselected,"'")
dftype <- dbGetQuery(db, sql)
dbDisconnect(db)
df <- df %>% select(Indicator,Period,Code) #IndSubtype,
df2 <- data.frame(Choices,stringsAsFactors=F) 
df2$X<-1
dfperiod<-data.frame(period,stringsAsFactors=F)
dfperiod$X<-1
df2<-df2 %>% left_join(dfperiod,by="X") %>% select(-X)
names(df2) = c("Indicator","Period")

df <- df2 %>% left_join(df,by=c("Indicator","Period")) %>%
  mutate(Code=ifelse(is.na(Code),-99,Code)) %>%
  spread(key="Period",value="Code")

values$df_ind_status <- df
values$df_ind_type <-dftype


db <- dbConnect(SQLite(), dbname=dbpath)
sql<-paste0("SELECT * FROM resAvg WHERE WB in ('SE584870-174310','SE580325-113500')")
df <- dbGetQuery(db, sql)
dbDisconnect(db)
# 
# oksubtypes <- df %>% distinct(Indicator,IndSubtype,Code) %>% 
#   filter(Code>-10,IndSubtype!="")
# 
# subtypes <- df %>% distinct(Indicator,IndSubtype) %>%
#   filter(IndSubtype!="")
# 
# subtypes <- subtypes %>% 
#   left_join(oksubtypes,by=c("Indicator","IndSubtype")) %>%
#   filter(!is.na(Code)) %>%
#   select(-Code)
#   

CleanSubTypes <- function(df){
  oksubtypes <- df %>% distinct(WB,Indicator,IndSubtype,Code) %>% 
    filter(Code>-10,IndSubtype!="")
  
  subtypes <- df %>% distinct(WB,Indicator,IndSubtype) %>%
    filter(IndSubtype!="")
  
  subtypes <- subtypes %>% 
    left_join(oksubtypes,by=c("WB","Indicator","IndSubtype")) %>%
    filter(is.na(Code)) %>%
    select(-Code) %>%
    mutate(drop=1)

  df <- df %>% left_join(subtypes,by=c("WB","Indicator","IndSubtype")) %>%
    filter(is.na(drop)) %>%
    select(-drop)
  return(df)
}

df2<-CleanSubTypes(df)
