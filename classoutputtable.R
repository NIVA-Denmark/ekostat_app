


# ClassOutputTableDT
#
#
ClassOutputTableDT<-function(df,ClassVar="Class",Groups="",
                             ymin=0,ymax=0,
                             barwidthpx=c("20px","20px","20px","20px","20px","0px"),barcolours=c('#FF2B00','#FF8066','#FFD5CC','#99FF66','#33AA00',"#FFFFFF"),
                             remove="",roundlist=NULL,colOK=0,
                             sDOM="t"){
  
  
  if(is.data.frame(df)){
    return(DT::renderDataTable({
      
      df[,"X"] <- df[,ClassVar]
      df$X <- factor(df$X,levels=c("Bad","Poor","Mod","Good","High"))
      
      GroupsClass<-c(Groups,"X")
      df.count<- df %>% 
        group_by_(.dots=GroupsClass) %>% 
        summarize(n=n()) %>% mutate(f = n / sum(n)) %>% 
        complete(X, fill = list(f = 0))
      df.count$f[is.na(df.count$X)]<-0
      
      dt<-df.count %>% 
        group_by_(.dots=Groups) %>% 
        summarize(Classes = spk_chr(f,type='bar',barWidth=barwidthpx,chartRangeMin=ymin, chartRangeMax=ymax,
                                    colorMap=barcolours)) %>% 
        ungroup() %>%
        select(-one_of(remove)) %>%
        datatable(escape = F,rownames = F,selection = 'single',
                  options = list(dom=sDOM,fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}')))
      if(!is.null(roundlist)){
        dt<-dt %>% formatRound(columns=roundlist, digits=3)
      }
      
      
      if(colOK){
        dt<-dt %>% # Style cells with max_val vector
          formatStyle(
            columns = colOK,
            #backgroundColor = styleEqual(c(0, -1,-90,-91), c('', 'yellow', 'yellow', 'yellow'))
            backgroundColor = styleEqual(c("OK","data<3yrs","missing","missing"), c('', 'yellow', 'red', 'red'))
          )
      }
      dt<-dt %>% spk_add_deps()
      dt            
    }))
  }else{
    return(DT::renderDataTable({
      dt<-data.frame() %>% 
        datatable()
    }))
  }
  
}


# ClearErrorValues
#
#
ClearErrorValues <- function(df,checkvar="Code",OKvalue=0,varList=c("EQR","Class")){
  for(var in varList){
    df[df[,checkvar]!=0,var]<-NA
  }
  return(df)
}


# ClassObsTableDT
#
#
ClassObsTableDT<-function(df,roundlist=NULL,sDOM="t"){
  if(is.data.frame(df)){
    return(DT::renderDataTable({
      dt<-df %>% datatable(escape = F,rownames = F,selection = 'none',
                           options = list(dom=sDOM))
      if(!is.null(roundlist)){
        dt<-dt %>% formatRound(columns=roundlist, digits=3)
      }
      dt
    })
    )
  } else{
    return(DT::renderDataTable({
      dt<-data.frame() %>% 
        datatable()
    }))
  }
  
}


# SelectObs
#
#
#filter observation data based on the selected indicator, WB and period
SelectObs<-function(df,indicator,sWB,sPeriod){
  varlist<-GetVarNames(indicator)
  obsvar<-varlist[length(varlist)]

  varlist<-c("station","obspoint","date","year","month",varlist)

  df <- df %>% filter(WB==sWB,period==sPeriod)
  df <- df[!is.na(df[,obsvar]),]
  df <- df[,varlist]
  
  return(df)
  
}

# GetVarNames
#
#

GetVarNames<-function(indicator){
  if(indicator!=""){
    
    df.indicators<-ReadIndicatorType()
    df.indicators<-df.indicators %>% filter(Indicator==indicator)
    obsvar<-as.character(df.indicators[1,"Parameter"])
    if(indicator=="Oxygen"){
      varlist<-c("depth","sali",obsvar)
    }else{
      varlist<-c("sali",obsvar)
    } 
  }else{
    varlist<-""
  }
  return(varlist)
}


