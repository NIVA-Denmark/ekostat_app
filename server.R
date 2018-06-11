library(shiny)
library(DT)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(prodlim)
library(sparkline)
library(RSQLite)

source("ReadIndicatorType.R")
source("classoutputtable.R")
source("Aggregation.R")

shinyServer(function(input, output, session) {
  dfind<-ReadIndicatorType()
  
  db <- dbConnect(SQLite(), dbname="data/ekostat.db")
  wb <- dbGetQuery(db, "SELECT * FROM WB")
  wb <- wb %>% mutate(DistrictID = paste0(Type," ",Typename))
  DistrictList<-c("1s Västkustens inre kustvatten","1n Västkustens inre kustvatten","2 Västkustens fjordar","3 Skagerak, Västkustens yttre kustvatten","4 Kattegat, Västkustens yttre kustvatten","5 Södra Hallands och norra Öresunds kustvatten","6 Öresunds kustvatten","7 Skånes kustvatten","8 Blekinge skärgårds och Kalmarsunds inre kustvatten","9 Blekinge skärgård, och Kalmarsunds yttre kustvatten","10 Östra Ölands, sydöstra Gotlands kustvatten samt Gotska sandön","11 Gotlands västra och norra kustvatten","12n Östergötlands samt Stockholms skärgård, mellankustvatten","12s Östergötlands samt Stockholms skärgård, mellankustvatten","13 Östergötlands inre skärgård","14 Östergötlands, yttre kustvatten","15 Stockholms skärgård, yttre kustvatten","16 Södra Bottenhavet, inre kustvatten","17 Södra Bottenhavet, yttre kustvatten","18 Norra Bottenhavet, Höga kustens inre kustvatten","19 Norra Bottenhavet, Höga kustens yttre kustvatten","20 Norra Kvarkens inre kustvatten","21 Norra Kvarkens yttre kustvatten","22 Bottenviken, inre kustvatten","23 Bottenviken, yttre kustvatten","24 Stockholms inre skärgård og Hallsfjärden","25 Göta Älvs- och Nordre Älvs estuarie")
  wb$DistrictID<-factor(wb$DistrictID, levels=DistrictList)
  dbDisconnect(db)
  
  values <- reactiveValues(resMC = data.frame())
  
  district_list <- reactive({
    sort(unique(wb$DistrictID))
  })
  
  output$selectWaterDistrict <- renderUI({
    tagList(
      selectInput(
        "district",
        "Select Waterbody Type",
        choices = district_list(),
        selected = ""
      )
    )
  })  
  
  waterbody_list <- reactive({
    dfwb <- filter(wb, DistrictID %in% input$district)
    res <- sort(unique(dfwb$WB))
    return(res)
  })
  
  period_list <- reactive({
    #period<-c("2004-2009","2010-2015","2016-2021")
    period<-c("2004-2009","2010-2015")
    res <- period 
    return(res)
  })
  
  
  output$selectPeriod <- renderUI({
    tagList(selectInput(
      "period",
      "Select Period(s)",
      choices = period_list(),
      multiple = TRUE
    ))
  })
  
  output$selectWaterBodies <- renderUI({
    tagList(
      selectInput(
        "waterbody",
        "Select Waterbody(s)",
        choices = waterbody_list(),
        selected = "ALL",
        multiple = TRUE
      )
    )
  })  
  
  output$dataButton <- renderUI({
    if (datacount() > 0) {
      buttontext <-"Get data"
      tagList(actionButton("dataButton", buttontext))
    }
  })  
  
  datacount <- reactive({
    
    db <- dbConnect(SQLite(), dbname="data/ekostat.db")
    periodlist<-paste(paste0("'",input$period,"'"),collapse = ",")
    wblist<-paste(paste0("'",input$waterbody,"'"),collapse = ",")
    
    sql<-paste0("SELECT COUNT(*) FROM data WHERE period IN (",periodlist,") AND WB IN (",wblist,")")
    nrows <- dbGetQuery(db, sql)
    dbDisconnect(db)
    
    return(nrows)
  })

  #Check box with list of available indicators
  output$chkIndicators <- renderUI({
    if (datacount() > 0) {
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
      
       sList = c("Chlorophyll a (EQR)" = "CoastChlaEQR",
                "Phytoplankton Biovolume (EQR)" = "CoastBiovolEQR",
                "Summer TN (EQR)" = "CoastTNsummerEQR",
                "Winter TN (EQR)" = "CoastTNwinterEQR",
                "Summer TP (EQR)" = "CoastTPsummerEQR",
                "Winter TP (EQR)" = "CoastTPwinterEQR",
                "Winter DIN (EQR)" = "CoastDINwinterEQR",
                "Winter DIP (EQR)" = "CoastDIPwinterEQR",
                "Secchi Depth (EQR)" = "CoastSecchiEQR",
                "Benthic Quality Index (BQI)" = "CoastBQI",
                "Multi Species Maximum Depth Index (MSMDI)" = "CoastMSMDI",
                "Dissolved Oxygen (O2)" = "CoastOxygen")

       
      tagList(checkboxGroupInput("indSelect", "Indicators:",
                                 sList, selected = Choices))
      
    }
  })
  output$goButton <- renderUI({
    if (datacount() > 0) {
      tagList(actionButton("goButton", "Calculate Status"))
    }
  })

  
  # 
  observeEvent(input$dataButton, {
    n <- datacount()
    output$nrows <- renderUI({
      tagList(p(renderText(
        paste0("Selected: ", n, " rows of data.")
      )))
    })
    updateNavbarPage(session, "inTabset", selected = "Assessment")
  })
  
  typeselect <- reactive({
    # until we can clear up the waterbodies with multiple typologies, we need this fix
    sType<-unlist(strsplit(input$district," "))
    #cat(paste0("Type=",sType[[1]],"\n"))
    return(sType[[1]])
  })
  
  df.select <- reactive({
    db <- dbConnect(SQLite(), dbname="data/ekostat.db")
    periodlist<-paste(paste0("'",input$period,"'"),collapse = ",")
    wblist<-paste(paste0("'",input$waterbody,"'"),collapse = ",")
    sql<-paste0("SELECT * FROM data WHERE period IN (",periodlist,") AND WB IN (",wblist,")")
    df <- dbGetQuery(db, sql)
    dbDisconnect(db)
    # until we can clear up the waterbodies with multiple typologies, we need this fix
    df <- df %>% filter(typology==typeselect())
    cat(paste0("df.select nrows=",nrow(df),"\n"))
    
    df$date<-as.Date(df$date,origin="1970-01-01")
    return(df)
  })
  
  
  observeEvent(input$goButton, {
    nSimMC <- input$n
    IndList <- input$indSelect
    
    #Check that at least one indicator has been selected
    if (length(IndList) > 0) {
 
      n <- nrow(df.select())
      
      db <- dbConnect(SQLite(), dbname="data/ekostat.db")
      periodlist<-paste(paste0("'",input$period,"'"),collapse = ",")
      wblist<-paste(paste0("'",input$waterbody,"'"),collapse = ",")
      sql<-paste0("SELECT * FROM resAvg WHERE period IN (",periodlist,") AND WB IN (",wblist,")")
      resAvg <- dbGetQuery(db, sql)
      sql<-paste0("SELECT * FROM resMC WHERE period IN (",periodlist,") AND WB IN (",wblist,") AND sim <= ",nSimMC)
      resMC <- dbGetQuery(db, sql)
      sql<-paste0("SELECT * FROM resErr WHERE period IN (",periodlist,") AND WB IN (",wblist,")")
      resErr <- dbGetQuery(db, sql)
      dbDisconnect(db)
      resAvg <- resAvg %>% filter(Indicator %in% IndList) %>% filter(Type==typeselect())
      resMC <- resMC %>% filter(Indicator %in% IndList) %>% filter(Type==typeselect())
      resErr <- resErr %>% filter(Indicator %in% IndList) %>% filter(Type==typeselect())
      
     
      values$resAvg <- resAvg
      values$resMC <- resMC
      values$resErr <- resErr
      values$res2MC <- ""
      values$res3MC <- ""
      values$res4MC <- ""
      values$resInd <- ""
      values$resObs <- ""
      updateNavbarPage(session, "inTabset", selected = "Results")
    } else{
      #no indicators selected
      showModal(modalDialog(
        title = div(tags$b("No indicators selected", style = "color: red;")),
        "You need to select at least one indicator!"
      ))
    }
    
  })
  
  
  
  RoundColList <-
    c(
      "secchi","temp","sali","chla" ,"biovol","TP","TN","dens_dif","BQI","MSMDI","logitMSMDI","Oxygen"
    )
  
  output$nText <- renderText({
    outText()
  })

  
  observeEvent(input$chkClassBnds, {
    if (nrow(values$resMC) > 0) {
      str(paste0("dfMC updated n=", nrow(values$resMC)))
      if (input$chkClassBnds == TRUE) {
        grplist <- c(
          "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
          "Months","Unit","Note","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
        )
      } else{
        grplist <- c(
          "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator",
          "IndSubtype","Months","Unit","Note","Mean","StdErr","EQR","Class"
        )
      }
      
      df <-
        values$resMC %>% rename(
          EQRMC = EQR,
          ClassMC = Class,
          Class = ClassAvg,
          EQR = EQRavg
        )
      if (!input$IgnoreErr) {
        df <- ClearErrorValues(df, varList = c("EQR", "Class", "ClassMC", "EQRMC"))
      }
      output$resTableMC <- ClassOutputTableDT(
        df,
        Groups = grplist,
        ClassVar = "ClassMC",
        roundlist = c("Mean", "StdErr", "EQR"),
        colOK = 11,
        sDOM = "pl"
      )
    }
  })
  
  
  observeEvent(values$resMC, {
    if (nrow(values$resMC) > 0) {
      str(paste0("dfMC updated n=", nrow(values$resMC)))
      if (input$chkClassBnds == TRUE) {
        grplist <- c(
          "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
          "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
        )
      } else{
        grplist <- c(
          "WB","Type","Period","QEtype","QualityElement","QualitySubelement",
          "Indicator","IndSubtype","Note","Unit","Months","Mean","StdErr","EQR","Class"
        )
      }
      
      df <-
        values$resMC %>% rename(
          EQRMC = EQR,
          ClassMC = Class,
          Class = ClassAvg,
          EQR = EQRavg
        )
      if (!input$IgnoreErr) {
        df <- ClearErrorValues(df, varList = c("EQR", "Class", "ClassMC", "EQRMC"))
      }
      output$resTableMC <- ClassOutputTableDT(
        df,
        Groups = grplist,
        ClassVar = "ClassMC",
        roundlist = c("Mean", "StdErr", "EQR"),
        colOK = 11,
        sDOM = "pl"
      )
      
      resMC <- values$resMC
      resAvg <- values$resAvg
      
      if (!input$IgnoreErr) {
        resMC <-
          ClearErrorValues(resMC, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
        resAvg <- ClearErrorValues(resAvg, varList = c("EQR", "Class"))
      }
      res1MC <-
        Aggregate(
          resMC,
          Groups = c("Region", "WB", "Type", "Typename", "Period", "sim"),
          level = 1
        ) %>% rename(ClassMC = Class)
      res1Avg <-
        Aggregate(
          resAvg,
          Groups = c("Region", "WB", "Type", "Typename", "Period"),
          level = 1
        ) %>%
        select(Region, WB, Type, Typename, Period, Class)
      values$res1MC <- res1MC %>% left_join(res1Avg,by=c("Region", "WB", "Type", "Typename", "Period"))
    }
  })
  
  observeEvent(input$IgnoreErr, {
    if (nrow(values$resMC) > 0) {
      df <-
        values$resMC %>% rename(
          EQRMC = EQR,
          ClassMC = Class,
          Class = ClassAvg,
          EQR = EQRavg
        )
      if (!input$IgnoreErr) {
        df <- ClearErrorValues(df, varList = c("EQR", "Class", "EQRMC", "ClassMC"))
      }
      grplist <-
        c(
          "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype","Note","Unit","Months",
          #"Worst","PB","MP","GM","HG","Ref",
          "Mean","StdErr","EQR","Class"
        )
      output$resTableMC <- ClassOutputTableDT(
        df,
        Groups = grplist,
        ClassVar = "ClassMC",
        roundlist = c("Mean", "StdErr", "EQR"),
        colOK = 9,
        sDOM = "pl"
      )
      resMC <- values$resMC
      resAvg <- values$resAvg
      if (!input$IgnoreErr) {
        resMC <-
          ClearErrorValues(resMC, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
        resAvg <- ClearErrorValues(resAvg, varList = c("EQR", "Class"))
      }
      res1MC <-
        Aggregate(
          resMC,
          Groups = c("Region", "WB", "Type", "Typename", "Period", "sim"),
          level = 1
        ) %>% rename(ClassMC = Class)
      res1Avg <-
        Aggregate(
          resAvg,
          Groups = c("Region", "WB", "Type", "Typename", "Period"),
          level = 1
        ) %>%
        select(Region, WB, Type, Typename, Period, Class)
      cat("left join res1MC2")
      values$res1MC <- res1MC %>% left_join(res1Avg,by=c("Region", "WB", "Type", "Typename", "Period"))
    }
    values$res2MC <- ""
    values$res3MC <- ""
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
    
  })
  
  observeEvent(input$resTable1_rows_selected, {
    df <-
      values$resMC %>% group_by(WB, Period) %>% summarise() %>% ungroup()
    values$sWB <- df$WB[input$resTable1_rows_selected]
    values$sPeriod <- df$Period[input$resTable1_rows_selected]
    df <- filter(values$resMC, WB == values$sWB, Period == values$sPeriod)
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    res2MC <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type", "sim"),
                level = 2) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    df <- filter(values$resAvg, WB == values$sWB, Period == values$sPeriod)
    if (!input$IgnoreErr) {
      df <- ClearErrorValues(df, varList = c("EQR", "Class"))
    }
    res2Avg <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type"),
                level = 2) %>%
      select(WB, Type, Period, QEtype, EQR, Class)
    values$res2MC <- res2MC %>% left_join(res2Avg,by = c("WB", "Period", "Type", "QEtype"))
    values$res3MC <- ""
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  })
  
  observeEvent(input$resTable2_rows_selected, {
    n <- input$resTable2_rows_selected
    df <-
      values$res2MC %>% group_by(QEtype) %>% summarise() %>% ungroup()
    values$sQEtype <- df$QEtype[input$resTable2_rows_selected]
    df <-
      filter(values$resMC,
             WB == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    res3MC <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type", "sim"),
                level = 3) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    df <-
      filter(values$resAvg,
             WB == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)
    if (!input$IgnoreErr) {
      df <- ClearErrorValues(df, varList = c("EQR", "Class"))
    }
    res3Avg <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type"),
                level = 3) %>%
      select(WB, Type, Period, QEtype, QualityElement, EQR, Class)
    values$res3MC <- res3MC %>% left_join(res3Avg,by = c("WB", "Period", "Type", "QualityElement", "QEtype"))
    
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  })
  
  observeEvent(input$resTable3_rows_selected, {
    df <-
      values$res3MC %>% group_by(QualityElement) %>% summarise() %>% ungroup()
    values$sQualityElement <-
      df$QualityElement[input$resTable3_rows_selected]
    df <- filter(
      values$resMC,
      WB == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    res4MC <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type", "sim"),
                level = 4) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    df <- filter(
      values$resAvg,
      WB == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    res4Avg <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type"),
                level = 4) %>%
      select(WB,Type,Period,QEtype,QualityElement,QualitySubelement,EQR,Class)
    values$res4MC <- res4MC %>% left_join(res4Avg,by = c("WB", "Period", "Type", "QualityElement", "QEtype", "QualitySubelement"))
    
    values$resInd <- ""
    values$resObs <- ""
  })
  
  observeEvent(input$resTable4_rows_selected, {
    #n<-input$resTable4_rows_selected
    df <-
      values$res4MC %>% group_by(QualitySubelement) %>% summarise() %>% ungroup()
    values$sQualitySubelement <-
      df$QualitySubelement[input$resTable4_rows_selected]
    
    df <- values$resMC
    if (!input$IgnoreErr) {
      df <-
        ClearErrorValues(df, varList = c("EQR", "Class", "ClassAvg", "EQRavg"))
    }
    
    values$resInd <- filter(
      df,
      WB == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement,
      QualitySubelement == values$sQualitySubelement
    ) %>%
      rename(
        EQRMC = EQR,
        ClassMC = Class,
        Class = ClassAvg,
        EQR = EQRavg
      )
  })
  
  observeEvent(input$resTableInd_rows_selected, {
    df <-
      values$resInd %>% group_by(Indicator) %>% summarise() %>% ungroup()
    values$sIndicator <-
      df$Indicator[input$resTableInd_rows_selected]
    
    cat(paste0("Indicator=",values$sIndicator,"\n"))
    
    df <- SelectObs(
      df.select(),
      indicator = values$sIndicator,
      sWB = values$sWB,
      sPeriod = values$sPeriod
    )
    if(nrow(df)>0){
      values$resObs <- df
    }else{
      values$resObs <- ""
    }
    
  })
  
  output$titleTable1 <- renderText({
    if (is.null(values$res1MC)) {
      "<h3>No results</h3>"
    } else{
      if (typeof(values$res1MC)!="list") {
        "<h3>No results</h3>" # style='color:#FF0000';
      } else{
        "<h3>Overall Results:</h3>"
      }
    }
  })
  
  
  observeEvent(values$res1MC, {
    output$resTable1 <-
      ClassOutputTableDT(
        values$res1MC,
        Groups = c("Region", "WB", "Type", "Typename", "Period", "Class"),
        roundlist = c("pGES"),
        ClassVar = "ClassMC"
      )
    
  })
  
  
  observeEvent(values$res2MC, {
    grplist <- c("WB", "Period", "Type", "QEtype", "EQR", "Class")
    rmlist = c("WB", "Period", "Type")
    
    output$resTable2 <- ClassOutputTableDT(
      values$res2MC,
      Groups = grplist,
      roundlist = c("EQR","pGES"),
      remove = rmlist,
      ClassVar = "ClassMC"
    )
    
    output$titleTable2 <- renderText({
      if (typeof(values$res2MC)!="list") {
        ""
      } else{
        "<h3>Biological/Supporting:</h3>"
      }
    })
    
  })
  
  
  observeEvent(values$res3MC, {
    grplist <-
      c("WB","Period","Type","QEtype","QualityElement","EQR","Class")
    rmlist = c("WB", "Period", "Type", "QEtype")
    output$resTable3 <-
      ClassOutputTableDT(
        values$res3MC,
        roundlist = c("EQR","pGES"),
        Groups = grplist,
        remove = rmlist,
        ClassVar = "ClassMC"
      )
    
    output$titleTable3 <- renderText({
      if (typeof(values$res3MC)!="list") {
        ""
      } else{
        "<h3>QualityElement:</h3>"
      }
    })
  })
  
  observeEvent(values$res4MC, {
    grplist <-
      c(
        "WB","Period","Type","QEtype","QualityElement","QualitySubelement","EQR","Class"
      )
    rmlist = c("WB", "Period", "Type", "QEtype", "QualityElement")
    output$resTable4 <-
      ClassOutputTableDT(
        values$res4MC,
        roundlist = c("EQR","pGES"),
        Groups = grplist,
        remove = rmlist,
        ClassVar = "ClassMC"
      )
    
    output$titleTable4 <- renderText({
      if (typeof(values$res4MC)!="list") {
        ""
      } else{
        "<h3>Subelement:</h3>"
      }
    })
  })
  
  observeEvent(values$resInd, {
    grplist <- c(
      "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator",
      "IndSubtype","Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
    )
    rmlist <- c("WB","Type","Period","QEtype","QualityElement","QualitySubelement")
    
    output$resTableInd <- ClassOutputTableDT(
      values$resInd,
      Groups = grplist,
      ClassVar = "ClassMC",
      roundlist = c("Mean", "StdErr", "EQR","pGES"),
      remove = rmlist,
      colOK = 3
    )
    output$titleTableInd <- renderText({
      if (typeof(values$resInd)!="list") {
        #if (values$resInd == "") {
        ""
      } else{
        "<h3>Indicators:</h3>"
      }
    })
  })
  
  observeEvent(values$resObs, {
    #if (typeof(values$sIndicator)=="list") {
    if (!is.null(values$sIndicator)) {
      vars = GetVarNames(values$sIndicator)
    } else{
      vars <- ""
    }
    
    #cat(paste0("value$resObs [",typeof(values$resObs),"]\n"))
    
    output$resTableObs <-
      ClassObsTableDT(values$resObs, sDOM = "pl", roundlist = vars)
    output$titleTableObs <- renderText({
      if (typeof(values$resObs)!="list") {
        ""
      } else{
        "<h3>Observations:</h3>"
      }
    })
    
    
    output$plotObs <- renderPlot({
      if (typeof(values$resObs)!="list") {
        p <- 0
        #cat("value=",paste0(values$resObs[1]),"\n")
      } else{
        yvar <- vars[length(vars)]
        
        df <- values$resObs
        df$station <- as.factor(df$station)
        
        p <- ggplot(df, aes_string(x = "date", y = yvar, colour="station")) + geom_point(size=2) 
        p <- p + theme_minimal(base_size = 16) + scale_x_date(date_labels= "%d-%m-%Y") + xlab("Date") 

      }
      return(p)
    }, height = 400, width = 600)
    
    
    
  })
  
  
})
