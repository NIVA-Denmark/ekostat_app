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

shinyInput = function(FUN, len, id, labels,...) {
  inputs = character(len)
  if(typeof(labels)=="character"){
    labels<-rep(labels, len)
  }
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN(paste0(id, i),label = labels[i], ...))# NULL,
  }
  inputs
}
buttonInput = function(FUN, len, id, labels, ...) {
  inputs = character(len)
  if(typeof(labels)=="character"){
    labels<-rep(labels, len)
  }
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN( inputId=paste0(id,i), label=labels[i], icon=NULL,width=NULL, ...)) #paste0(id, i),
  }
  inputs
}

# obtain the values of inputs
shinyValue = function(id, len) {
  unlist(lapply(seq_len(len), function(i) {
    value = input[[paste0(id, i)]]
    if (is.null(value)) NA else value
  }))
}

