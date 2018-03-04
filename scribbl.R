tttf <- function(data, varname) {
  return(data[varname])
}

tttf2 <- function(data, varname) {
  return(`$`(tt, as.name(vorname[[1]])))
}

tttf2(solarcell_tbl, 'Produktion')

`$`(tt, "Produktion")
tt


kwh <- c(41,41, 23, 18,23,8,8,8, 41,8,41,23,23)
anzahl <- length(kwh)
stabile_wetterlage <- vector(mode="numeric", length=anzahl)
stabile_wetterlage_df <- data_frame(produktion=as.numeric(), anzahl = as.numeric() )
tmp <- 0
counter <- 1
wl_index <- 1

tmp <- kwh[1]
for(i in 2:anzahl) {
 
  if(kwh[i] == tmp){
    counter <- counter+1
    
  } else {
    print(paste0("Leistung in kwh: ", tmp))  
    print(paste0("zählerstand: ", counter) )
    print(paste0("index: ", wl_index) )
    # haeufigkeit der stabilen wetterlage um eins erhoeht
    stabile_wetterlage[wl_index] <- counter
    stabile_wetterlage_df[nrow(stabile_wetterlage_df) + 1,] = c(as.factor(tmp), counter)
    
    counter <- 1
    wl_index <- wl_index+1
  }
  tmp <- kwh[i]
}
#don't forget the last entry
stabile_wetterlage_df[nrow(stabile_wetterlage_df) + 1,] = c(tmp, counter)
ggplot(data = stabile_wetterlage_df, aes(x=anzahl, fill=produktion) + geom_histogram(position="identity", binwidth=.5)

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
aa <- ""
tmp_wetterlage <- ""

#list all factors in a column
#levels(factor(df$col)) 
#unique(df$col)
#names(table(df$col))


for(i in 1:anzahl) {
  aa <- akt_wetterlage[i]
  if( aa == tmp_wetterlage){
    counter <- counter+1
  } else {
    #wetterlage hat sich geaendert, ergebnis im entsprechenden vector speichern, und zaehler zuruecksetzen
    wetterlage_decider()
    #zaehler wieder auf 1 setzen
    counter <- 1
  }
  tmp_wetterlage <- aa
}

wetterlage_decider <-function() {
  if(tmp_wetterlage == "schoen"){
    #write upstream with the superassignment operator 
    tage_schoen <<- c(tage_schoen, counter)
    schoen <<- c(schoen, "schoen")
  } else if (tmp_wetterlage == "durchschnitt"){
    tage_durchschnitt <<- c(tage_durchschnitt, counter)
    durchschnitt <<- c(durchschnitt, "durchschnitt")
  } else {
    tage_schlecht <<- c(tage_schlecht, counter)
    schlecht <<- c(schlecht, "schlecht") 
  }
}

wetterlage_decider <-function(akt_wetterlage, counter, wetterlage_df ) {
  
  stopifnot(is.factor(akt_wetterlage))
  stopifnot(is.numeric(counter))
  stopifnot(is.data.frame(wetterlage_df))
  
  
  if(tmp_wetterlage == "schoen"){
    #write upstream with the superassignment operator 
    tage_schoen <<- c(tage_schoen, counter)
    schoen <<- c(schoen, "schoen")
  } else if (tmp_wetterlage == "durchschnitt"){
    tage_durchschnitt <<- c(tage_durchschnitt, counter)
    durchschnitt <<- c(durchschnitt, "durchschnitt")
  } else {
    tage_schlecht <<- c(tage_schlecht, counter)
    schlecht <<- c(schlecht, "schlecht") 
  }
}


updateWetterlage() <-function(akt_wetterlage_name, akt_wetterlage_value, counter_column_name, counter_value, data_df) {
  
  tmp_df <- data_df %>% 
    filter(data[akt_wetterlage_name] == akt_wetterlage_value ) %>% 
    filter(data[counter_column_name] = counter_value)
  tmp_df[,3] == tmp_df+1
  
  data_df %>% 
    add_row(akt_wetterlage_name = akt_wetterlage_value, counter_column_name = counter_value, data_df[,3] = 1)
  
}

findWetterlage <-function(akt_wetterlage_name, akt_wetterlage_value, counter_column_name, counter_value, data ) {
  
  data %>% 
    filter(data[akt_wetterlage_name] == akt_wetterlage_value ) %>% 
    filter(data[counter_column_name] = counter_value)
}



test_1 <- solarcell_tbl %>% filter(YEAR==2017 & MONTH == 1  & DAY == 1)

solarcell_tbl <- mutate(solarcell_tbl, Produktion = ifelse(YEAR==2017 & MONTH == 1  & DAY == 1, 200, Produktion))

solarcell_tbl %>% filter(YEAR==2017 & MONTH == 1  & DAY == 1)





#########
levels(atmospheric_conditions_df$atm_condition)
atmospheric_conditions_df$atm_condition <- factor(atmospheric_conditions_df$atm_condition)
ggplot(data = atmospheric_conditions_df, aes(x=duration, fill = atm_condition)) + geom_histogram(position = "identity")
