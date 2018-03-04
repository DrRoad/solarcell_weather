wetter <- function(leistung){
  if(leistung >= 35){
    return("fine")
  }else if ( leistung >= 15){
    return("middling")
  }else{
    return("bad")
  }
}

#new atmospheric condition analyzer
atm_condition_fn <- function(data, rating_colum ) {
  #a couple of temp variables
  counter <- 1
  rating <- data[rating_colum]
  length_of_data <- nrow(rating)
  tmp <- rating[1,]
  atmospheric_conditions_df <- data_frame(atm_condition = character(), duration = numeric())
  for(i in 2:length_of_data) {
    if(rating[i,] == tmp){
      counter <- counter+1
    } else {
      # add a new row
     atmospheric_conditions_df[nrow(atmospheric_conditions_df) + 1,] = c(tmp, counter)
      counter <- 1
    }
    tmp <- rating[i,]
  }
  #don't forget the last entry
  atmospheric_conditions_df[nrow(atmospheric_conditions_df) + 1,] = c(tmp, counter)
  return(atmospheric_conditions_df)
}


######################################## old stuff #######################################
#returns a list with attributes anzahl tage schoen, schlecht and durchschnitt
prepare4weathersituationHist <-function(wetter_solar_prod) {
  
  tmp <- 0
  counter <- 0
  tmp_wetterlage <-wetter_solar_prod$wetterlage[1]
  akt_wetterlage <- wetter_solar_prod$wetterlage
  
  anzahl <- length(akt_wetterlage)
  tage_schoen <- numeric(0)
  tage_durchschnitt <- numeric(0)
  tage_schlecht <- numeric(0)
  aa <- ""
  
  for(i in 1:anzahl) {
    aa <- akt_wetterlage[i]
    if( aa == tmp_wetterlage){
      counter <- counter+1
    } else {
      #wetterlage hat sich geaendert, ergebnis im entsprechenden vector speichern, und zaehler zuruecksetzen
      if(tmp_wetterlage == "schoen"){
        tage_schoen <- c(tage_schoen, counter)
      } else if (tmp_wetterlage == "durchschnitt"){
        tage_durchschnitt <- c(tage_durchschnitt, counter)
      } else {
        tage_schlecht <- c(tage_schlecht, counter)
      }
      #zaehler wieder auf 1 setzen
      counter <- 1
    }
    tmp_wetterlage <- aa
  }# end iteration over length wetter_solar_prod$wetterlage
  # write the last value
  if(tmp_wetterlage == "schoen"){
    tage_schoen <- c(tage_schoen, counter)
  } else if (tmp_wetterlage == "durchschnitt"){
    tage_durchschnitt <- c(tage_durchschnitt, counter)
  } else {
    tage_schlecht <- c(tage_schlecht, counter)
  }
  
  weathersituationHist <- list(tageschoen = tage_schoen, 
                               tagedurchschnitt = tage_durchschnitt, 
                               tageschlecht=tage_schlecht)
  
  return(weathersituationHist)
}

#returns a data.frame with wetterlage as factor and anzahltage as value
prepare4weathersituationHist2 <-function(wetter_solar_prod) {
  
  tmp <- 0
  counter <- 0
  tmp_wetterlage <-wetter_solar_prod$wetterlage[1]
  akt_wetterlage <- wetter_solar_prod$wetterlage
  
  anzahl <- length(akt_wetterlage)
  tage_schoen <- numeric(0)
  tage_durchschnitt <- numeric(0)
  tage_schlecht <- numeric(0)
  aa <- ""
  schoen <- character(0)
  schlecht <- character(0)
  durchschnitt <- character(0)
  
  
  wetterlage_decider <-function(){
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
  }# end iteration over length wetter_solar_prod$wetterlage
  # write the last value
  wetterlage_decider()
  #combine vectors
  wetterlage <- c(schoen, durchschnitt, schlecht)
  anzahltage <- c(tage_schoen, tage_durchschnitt, tage_schlecht)
  df <- data.frame(wetterlage, anzahltage)
  
  return(df)
}


#(40 being the number of rows you want in your sample).
#smallerDF<-randomSample(bigDF, 40)
randomSample = function(df,n) { 
  return (df[sample(nrow(df), n, replace = FALSE),])
}

library(RColorBrewer)
myColours <- brewer.pal(6,"Blues")
my.settings <- list(
     superpose.polygon=list(col=myColours[2:5], border="transparent"),
     strip.background=list(col=myColours[4]),
     strip.border=list(col="black")
)

make_hist <- function(df_wetterlage, title){
  histogram(~anzahltage | wetterlage, 
            data = df_wetterlage, 
            col = "black", 
            main = title,
            scales = list(x = list(rot = 45)),
            layout = c(3,1),
            par.settings = my.settings,
            par.strip.text=list(col="white", font=2),
            panel = function(...) {
              panel.grid(h = 16, v = 0)
              panel.histogram(...)
            }
  )
}