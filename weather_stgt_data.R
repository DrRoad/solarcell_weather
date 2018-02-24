#read stgt weather data

library(stringr)
library(readr)
library(dplyr)
dir_name = "weather_data/ECA_blended_custom-2"
weather_data_files_names <- list.files(path = dir_name, pattern = "*_STAID004671.txt")
weather_data_files_names <- paste(dir_name, weather_data_files_names, sep = "/")

weather_data_tbl <- lapply(weather_data_files_names, function(i){
  read_csv(i, skip=20, col_names = FALSE)
}) %>% bind_rows()
colnames(weather_data_tbl) <-c("squid", "datum", "value", "quality_code")

weather_data_tbl$squid <- as.character(weather_data_tbl$squid)
weather_data_tbl$value <- as.numeric(weather_data_tbl$value)
weather_data_tbl <- weather_data_tbl %>% mutate(squid = str_replace_all(squid, "129140","sunduration" ))
weather_data_tbl <- weather_data_tbl %>% mutate(squid = str_replace_all(squid, "129139","cloudcover" ))
weather_data_tbl <- weather_data_tbl %>% mutate(squid = str_replace_all(squid, "129148","windspeed" ))
weather_data_tbl <- weather_data_tbl %>% mutate(squid = str_replace_all(squid, "129147","humidity" ))
weather_data_tbl <- weather_data_tbl %>% mutate(squid = str_replace_all(squid, "129142","precipation" ))
weather_data_tbl <- weather_data_tbl %>% mutate(squid = str_replace_all(squid, "129143","snowdepth" ))
weather_data_tbl <- weather_data_tbl %>% mutate(squid = str_replace_all(squid, "129144","mean_temp" ))
weather_data_tbl <- weather_data_tbl %>% mutate(squid = str_replace_all(squid, "129145","min_temp" ))
weather_data_tbl <- weather_data_tbl %>% mutate(squid = str_replace_all(squid, "100208","max_temp" ))

weather_data_tbl2 <- weather_data_tbl %>% filter(value != -9999)


#weather_data_tbl$squid <- within(weather_data_tbl, squid=="129140" <- "sunduration")
# sub('.*\\/', '', weather_data_files_names[1])
# 
# solarcell_tbl <- data_frame()
# for(file in  weather_data_files_names) {
#   
#   tmp <- read_csv(file, skip = 19)
#   bind_rows(solarcell_tbl,tmp) 
# }
# ttt <- sub('.*\\/', '', weather_data_files_names[1])
# ttt <- read_csv(weather_data_files_names[1], skip = 19)
