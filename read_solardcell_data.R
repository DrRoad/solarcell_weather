#load solarzellen data
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
source("summary_se.R")

dir_name = "solarzellen_data"
solarcell_files_names = list.files(path = dir_name, pattern="/*.csv")
solarcell_files_names<- paste(dir_name, solarcell_files_names, sep = "/")
solarcell_tbl <- data_frame("datum"=character(0), "zaehlerstand_morgens"= numeric(0), "zaehlerstand_abends" = numeric(0),"produktion"= numeric(0),"eigen_verbrauch"= numeric(0),"eingespeist"= numeric(0))

solarcell_tbl = lapply(solarcell_files_names, read_csv2) %>% bind_rows()
#using lubridate to make the Datum column more usefull
solarcell_tbl$Datum <- dmy(solarcell_tbl$Datum)
solarcell_tbl <- summarySE(solarcell_tbl, measurevar="Produktion", groupvars=c("YEAR","MONTH"))


y <- solarcell_tbl$Produktion
t <- 1:2167
x <- solarcell_tbl$Datum

# sinus fit 
#1
ssp <- spectrum(y) 
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t))
summary(reslm)

rg <- diff(range(y))
plot(y~t,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
lines(fitted(reslm)~t,col=4,lty=1)   # dashed blue line is sin fit

# including 2nd harmonic really improves the fit
reslm2 <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
summary(reslm2)
lines(fitted(reslm2)~t,col=3) 

#2 fft
raw.fft = fft(y)

ggplot(solarcell_tbl, aes(x = Datum, y = Produktion)) + 
  geom_point(size=2, shape=23) + 
  geom_smooth(linetype="dashed")

ggplot(data = solarcell_tbl, aes(x = Datum, y = Produktion)) + 
  geom_point() + 
  geom_smooth(se=FALSE, method="lm", formula = y ~ sin(2*pi/per*x)+cos(2*pi/per*x))

#including second harmonic
ggplot(data = solarcell_tbl, aes(x = Datum, y = Produktion)) + 
  geom_point() + 
  geom_smooth(se=FALSE, method="lm", formula = y ~ sin(2*pi/per*x)+cos(2*pi/per*x)+sin(4*pi/per*x)+cos(4*pi/per*x))

#manage the dates
solarcell_tbl$YEAR <- year(solarcell_tbl$Datum)
solarcell_tbl$MONTH <- month(solarcell_tbl$Datum)
solarcell_tbl$DAY <- day(solarcell_tbl$Datum)

ggplot(data = solarcell_tbl, aes(x = MONTH, y = Produktion )) + 
  geom_point() +
  facet_wrap(~YEAR) +
  geom_smooth(se=FALSE, method="lm", formula = y ~ sin(2*pi/per*x)+cos(2*pi/per*x)+sin(4*pi/per*x)+cos(4*pi/per*x))




ggplot(data = solarcell_tbl, aes(x = MONTH, y = Produktion )) + 
  geom_bar(aes(fill=YEAR), position=position_dodge(.9), stat="identity") +
  facet_wrap(~YEAR) +
  geom_errorbar(aes(ymin=Produktion-se, ymax=Produktion+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

# monthly plot
nr <- solarcell_tbl %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(solarcell_tbl$Produktion) %>% nrow()
solar_monthly <- data.frame(matrix(NA, nrow = nr, ncol = 1))
solar_monthly <- solarcell_tbl %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(Produktion)

x <-1:nrow(solar_monthly)
ggplot(solar_monthly, aes(x=x, y=solar_monthly$Produktion)) + geom_bar(stat = "identity", width = 0.5)

#summary over for months
solarcell_tbl %>% 
  group_by( MONTH) %>% 
  summarise_at(vars(Produktion), funs(mean,sd))

#summary over for year
solarcell_tbl %>% 
  group_by(YEAR) %>% 
  summarise_at(vars(Produktion), funs(mean,sd))

# for(file in  solarcell_files_names) {
#   tmp <- read_csv2(file)
#   bind_rows(solarcell_tbl,tmp) 
# }
# 
# tmp <-read_csv2(solarcell_files_names[4], skip=1)
# rbind(solarcell_tbl, tmp)
# solarcell_files_names[4]


# Step 2: drop anything past the N/2 - 1th element.
# This has something to do with the Nyquist-shannon limit, I believe
# (https://en.wikipedia.org/wiki/Nyquist%E2%80%93Shannon_sampling_theorem)
truncated.fft = raw.fft[seq(1, length(y)/2 - 1)]

# Step 3: drop the first element. It doesn't contain frequency information.
truncated.fft[1] = 0

# Step 4: the importance of each frequency corresponds to the absolute value of the FFT.
# The 2, pi, and length(y) ensure that omega is on the correct scale relative to t.
# Here, I set omega based on the largest value using which.max().
omega = which.max(abs(truncated.fft)) * 2 * pi / length(y)

#3 tidyverse
tt <- solarcell_tbl %>% 
  group_by(YEAR, MONTH) %>% 
  summarise(Produktion)
str(tt)
x <-1:nrow(tt)
ggplot(tt, aes(x=x, y=P)) + geom_bar(stat = "identity", width = 0.5)
