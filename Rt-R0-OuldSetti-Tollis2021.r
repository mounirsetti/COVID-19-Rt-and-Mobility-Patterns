#Mounir Ould Setti & Sylvain Tollis
#mounir.ould.setti@uef.fi

##### 0. Loading the libraries #######

library(readxl)
library(ggplot2)
library(EpiEstim)
library(Epi)
library(epiDisplay)
library(dlnm)
library(tsModel)
library(dplyr)
library(plotly)
library(incidence)
library(tidyr)
library(tsoutliers)
library(strucchange)
library(forecast)
library(cluster)
library(stringr)
library(pracma)
library(car)
library(broom)
library(insight)
library(scales)
library(trend)
library(sjPlot)
library(countrycode)
library(ggpubr)


`%notin%` <- Negate(`%in%`)
oldpar <- par(no.readonly=TRUE)

#### 1. Importing the data ####

#loading the data from John Hopkin's Github
 globalc <- read.csv(
   "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
 ) #If your internet is slow, you can manually download the file, and then use the command below instead - after uncommenting it.

#globalc <- read.csv("time_series_covid19_confirmed_global.csv")

usa.regions <- rbind(read.csv("usa2_infinitesimalStatesRemoved.csv")[,c(1,4,2,3)],
                     read.csv("usa2_PuertoRicoOnly.csv")[,c(1,4,2,3)])
colnames(usa.regions) <- c("country", "date", "cumcases", "newcases")


#Importing the Google mobility data report
mobil <- read.csv(
   "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", na.strings = ""
 ) #If your internet is slow, you can manually download the file, and then use the command below instead - after uncommenting it.

#mobil <- read.csv(
#  "Global_Mobility_Report.csv", na.strings = ""
#)

#### 2. Working up the data ############

mobil2 <- mobil[which(is.na(mobil$sub_region_1)),]
mobil2 <- mobil2[which(is.na(mobil2$metro_area)),]
mobil2 <- mobil2[,c("country_region","date","retail_and_recreation_percent_change_from_baseline",
                    "grocery_and_pharmacy_percent_change_from_baseline",
                    "transit_stations_percent_change_from_baseline",
                    "workplaces_percent_change_from_baseline",
                    "residential_percent_change_from_baseline","parks_percent_change_from_baseline")]
usa.regions.mob <- mobil[which(mobil$country_region=="United States" &
              mobil$sub_region_1 %in% unique(usa.regions$country) &
              is.na(mobil$sub_region_2) & is.na(mobil$metro_area)),
      c("sub_region_1","date","retail_and_recreation_percent_change_from_baseline",
        "grocery_and_pharmacy_percent_change_from_baseline",
        "transit_stations_percent_change_from_baseline",
        "workplaces_percent_change_from_baseline",
        "residential_percent_change_from_baseline","parks_percent_change_from_baseline")]

colnames(mobil2) <- c("country","date","retail","grocery","transit","work","residential","parks")
colnames(usa.regions.mob) <- c("country","date","retail","grocery","transit","work","residential","parks")
usa.regions$country <- paste0("US.",usa.regions$country)
usa.regions.mob$country <- paste0("US.", usa.regions.mob$country)
mobil2 <- rbind(mobil2, usa.regions.mob)
#Checking for duplicate dates
dupdates <- data.frame(country=unique(mobil2$country), uniquedates = NA, totaldates =NA,
                       difference = NA)
for (i in dupdates$country) {
dupdates$uniquedates[dupdates$country==i]  <- length(unique(mobil2$date[mobil2$country==i]))
dupdates$totaldates[dupdates$country==i]  <- length(mobil2$date[mobil2$country==i])
dupdates$difference[dupdates$country==i] <- 
  dupdates$totaldates[dupdates$country==i] - dupdates$uniquedates[dupdates$country==i]
}
dupdates$country[-which(dupdates$difference==0)]

dupdates2 <- data.frame(country=unique(usa.regions$country), uniquedates = NA, totaldates =NA,
                        difference = NA)
for (i in dupdates2$country) {
  dupdates2$uniquedates[dupdates2$country==i]  <- length(unique(usa.regions$date[usa.regions$country==i]))
  dupdates2$totaldates[dupdates2$country==i]  <- length(usa.regions$date[usa.regions$country==i])
  dupdates2$difference[dupdates2$country==i] <- 
    dupdates2$totaldates[dupdates2$country==i] - dupdates2$uniquedates[dupdates2$country==i]
}
dupdates2$country[-which(dupdates2$difference==0)]

#Fixing country names not in the Johns Hopkins db
setdiff(unique(mobil2$country),unique(globalc$Country.Region))

mobil3 <- mobil2[-which(mobil2$country %in% c("Aruba","Hong Kong","RÃ©union")),]
mobil3$country[mobil3$country=="The Bahamas"] <- "Bahamas"
mobil3$country[mobil3$country=="CÃ´te d'Ivoire"] <- "Cote d'Ivoire" 
mobil3$country[mobil3$country=="Cape Verde"] <- "Cabo Verde"  
mobil3$country[mobil3$country=="South Korea"] <- "Korea, South" 
mobil3$country[mobil3$country=="Myanmar (Burma)"] <- "Burma" 
globalc$Country.Region[globalc$Country.Region=="Taiwan*"] <- "Taiwan" 
mobil3$country[mobil3$country=="United States"] <- "US" 
mobil3$country[mobil3$country == "Puerto Rico"] <- "US.Puerto Rico"

#Turning globalc into a longitudinal dataset
globlong <- data.frame("country"=rep(unique(mobil3$country),each=length(unique(mobil3$date))),
                       "date"=colnames(globalc)[29:(28+(length(unique(mobil3$date))))],
                       "cumcases"=NA,"newcases"=NA
)

for (i in 1:nrow(globlong)) {
  globlong$cumcases[i] <- sum(globalc[globalc$Country.Region==globlong$country[i],globlong$date[i]])
}

# Deriving new from cumulative cases
for (n in unique(globlong$country)) {
  for (i in 2:length(globlong$country[globlong$country == n])) {
    globlong$newcases[globlong$country == n][i] <-
      globlong$cumcases[globlong$country == n][i] - globlong$cumcases[globlong$country ==
                                                                        n][i - 1]
  }
  globlong$newcases[globlong$country == n][1] <-
    globlong$newcases[globlong$country == n][2]
}

globlong$date <- as.Date(globlong$date,format=("X%m.%d.%y"))

# Adding US regional case data
for (i in unique(usa.regions$country)) {
  for (k in unique(usa.regions$date[usa.regions$country == i])) {
    globlong[globlong$country == i & globlong$date == k,c(3,4)] <- 
      usa.regions[usa.regions$country == i & usa.regions$date == k, c(3,4)]
  }
}



#Adding new and cum cases count to the mobility database
lmobil3 <- length(mobil3)
mobil3b <- mobil3
for (i in unique(mobil3b$country)) {
  for (k in unique(mobil3b$date)) {
    mobil3b[mobil3b$country==i & mobil3b$date ==k,c(lmobil3+1,lmobil3+2)] <- 
      globlong[globlong$country==i & globlong$date==k,c(3,4)]
    
    
  }
}
mobil4 <- mobil3b

#dealing with negative values of new cases
for (i in 1:nrow(mobil4)) {
  mobil4$newcases[i] <- ifelse(mobil4$newcases[i]<0,
                               abs((mobil4$newcases[i-1]+mobil4$newcases[i+1])/2),
                               mobil4$newcases[i])
}
mobil4$newcases[mobil4$newcases==0] <- 1
mobil4$date <- as.Date(mobil4$date) #converting dates to date format
#Removing observations with 0 or 1 cumulative cases only
mobil4 <- mobil4[mobil4$cumcases>1,]

#### 3. Generating Rt values ############
countries <- list()
for (i in unique(mobil4$country)) {
  #countries[i] <- list(mobil4[mobil4$country==i,])
  countries[[i]] <- mobil4[mobil4$country==i,]
}

#SI from https://jidc.org/index.php/journal/article/view/33839705/2475 : 5.15 (95% CI, 4.73 – 5.57)
#deriving sd (34 studies n=3366 infected-infectee pairs)
sqrt(3366)*(5.57 - 4.73)/3.92
#12.43227
for (i in unique(mobil4$country)) {
  tempRt <-
    estimate_R(countries[[i]]$newcases,
               method = "parametric_si",
               config = make_config(list(mean_si = 5.15,
                                         std_si = 12.43227))
)
  countries[[i]]$R[1] <- tempRt$R[1,3]
  countries[[i]]$sd[1] <- tempRt$R[1,4]
  
  for (k in 1:nrow(tempRt$R)) {
    countries[[i]]$R[tempRt$R[k,1]:tempRt$R[k,2]] <- tempRt$R[k,3]
    countries[[i]]$sd[tempRt$R[k,1]:tempRt$R[k,2]] <- tempRt$R[k,4]
  }
  rm(tempRt)
}


#### 4. Reconciliating the database ############
mobil5 <- as.data.frame(countries[[unique(mobil4$country)[c(1)]]])
for (i in unique(mobil4$country)[-c(1)]) {
  mobil5 <- rbind(mobil5,as.data.frame(countries[[i]]))
}

#excluding these countries
excld <- c("Antigua and Barbuda","Bahamas","North Macedonia","Malta","Guinea-Bissau","Liechtenstein")
mobil5 <- mobil5[which(mobil5$country %notin% excld),]
#Removing dates before 21 February 2020 or after 20 April 2021 and dividing the database into 3

mobil6 <- mobil5[mobil5$date >= "2020-02-20" & mobil5$date <= "2020-07-27",]
mobil7 <- mobil5[mobil5$date >= "2020-07-27" & mobil5$date <= "2020-12-31",]
mobil5 <- mobil5[mobil5$date >= "2020-02-20" & mobil5$date <= "2020-12-31",]


# mobil5 <- na.omit(mobil5)
# mobil6 <- na.omit(mobil6)
# mobil7 <- na.omit(mobil7)
# mobil8 <- na.omit(mobil8)

##removing countries that have less than 60 days of complete observations (days) and observations with 0 cumulative cases
# checklow <- data.frame("country"=unique(na.omit(mobil5)$country),"days"=NA)
# for (i in unique(na.omit(mobil5)$country)) {
#   checklow[checklow$country==i,"days"]  <- length(na.omit(mobil5)[na.omit(mobil5)$country==i,"date"])
# }
# excld <- unique(mobil5$country)[which(unique(mobil5$country) %notin% checklow$country[which(checklow$days>60)])]

#no countries to exclude
##excld <- append(excld,c("Antigua and Barbuda","Bahamas","North Macedonia","Malta"))
# to be excluded are countries with no SI data (from further analysis: idealforecast[is.na(idealforecast$Rsq2),c("country")])


#### 5. Scatter plots #####
r2alag5 <- data.frame("country"=rep(unique(mobil5$country),each=30),"lag"=0:29,"retail"=NA,"grocery"=NA,
                      "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)

for (i in 0:29) {
pdf(file=paste0("Export/ScatterPlots5a/scatt",i,".pdf")) 
par(mfrow = c(3,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))

for (k in unique(mobil5$country)){
  
  mobiltemp <- mobil5[mobil5$country==k,]
  mobiltemp$retail <- lag(mobiltemp$retail,i)
  mobiltemp$grocery <- lag(mobiltemp$grocery,i)
  mobiltemp$transit <- lag(mobiltemp$transit,i)
  mobiltemp$work <- lag(mobiltemp$work,i)
  mobiltemp$residential <- lag(mobiltemp$residential,i)
  mobiltemp$parks <- lag(mobiltemp$parks,i)
  mobiltemp <- na.omit(mobiltemp)
  
  plot(R~retail,data=mobiltemp,pch=20,
       xlab="Retail mobility",ylab="Rt",cex=0.7,col=1)
  lmtemp.retail <- lm(R~retail,data=mobiltemp)
  abline(lmtemp.retail,col="blue")
  mtext(paste0("pearson ",round(with(mobiltemp,cor(R,retail,use="complete.obs")),2),
               "; adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),
               "; coef. ",round(summary(lmtemp.retail)$coefficients[2,1],3),
               "; p ",format.pval(summary(lmtemp.retail)$coefficients[2,4],3)
               ),line=-1,cex=0.7,col="blue")
  r2alag5$retail[r2alag5$country == k & r2alag5$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
    summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
  )
  rm(lmtemp.retail)
  
  plot(R~grocery,data=mobiltemp,pch=20,
       xlab="Grocery mobility",ylab="Rt",cex=0.7,col=2)
  lmtemp.grocery <- lm(R~grocery,data=mobiltemp)
  abline(lmtemp.grocery,col="blue")
  mtext(paste0("pearson ",round(with(mobiltemp,cor(R,grocery,use="complete.obs")),2),
               "; adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),
               "; coef. ",round(summary(lmtemp.grocery)$coefficients[2,1],3),
               "; p ",format.pval(summary(lmtemp.grocery)$coefficients[2,4],3)
  ),line=-1,cex=0.7,col="blue")
  r2alag5$grocery[r2alag5$country == k & r2alag5$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
    summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
  )
  rm(lmtemp.grocery)
  
  plot(R~transit,data=mobiltemp,pch=20,
       xlab="Transit mobility",ylab="Rt",cex=0.7,col=3)
  lmtemp.transit <- lm(R~transit,data=mobiltemp)
  abline(lmtemp.transit,col="blue")
  mtext(paste0("pearson ",round(with(mobiltemp,cor(R,transit,use="complete.obs")),2),
               "; adj.rsq ",round(summary(lmtemp.transit)$adj.r.squared,2),
               "; coef. ",round(summary(lmtemp.transit)$coefficients[2,1],3),
               "; p ",format.pval(summary(lmtemp.transit)$coefficients[2,4],3)
  ),line=-1,cex=0.7,col="blue")
  r2alag5$transit[r2alag5$country == k & r2alag5$lag==i] <- summary(lmtemp.transit)$adj.r.squared*ifelse(
    summary(lmtemp.transit)$coefficients[2,1]>0,1,-1
  )
  rm(lmtemp.transit)
  
  plot(R~work,data=mobiltemp,pch=20,
       xlab="Work mobility",ylab="Rt",cex=0.7,col=4)
  lmtemp.work <- lm(R~work,data=mobiltemp)
  abline(lmtemp.work,col="blue")
  mtext(paste0("pearson ",round(with(mobiltemp,cor(R,work,use="complete.obs")),2),
               "; adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),
               "; coef. ",round(summary(lmtemp.work)$coefficients[2,1],3),
               "; p ",format.pval(summary(lmtemp.work)$coefficients[2,4],3)
  ),line=-1,cex=0.7,col="blue")
  r2alag5$work[r2alag5$country == k & r2alag5$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
    summary(lmtemp.work)$coefficients[2,1]>0,1,-1
  )
  rm(lmtemp.work)
  
  plot(R~residential,data=mobiltemp,pch=20,
       xlab="Residential mobility",ylab="Rt",cex=0.7,col=5)
  lmtemp.residential <- lm(R~residential,data=mobiltemp)
  abline(lmtemp.residential,col="blue")
  mtext(paste0("pearson ",round(with(mobiltemp,cor(R,residential,use="complete.obs")),2),
               "; adj.rsq ",round(summary(lmtemp.residential)$adj.r.squared,2),
               "; coef. ",round(summary(lmtemp.residential)$coefficients[2,1],3),
               "; p ",format.pval(summary(lmtemp.residential)$coefficients[2,4],3)
  ),line=-1,cex=0.7,col="blue")
  r2alag5$residential[r2alag5$country == k & r2alag5$lag==i] <- summary(lmtemp.residential)$adj.r.squared*ifelse(
    summary(lmtemp.residential)$coefficients[2,1]>0,1,-1
  )
  rm(lmtemp.residential)
  
  plot(R~parks,data=mobiltemp,pch=20,
       xlab="Parks mobility",ylab="Rt",cex=0.7,col=6)
  lmtemp.parks <- lm(R~parks,data=mobiltemp)
  abline(lmtemp.parks,col="blue")
  mtext(paste0("pearson ",round(with(mobiltemp,cor(R,parks,use="complete.obs")),2),
               "; adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),
               "; coef. ",round(summary(lmtemp.parks)$coefficients[2,1],3),
               "; p ",format.pval(summary(lmtemp.parks)$coefficients[2,4],3)
  ),line=-1,cex=0.7,col="blue")
  r2alag5$parks[r2alag5$country == k & r2alag5$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
    summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
  )
  rm(lmtemp.parks)
  
  title(main=paste0(k,", ",i," days of lag ", "20.02.20 to 31.12.20"), line = 1, outer = TRUE)
  rm(mobiltemp)
  
}

par(oldpar)
dev.off() 
}


r2alag6 <- data.frame("country"=rep(unique(mobil6$country),each=30),"lag"=1:30,"retail"=NA,"grocery"=NA,
                      "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)

for (i in 1:30) {
  pdf(file=paste0("Export/ScatterPlots6a/scatt",i,".pdf")) 
  par(mfrow = c(3,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
  
  for (k in unique(mobil6$country)){
    
    mobiltemp <- mobil6[mobil6$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$transit <- lag(mobiltemp$transit,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$residential <- lag(mobiltemp$residential,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    mobiltemp <- na.omit(mobiltemp)
    
    plot(R~retail,data=mobiltemp,pch=20,
         xlab="Retail mobility",ylab="Rt",cex=0.7)
    lmtemp.retail <- lm(R~retail,data=mobiltemp)
    abline(lmtemp.retail,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,retail,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.retail)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.retail)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag6$retail[r2alag6$country == k & r2alag6$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
      summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.retail)
    
    plot(R~grocery,data=mobiltemp,pch=20,
         xlab="Grocery mobility",ylab="Rt",cex=0.7)
    lmtemp.grocery <- lm(R~grocery,data=mobiltemp)
    abline(lmtemp.grocery,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,grocery,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.grocery)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.grocery)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag6$grocery[r2alag6$country == k & r2alag6$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
      summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.grocery)
    
    plot(R~transit,data=mobiltemp,pch=20,
         xlab="Transit mobility",ylab="Rt",cex=0.7)
    lmtemp.transit <- lm(R~transit,data=mobiltemp)
    abline(lmtemp.transit,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,transit,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.transit)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.transit)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.transit)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag6$transit[r2alag6$country == k & r2alag6$lag==i] <- summary(lmtemp.transit)$adj.r.squared*ifelse(
      summary(lmtemp.transit)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.transit)
    
    plot(R~work,data=mobiltemp,pch=20,
         xlab="Work mobility",ylab="Rt",cex=0.7)
    lmtemp.work <- lm(R~work,data=mobiltemp)
    abline(lmtemp.work,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,work,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.work)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.work)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag6$work[r2alag6$country == k & r2alag6$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
      summary(lmtemp.work)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.work)
    
    plot(R~residential,data=mobiltemp,pch=20,
         xlab="Residential mobility",ylab="Rt",cex=0.7)
    lmtemp.residential <- lm(R~residential,data=mobiltemp)
    abline(lmtemp.residential,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,residential,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.residential)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.residential)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.residential)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag6$residential[r2alag6$country == k & r2alag6$lag==i] <- summary(lmtemp.residential)$adj.r.squared*ifelse(
      summary(lmtemp.residential)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.residential)
    
    plot(R~parks,data=mobiltemp,pch=20,
         xlab="Parks mobility",ylab="Rt",cex=0.7)
    lmtemp.parks <- lm(R~parks,data=mobiltemp)
    abline(lmtemp.parks,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,parks,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.parks)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.parks)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag6$parks[r2alag6$country == k & r2alag6$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
      summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.parks)
    
    title(main=paste0(k,", ",i," days of lag ", "20.02.20 to 27.07.20"), line = 1, outer = TRUE)
    rm(mobiltemp)
  
}
  
  par(oldpar)
  dev.off() 
}

r2alag7 <- data.frame("country"=rep(unique(mobil7$country),each=30),"lag"=1:30,"retail"=NA,"grocery"=NA,
                      "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)

for (i in 1:30) {
  pdf(file=paste0("Export/ScatterPlots7a/scatt",i,".pdf")) 
  par(mfrow = c(3,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
  
  for (k in unique(mobil7$country)){
    
    mobiltemp <- mobil7[mobil7$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$transit <- lag(mobiltemp$transit,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$residential <- lag(mobiltemp$residential,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    mobiltemp <- na.omit(mobiltemp)
    
    plot(R~retail,data=mobiltemp,pch=20,
         xlab="Retail mobility",ylab="Rt",cex=0.7)
    lmtemp.retail <- lm(R~retail,data=mobiltemp)
    abline(lmtemp.retail,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,retail,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.retail)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.retail)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag7$retail[r2alag7$country == k & r2alag7$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
      summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.retail)
    
    plot(R~grocery,data=mobiltemp,pch=20,
         xlab="Grocery mobility",ylab="Rt",cex=0.7)
    lmtemp.grocery <- lm(R~grocery,data=mobiltemp)
    abline(lmtemp.grocery,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,grocery,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.grocery)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.grocery)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag7$grocery[r2alag7$country == k & r2alag7$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
      summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.grocery)
    
    plot(R~transit,data=mobiltemp,pch=20,
         xlab="Transit mobility",ylab="Rt",cex=0.7)
    lmtemp.transit <- lm(R~transit,data=mobiltemp)
    abline(lmtemp.transit,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,transit,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.transit)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.transit)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.transit)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag7$transit[r2alag7$country == k & r2alag7$lag==i] <- summary(lmtemp.transit)$adj.r.squared*ifelse(
      summary(lmtemp.transit)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.transit)
    
    plot(R~work,data=mobiltemp,pch=20,
         xlab="Work mobility",ylab="Rt",cex=0.7)
    lmtemp.work <- lm(R~work,data=mobiltemp)
    abline(lmtemp.work,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,work,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.work)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.work)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag7$work[r2alag7$country == k & r2alag7$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
      summary(lmtemp.work)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.work)
    
    plot(R~residential,data=mobiltemp,pch=20,
         xlab="Residential mobility",ylab="Rt",cex=0.7)
    lmtemp.residential <- lm(R~residential,data=mobiltemp)
    abline(lmtemp.residential,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,residential,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.residential)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.residential)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.residential)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag7$residential[r2alag7$country == k & r2alag7$lag==i] <- summary(lmtemp.residential)$adj.r.squared*ifelse(
      summary(lmtemp.residential)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.residential)
    
    plot(R~parks,data=mobiltemp,pch=20,
         xlab="Parks mobility",ylab="Rt",cex=0.7)
    lmtemp.parks <- lm(R~parks,data=mobiltemp)
    abline(lmtemp.parks,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,parks,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.parks)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.parks)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    r2alag7$parks[r2alag7$country == k & r2alag7$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
      summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.parks)
    
    title(main=paste0(k,", ",i," days of lag ", "27.07.20 to 31.12.20"), line = 1, outer = TRUE)
    rm(mobiltemp)
    
  }
  
  par(oldpar)
  dev.off() 
}







for (i in 0) {
  pdf(file=paste0("Export/tsmobil",i,".pdf")) 
  par(mfrow = c(3,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
  for (k in unique(mobil5$country)){
    
    mobiltemp <- mobil5[mobil5$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$transit <- lag(mobiltemp$transit,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$residential <- lag(mobiltemp$residential,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    #mobiltemp <- na.omit(mobiltemp)
    
    plot(retail~date,data=mobiltemp,ty="l",
         ylab="Retail mobility",xlab="Date (days)",lwd=1.5,col=1)
    
    plot(grocery~date,data=mobiltemp,ty="l",
         ylab="Grocery mobility",xlab="Date (days)",lwd=1.5,col=2)
    
    plot(transit~date,data=mobiltemp,ty="l",
         ylab="Transit mobility",xlab="Date (days)",lwd=1.5,col=3)
    
    plot(work~date,data=mobiltemp,ty="l",
         ylab="Work mobility",xlab="Date (days)",lwd=1.5,col=4)
    
    plot(residential~date,data=mobiltemp,ty="l",
         ylab="Residential mobility",xlab="Date (days)",lwd=1.5,col=5)
    
    plot(parks~date,data=mobiltemp,ty="l",
         ylab="Parks mobility",xlab="Date (days)",lwd=1.5,col=6)
    title(main=paste0(k,", ",i," days of lag"), line = 1, outer = TRUE)
    
    }
  par(oldpar)
  dev.off() 
}



pdf(file="Export/tsrt.pdf") 
par(mfrow = c(3,1),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
for (k in unique(mobil5$country)){
  
  mobiltemp <- mobil5[mobil5$country==k,]
  
  plot(R~date,data=mobiltemp,ty="l",
       ylab="Rt",xlab="Date (days)",lwd=1.5)
  plot(newcases~date,data=mobiltemp,ty="l",
       ylab="New cases",xlab="Date (days)",lwd=1.5)
  plot(cumcases~date,data=mobiltemp,ty="l",
       ylab="Cumulative case count",xlab="Date (days)",lwd=1.5)
  title(main=k, line = 1, outer = TRUE)
}
par(oldpar)
dev.off() 




corlag5 <- data.frame("country"=rep(unique(mobil5$country),each=30),"lag"=NA,"retail"=NA,"grocery"=NA,
                      "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in unique(mobil5$country)) {
  for (l in 1:30) {
    #
    corlag5[corlag5$country==i,]$lag[l] <- l
    corlag5[corlag5$country==i,]$retail[l] <-
      cor(lag(mobil5$retail[mobil5$country==i ],l),mobil5$R[mobil5$country==i],use="complete.obs")
    corlag5[corlag5$country==i,]$grocery[l] <-
      cor(lag(mobil5$grocery[mobil5$country==i],l),mobil5$R[mobil5$country==i],use="complete.obs")
    corlag5[corlag5$country==i,]$transit[l] <-
      cor(lag(mobil5$transit[mobil5$country==i],l),mobil5$R[mobil5$country==i],use="complete.obs")
    corlag5[corlag5$country==i,]$residential[l] <-
      cor(lag(mobil5$residential[mobil5$country==i],l),mobil5$R[mobil5$country==i],use="complete.obs")
    corlag5[corlag5$country==i,]$work[l] <-
      cor(lag(mobil5$work[mobil5$country==i],l),mobil5$R[mobil5$country==i],use="complete.obs")
    corlag5[corlag5$country==i,]$parks[l] <-
      cor(lag(mobil5$parks[mobil5$country==i],l),mobil5$R[mobil5$country==i],use="complete.obs")
    #
    }
}

pdf(file="Export/corlag5.pdf") 
for (i in unique(corlag5$country)){
  plot(retail~lag,data=corlag5[corlag5$country==i,],ty="l",main=paste0(i," 20.02.20 to 31.12.20"),
       ylim=c(-1,1),xlab="Lag (days)",ylab="Pearson r (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at = seq(-1,1,by=0.4))
  abline(h=0,lty=3)
  lines(grocery~lag,data=corlag5[corlag5$country==i,],ty="l",col=2)
  lines(transit~lag,data=corlag5[corlag5$country==i,],ty="l",col=3)
  lines(residential~lag,data=corlag5[corlag5$country==i,],ty="l",col=4)
  lines(work~lag,data=corlag5[corlag5$country==i,],ty="l",col=5)
  lines(parks~lag,data=corlag5[corlag5$country==i,],ty="l",col=6)
  legend("bottomright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,5,6),cex=0.7)
}
dev.off() 

corlag6 <- data.frame("country"=rep(unique(mobil6$country),each=30),"lag"=NA,"retail"=NA,"grocery"=NA,
                      "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in unique(mobil6$country)) {
  for (l in 1:30) {
    corlag6[corlag6$country==i,]$lag[l] <- l
    corlag6[corlag6$country==i,]$retail[l] <-
      cor(lag(mobil6$retail[mobil6$country==i],l),mobil6$R[mobil6$country==i],use="complete.obs")
    corlag6[corlag6$country==i,]$grocery[l] <-
      cor(lag(mobil6$grocery[mobil6$country==i],l),mobil6$R[mobil6$country==i],use="complete.obs")
    corlag6[corlag6$country==i,]$transit[l] <-
      cor(lag(mobil6$transit[mobil6$country==i],l),mobil6$R[mobil6$country==i],use="complete.obs")
    corlag6[corlag6$country==i,]$residential[l] <-
      cor(lag(mobil6$residential[mobil6$country==i],l),mobil6$R[mobil6$country==i],use="complete.obs")
    corlag6[corlag6$country==i,]$work[l] <-
      cor(lag(mobil6$work[mobil6$country==i],l),mobil6$R[mobil6$country==i],use="complete.obs")
    corlag6[corlag6$country==i,]$parks[l] <-
      cor(lag(mobil6$parks[mobil6$country==i],l),mobil6$R[mobil6$country==i],use="complete.obs")
  }
}

pdf(file="Export/corlag6.pdf") 
for (i in unique(corlag6$country)){
  plot(retail~lag,data=corlag6[corlag6$country==i,],ty="l",main=paste0(i," 20.02.20 to 27.07.20"),
       ylim=c(-1,1),xlab="Lag (days)",ylab="Pearson r (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at = seq(-1,1,by=0.4))
  abline(h=0,lty=3)
  lines(grocery~lag,data=corlag6[corlag6$country==i,],ty="l",col=2)
  lines(transit~lag,data=corlag6[corlag6$country==i,],ty="l",col=3)
  lines(residential~lag,data=corlag6[corlag6$country==i,],ty="l",col=4)
  lines(work~lag,data=corlag6[corlag6$country==i,],ty="l",col=5)
  lines(parks~lag,data=corlag6[corlag6$country==i,],ty="l",col=6)
  legend("bottomright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,5,6),cex=0.7)
}
dev.off() 

corlag7 <- data.frame("country"=rep(unique(mobil7$country),each=30),"lag"=NA,"retail"=NA,"grocery"=NA,
                      "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in unique(mobil7$country)) {
  for (l in 1:30) {
    corlag7[corlag7$country==i,]$lag[l] <- l
    corlag7[corlag7$country==i,]$retail[l] <-
      cor(lag(mobil7$retail[mobil7$country==i],l),mobil7$R[mobil7$country==i],use="complete.obs")
    corlag7[corlag7$country==i,]$grocery[l] <-
      cor(lag(mobil7$grocery[mobil7$country==i],l),mobil7$R[mobil7$country==i],use="complete.obs")
    corlag7[corlag7$country==i,]$transit[l] <-
      cor(lag(mobil7$transit[mobil7$country==i],l),mobil7$R[mobil7$country==i],use="complete.obs")
    corlag7[corlag7$country==i,]$residential[l] <-
      cor(lag(mobil7$residential[mobil7$country==i],l),mobil7$R[mobil7$country==i],use="complete.obs")
    corlag7[corlag7$country==i,]$work[l] <-
      cor(lag(mobil7$work[mobil7$country==i],l),mobil7$R[mobil7$country==i],use="complete.obs")
    corlag7[corlag7$country==i,]$parks[l] <-
      cor(lag(mobil7$parks[mobil7$country==i],l),mobil7$R[mobil7$country==i],use="complete.obs")
  }
}

pdf(file="Export/corlag7.pdf") 
for (i in unique(corlag7$country)){
  plot(retail~lag,data=corlag7[corlag7$country==i,],ty="l",main=paste0(i," 27.07.20 to 31.12.20"),
       ylim=c(-1,1),xlab="Lag (days)",ylab="Pearson r (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at = seq(-1,1,by=0.4))
  abline(h=0,lty=3)
  lines(grocery~lag,data=corlag7[corlag7$country==i,],ty="l",col=2)
  lines(transit~lag,data=corlag7[corlag7$country==i,],ty="l",col=3)
  lines(residential~lag,data=corlag7[corlag7$country==i,],ty="l",col=4)
  lines(work~lag,data=corlag7[corlag7$country==i,],ty="l",col=5)
  lines(parks~lag,data=corlag7[corlag7$country==i,],ty="l",col=6)
  legend("bottomright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,5,6),cex=0.7)
}
dev.off() 


#Fitting quadratic models
mob.values <- seq(-300, 520, 1)
r2blag5 <- data.frame("country"=rep(unique(mobil5$country),each=30),"lag"=0:29,"retail"=NA,"grocery"=NA,
                      "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in 0:29) {
  pdf(file=paste0("Export/ScatterPlots5b/quadr",i,".pdf")) 
  par(mfrow = c(3,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
  
  for (k in unique(mobil5$country)){
    
    mobiltemp <- mobil5[mobil5$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$transit <- lag(mobiltemp$transit,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$residential <- lag(mobiltemp$residential,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    
    mobiltemp$retail2 <- mobiltemp$retail^2
    mobiltemp$grocery2 <- mobiltemp$grocery^2
    mobiltemp$transit2 <- mobiltemp$transit^2
    mobiltemp$work2 <- mobiltemp$work^2 
    mobiltemp$residential2 <- mobiltemp$residential^2
    mobiltemp$parks2 <- mobiltemp$parks^2
    
    mobiltemp <- na.omit(mobiltemp)
    
    plot(R~retail,data=mobiltemp,pch=20,
         xlab="Retail mobility",ylab="Rt",cex=0.7,col=1)
    lmtemp.retail <- lm(R~retail+retail2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.retail,list(retail=mob.values, retail2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.retail)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag5$retail[r2blag5$country == k & r2blag5$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
      summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.retail)
    
    plot(R~grocery,data=mobiltemp,pch=20,
         xlab="Grocery mobility",ylab="Rt",cex=0.7,col=2)
    lmtemp.grocery <- lm(R~grocery+grocery2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.grocery,list(grocery=mob.values, grocery2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.grocery)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag5$grocery[r2blag5$country == k & r2blag5$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
      summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.grocery)
    
    plot(R~transit,data=mobiltemp,pch=20,
         xlab="Transit mobility",ylab="Rt",cex=0.7,col=3)
    lmtemp.transit <- lm(R~transit+transit2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.transit,list(transit=mob.values, transit2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.transit)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.transit)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag5$transit[r2blag5$country == k & r2blag5$lag==i] <- summary(lmtemp.transit)$adj.r.squared*ifelse(
      summary(lmtemp.transit)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.transit)
    
    plot(R~work,data=mobiltemp,pch=20,
         xlab="Work mobility",ylab="Rt",cex=0.7,col=4)
    lmtemp.work <- lm(R~work+work2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.work,list(work=mob.values, work2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.work)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag5$work[r2blag5$country == k & r2blag5$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
      summary(lmtemp.work)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.work)
    
    plot(R~residential,data=mobiltemp,pch=20,
         xlab="Residential mobility",ylab="Rt",cex=0.7,col=5)
    lmtemp.residential <- lm(R~residential+residential2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.residential,list(residential=mob.values, residential2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.residential)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.residential)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag5$residential[r2blag5$country == k & r2blag5$lag==i] <- summary(lmtemp.residential)$adj.r.squared*ifelse(
      summary(lmtemp.residential)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.residential)
    
    plot(R~parks,data=mobiltemp,pch=20,
         xlab="Parks mobility",ylab="Rt",cex=0.7,col=6)
    lmtemp.parks <- lm(R~parks+parks2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.parks,list(parks=mob.values, parks2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.parks)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag5$parks[r2blag5$country == k & r2blag5$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
      summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.parks)
    
    title(main=paste0(k,", quadr. , ",i," days of lag ", "20.02.20 to 31.12.20"), line = 1, outer = TRUE)
    rm(mobiltemp)
  
}
  
  par(oldpar)
  dev.off() 
}

r2blag6 <- data.frame("country"=rep(unique(mobil6$country),each=30),"lag"=1:30,"retail"=NA,"grocery"=NA,
                      "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in 1:30) {
  pdf(file=paste0("Export/ScatterPlots6b/quadr",i,".pdf")) 
  par(mfrow = c(3,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
  
  for (k in unique(mobil6$country)){
    
    mobiltemp <- mobil6[mobil6$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$transit <- lag(mobiltemp$transit,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$residential <- lag(mobiltemp$residential,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    
    mobiltemp$retail2 <- mobiltemp$retail^2
    mobiltemp$grocery2 <- mobiltemp$grocery^2
    mobiltemp$transit2 <- mobiltemp$transit^2
    mobiltemp$work2 <- mobiltemp$work^2 
    mobiltemp$residential2 <- mobiltemp$residential^2
    mobiltemp$parks2 <- mobiltemp$parks^2
    
    mobiltemp <- na.omit(mobiltemp)
    
    plot(R~retail,data=mobiltemp,pch=20,
         xlab="Retail mobility",ylab="Rt",cex=0.7)
    lmtemp.retail <- lm(R~retail+retail2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.retail,list(retail=mob.values, retail2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.retail)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag6$retail[r2blag6$country == k & r2blag6$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
      summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.retail)
    
    plot(R~grocery,data=mobiltemp,pch=20,
         xlab="Grocery mobility",ylab="Rt",cex=0.7)
    lmtemp.grocery <- lm(R~grocery+grocery2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.grocery,list(grocery=mob.values, grocery2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.grocery)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag6$grocery[r2blag6$country == k & r2blag6$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
      summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.grocery)
    
    plot(R~transit,data=mobiltemp,pch=20,
         xlab="Transit mobility",ylab="Rt",cex=0.7)
    lmtemp.transit <- lm(R~transit+transit2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.transit,list(transit=mob.values, transit2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.transit)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.transit)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag6$transit[r2blag6$country == k & r2blag6$lag==i] <- summary(lmtemp.transit)$adj.r.squared*ifelse(
      summary(lmtemp.transit)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.transit)
    
    plot(R~work,data=mobiltemp,pch=20,
         xlab="Work mobility",ylab="Rt",cex=0.7)
    lmtemp.work <- lm(R~work+work2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.work,list(work=mob.values, work2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.work)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag6$work[r2blag6$country == k & r2blag6$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
      summary(lmtemp.work)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.work)
    
    plot(R~residential,data=mobiltemp,pch=20,
         xlab="Residential mobility",ylab="Rt",cex=0.7)
    lmtemp.residential <- lm(R~residential+residential2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.residential,list(residential=mob.values, residential2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.residential)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.residential)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag6$residential[r2blag6$country == k & r2blag6$lag==i] <- summary(lmtemp.residential)$adj.r.squared*ifelse(
      summary(lmtemp.residential)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.residential)
    
    plot(R~parks,data=mobiltemp,pch=20,
         xlab="Parks mobility",ylab="Rt",cex=0.7)
    lmtemp.parks <- lm(R~parks+parks2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.parks,list(parks=mob.values, parks2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.parks)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag6$parks[r2blag6$country == k & r2blag6$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
      summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.parks)
    
    title(main=paste0(k,", quadr. , ",i," days of lag ", "20.02.20 to 27.07.20"), line = 1, outer = TRUE)
    rm(mobiltemp)
  
    }
  
  par(oldpar)
  dev.off() 
}


r2blag7 <- data.frame("country"=rep(unique(mobil7$country),each=30),"lag"=1:30,"retail"=NA,"grocery"=NA,
                      "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in 1:30) {
  pdf(file=paste0("Export/ScatterPlots7b/quadr",i,".pdf")) 
  par(mfrow = c(3,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
  
  for (k in unique(mobil7$country)){
    
      
    mobiltemp <- mobil7[mobil7$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$transit <- lag(mobiltemp$transit,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$residential <- lag(mobiltemp$residential,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    
    mobiltemp$retail2 <- mobiltemp$retail^2
    mobiltemp$grocery2 <- mobiltemp$grocery^2
    mobiltemp$transit2 <- mobiltemp$transit^2
    mobiltemp$work2 <- mobiltemp$work^2 
    mobiltemp$residential2 <- mobiltemp$residential^2
    mobiltemp$parks2 <- mobiltemp$parks^2
    
    mobiltemp <- na.omit(mobiltemp)
    
    plot(R~retail,data=mobiltemp,pch=20,
         xlab="Retail mobility",ylab="Rt",cex=0.7)
    lmtemp.retail <- lm(R~retail+retail2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.retail,list(retail=mob.values, retail2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.retail)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag7$retail[r2blag7$country == k & r2blag7$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
      summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.retail)
    
    plot(R~grocery,data=mobiltemp,pch=20,
         xlab="Grocery mobility",ylab="Rt",cex=0.7)
    lmtemp.grocery <- lm(R~grocery+grocery2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.grocery,list(grocery=mob.values, grocery2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.grocery)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag7$grocery[r2blag7$country == k & r2blag7$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
      summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.grocery)
    
    plot(R~transit,data=mobiltemp,pch=20,
         xlab="Transit mobility",ylab="Rt",cex=0.7)
    lmtemp.transit <- lm(R~transit+transit2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.transit,list(transit=mob.values, transit2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.transit)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.transit)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag7$transit[r2blag7$country == k & r2blag7$lag==i] <- summary(lmtemp.transit)$adj.r.squared*ifelse(
      summary(lmtemp.transit)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.transit)
    
    plot(R~work,data=mobiltemp,pch=20,
         xlab="Work mobility",ylab="Rt",cex=0.7)
    lmtemp.work <- lm(R~work+work2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.work,list(work=mob.values, work2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.work)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag7$work[r2blag7$country == k & r2blag7$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
      summary(lmtemp.work)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.work)
    
    plot(R~residential,data=mobiltemp,pch=20,
         xlab="Residential mobility",ylab="Rt",cex=0.7)
    lmtemp.residential <- lm(R~residential+residential2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.residential,list(residential=mob.values, residential2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.residential)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.residential)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag7$residential[r2blag7$country == k & r2blag7$lag==i] <- summary(lmtemp.residential)$adj.r.squared*ifelse(
      summary(lmtemp.residential)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.residential)
    
    plot(R~parks,data=mobiltemp,pch=20,
         xlab="Parks mobility",ylab="Rt",cex=0.7)
    lmtemp.parks <- lm(R~parks+parks2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.parks,list(parks=mob.values, parks2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.parks)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    r2blag7$parks[r2blag7$country == k & r2blag7$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
      summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.parks)
    
    title(main=paste0(k,", quadr. , ",i," days of lag ", "27.07.20 to 31.12.20"), line = 1, outer = TRUE)
    rm(mobiltemp)
  
}
  
  par(oldpar)
  dev.off() 
}



pdf(file="Export/r2alag5.pdf") 
for (i in unique(r2alag5$country)){
  plot(retail~lag,data=r2alag5[r2alag5$country==i,],ty="l",main=paste0(i," linear 20.02.20 to 31.12.20"),
       ylim=c(-1,1),xlab="Lag (days)",ylab="Adjusted r2 (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at =  seq(-1,1,by=0.2))
  
  lines(grocery~lag,data=r2alag5[r2alag5$country==i,],ty="l",col=2)
  lines(transit~lag,data=r2alag5[r2alag5$country==i,],ty="l",col=3)
  lines(residential~lag,data=r2alag5[r2alag5$country==i,],ty="l",col=4)
  lines(work~lag,data=r2alag5[r2alag5$country==i,],ty="l",col=5)
  lines(parks~lag,data=r2alag5[r2alag5$country==i,],ty="l",col=6)
  legend("topright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,5,6),cex=0.7)
}
dev.off() 

pdf(file="Export/r2alag6.pdf") 
for (i in unique(r2alag6$country)){
  plot(retail~lag,data=r2alag6[r2alag6$country==i,],ty="l",main=paste0(i," linear 20.02.20 to 27.07.20"),
       ylim=c(-1,1),xlab="Lag (days)",ylab="Adjusted r2 (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at =  seq(-1,1,by=0.2))
  
  lines(grocery~lag,data=r2alag6[r2alag6$country==i,],ty="l",col=2)
  lines(transit~lag,data=r2alag6[r2alag6$country==i,],ty="l",col=3)
  lines(residential~lag,data=r2alag6[r2alag6$country==i,],ty="l",col=4)
  lines(work~lag,data=r2alag6[r2alag6$country==i,],ty="l",col=6)
  lines(parks~lag,data=r2alag6[r2alag6$country==i,],ty="l",col=6)
  legend("topright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,6,6),cex=0.7)
}
dev.off() 

pdf(file="Export/r2alag7.pdf") 
for (i in unique(r2alag7$country)){
  plot(retail~lag,data=r2alag7[r2alag7$country==i,],ty="l",main=paste0(i," linear 27.07.20 to 31.12.20"),
       ylim=c(-1,1),xlab="Lag (days)",ylab="Adjusted r2 (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at =  seq(-1,1,by=0.2))
  
  lines(grocery~lag,data=r2alag7[r2alag7$country==i,],ty="l",col=2)
  lines(transit~lag,data=r2alag7[r2alag7$country==i,],ty="l",col=3)
  lines(residential~lag,data=r2alag7[r2alag7$country==i,],ty="l",col=4)
  lines(work~lag,data=r2alag7[r2alag7$country==i,],ty="l",col=7)
  lines(parks~lag,data=r2alag7[r2alag7$country==i,],ty="l",col=6)
  legend("topright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,7,6),cex=0.7)
}
dev.off() 


pdf(file="Export/r2blag5.pdf") 
for (i in unique(r2blag5$country)){
  plot(retail~lag,data=r2blag5[r2blag5$country==i,],ty="l",main=paste0(i," quadr 20.02.20 to 31.12.20"),
       ylim=c(-1,1),xlab="Lag (days)",ylab="Adjusted r2 (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at =  seq(-1,1,by=0.2))
  
  lines(grocery~lag,data=r2blag5[r2blag5$country==i,],ty="l",col=2)
  lines(transit~lag,data=r2blag5[r2blag5$country==i,],ty="l",col=3)
  lines(residential~lag,data=r2blag5[r2blag5$country==i,],ty="l",col=4)
  lines(work~lag,data=r2blag5[r2blag5$country==i,],ty="l",col=5)
  lines(parks~lag,data=r2blag5[r2blag5$country==i,],ty="l",col=6)
  legend("topright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,5,6),cex=0.7)
}
dev.off() 

pdf(file="Export/r2blag6.pdf") 
for (i in unique(r2blag6$country)){
  plot(retail~lag,data=r2blag6[r2blag6$country==i,],ty="l",main=paste0(i," quadr 20.02.20 to 27.07.20"),
       ylim=c(-1,1),xlab="Lag (days)",ylab="Adjusted r2 (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at =  seq(-1,1,by=0.2))
  
  lines(grocery~lag,data=r2blag6[r2blag6$country==i,],ty="l",col=2)
  lines(transit~lag,data=r2blag6[r2blag6$country==i,],ty="l",col=3)
  lines(residential~lag,data=r2blag6[r2blag6$country==i,],ty="l",col=4)
  lines(work~lag,data=r2blag6[r2blag6$country==i,],ty="l",col=6)
  lines(parks~lag,data=r2blag6[r2blag6$country==i,],ty="l",col=6)
  legend("topright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,6,6),cex=0.7)
}
dev.off() 

pdf(file="Export/r2blag7.pdf") 
for (i in unique(r2blag7$country)){
  plot(retail~lag,data=r2blag7[r2blag7$country==i,],ty="l",main=paste0(i," quadr 27.07.20 to 31.12.20"),
       ylim=c(-1,1),xlab="Lag (days)",ylab="Adjusted r2 (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at =  seq(-1,1,by=0.2))
  
  lines(grocery~lag,data=r2blag7[r2blag7$country==i,],ty="l",col=2)
  lines(transit~lag,data=r2blag7[r2blag7$country==i,],ty="l",col=3)
  lines(residential~lag,data=r2blag7[r2blag7$country==i,],ty="l",col=4)
  lines(work~lag,data=r2blag7[r2blag7$country==i,],ty="l",col=7)
  lines(parks~lag,data=r2blag7[r2blag7$country==i,],ty="l",col=6)
  legend("topright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,7,6),cex=0.7)
}
dev.off() 


#### 6. Number of occurrences and median days of observation ####
meddays <- c()
for (i in unique(mobil5$country)) {
  meddays <- append(meddays,length(unique(mobil5$date[mobil5$country==i])))
}
summary(meddays)
#max 316
#median 299


#### 7. Removing autocorrelation ####
library(tseries)
acf(mobil5$transit[mobil5$country=="Germany" & mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31"],lag.max=50)

#library(forecast)
transit.de <- ts(data = mobil5$transit[
  mobil5$country=="Germany" & mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31"
  ])
Rt.de <- ts(data = mobil5$R[
  mobil5$country=="Germany" & mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31"
])
transit.de.arima<- auto.arima(transit.de, stepwise=FALSE, approx=FALSE, parallel=TRUE, allowmean=FALSE,
                              allowdrift=FALSE, seasonal = TRUE)

summary(transit.de.arima)  

transit.de.pw <- resid(transit.de.arima)

qqnorm(transit.de.pw)
acf(transit.de.pw)

Rt.de.filtered <- residuals(Arima(Rt.de, model=transit.de.arima))
ccf1 <- ccf(transit.de.pw, Rt.de.filtered, lag.max=30)
plot(ccf1, main="", ylab="CCF", xlab="lag (day)")
p <- 2* (1 - pnorm(abs(ccf1$acf), mean = 0, sd = 1/sqrt(ccf1$n.used)))
cbind(ccf1$acf, p)


plot(transit.de)
plot(transit.de.pw)
cor(transit.de, transit.de.pw)

cor(transit.de, Rt.de)
cor.test(transit.de, Rt.de)

cor(lag(as.vector(transit.de), 14), Rt.de, use = "complete.obs")
cor(lag(as.vector(transit.de.pw), 14), Rt.de.filtered, use = "complete.obs")


### Sylvain's method
# gather all R(t), M(t) datapoints.
transit.de <- ts(data = mobil5$transit[
  mobil5$country=="Germany" & mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31"
])
Rt.de <- ts(data = mobil5$R[
  mobil5$country=="Germany" & mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31"
])

# perform the linear fit (including the determination of coefs a and b)
transit.de.fit <- lm(Rt.de ~ transit.de)
summary(transit.de.fit)
##Rt = a + b.Mt
##coef.b = 0.0071080 coef.a = 1.2551826

# Compute the residuals of the least squares fit e(t)=R(t)-a*M(t)
transit.de.resid <- resid(transit.de.fit)
plot(transit.de.resid)

# Perform a  statistical test on the residuals to check for the presence of autocorrelations.
acf(transit.de.resid, 30)

transit.de.D <- sum(
  (transit.de.resid-lag(transit.de.resid,1))^2 #, ...
  )

durbinWatsonTest(transit.de.fit, max.lag = 30)


#### 8a. Correcting for autocorrelation using the Durbin Watson Test ####
mobil5c <- mobil5[mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31",
                  c("country","date")]
durb <- data.frame(country = unique(mobil5$country),
                   "retail"=NA, "grocery"=NA, "transit"=NA, "work"=NA, "residential"=NA, "parks"=NA)


for (i in mobil5c$country) {
#  tryCatch({
  for (k in c("retail", "grocery", "transit", "work", "residential", "parks")) {
#    tryCatch({
    durb[durb$country == i, k] <- durbinWatsonTest(
      lm( ts(data = mobil5[mobil5$country == i & 
                             mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31", "R"]) ~
            ts(data = mobil5[mobil5$country == i &
                               mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31", k])),
      max.lag = 1)$r[1]
    mobil5c[mobil5c$country == i, k] <- 
      ts(data = mobil5[mobil5$country == i &
                         mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31", k]) -
      durb[durb$country == i, k] *
      lead(ts(data = mobil5[mobil5$country == i &
                              mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31", k]),1)
#    }, error=function(e){})
  }
  mobil5c[mobil5c$country == i, "R"] <- 
    mobil5[mobil5$country == i & mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31", "R"] - 
    durb[durb$country==i,"residential"]*
    lead(mobil5[mobil5$country == i & mobil5$date >= "2020-02-22" & mobil5$date <= "2020-12-31", "R"],1)
#}, error=function(e){})
}

write.csv(mobil5c, file = "Export/mobil5c.csv", row.names = FALSE)
write.csv(durb, file = "Export/durb.csv", row.names = FALSE)


#### 8b. Second level correction ####

mobil5c2 <- mobil5c[mobil5c$date >= "2020-02-22" & mobil5c$date <= "2020-12-31",
                  c("country","date")]
durb2 <- data.frame(country = unique(mobil5$country),
                   "retail"=NA, "grocery"=NA, "transit"=NA, "work"=NA, "residential"=NA, "parks"=NA)


for (i in mobil5c2$country) {
  #  tryCatch({
  for (k in c("retail", "grocery", "transit", "work", "residential", "parks")) {
    #    tryCatch({
    durb2[durb2$country == i, k] <- durbinWatsonTest(
      lm( ts(data = mobil5c[mobil5c$country == i & 
                             mobil5c$date >= "2020-02-22" & mobil5c$date <= "2020-12-31", "R"]) ~
            ts(data = mobil5c[mobil5c$country == i &
                               mobil5c$date >= "2020-02-22" & mobil5c$date <= "2020-12-31", k])),
      max.lag = 1)$r[1]
    mobil5c2[mobil5c2$country == i, k] <- 
      ts(data = mobil5c[mobil5c$country == i &
                         mobil5c$date >= "2020-02-22" & mobil5c$date <= "2020-12-31", k]) -
      durb2[durb2$country == i, k] *
      lead(ts(data = mobil5c[mobil5c$country == i &
                              mobil5c$date >= "2020-02-22" & mobil5c$date <= "2020-12-31", k]),1)
    #    }, error=function(e){})
  }
  mobil5c2[mobil5c2$country == i, "R"] <- 
    mobil5c[mobil5c$country == i & mobil5c$date >= "2020-02-22" & mobil5c$date <= "2020-12-31", "R"] - 
    durb2[durb2$country==i,"residential"]*
    lead(mobil5c[mobil5c$country == i & mobil5c$date >= "2020-02-22" & mobil5c$date <= "2020-12-31", "R"],1)
  #}, error=function(e){})
}

write.csv(mobil5c2, file = "Export/mobil5c.csv", row.names = FALSE)
write.csv(durb2, file = "Export/durb2.csv", row.names = FALSE)

#### 9. Plotting corlag with correction for autoregression ####
corlag5c <- data.frame("country"=rep(unique(mobil5c$country),each=30),"lag"=NA,"retail"=NA,"grocery"=NA,
                       "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in unique(mobil5c$country)) {
  for (l in 1:30) {
    #
    corlag5c[corlag5c$country==i,]$lag[l] <- l
    corlag5c[corlag5c$country==i,]$retail[l] <-
      cor(lag(mobil5c$retail[mobil5c$country==i ],l),mobil5c$R[mobil5c$country==i],use="complete.obs")
    corlag5c[corlag5c$country==i,]$grocery[l] <-
      cor(lag(mobil5c$grocery[mobil5c$country==i],l),mobil5c$R[mobil5c$country==i],use="complete.obs")
    corlag5c[corlag5c$country==i,]$transit[l] <-
      cor(lag(mobil5c$transit[mobil5c$country==i],l),mobil5c$R[mobil5c$country==i],use="complete.obs")
    corlag5c[corlag5c$country==i,]$residential[l] <-
      cor(lag(mobil5c$residential[mobil5c$country==i],l),mobil5c$R[mobil5c$country==i],use="complete.obs")
    corlag5c[corlag5c$country==i,]$work[l] <-
      cor(lag(mobil5c$work[mobil5c$country==i],l),mobil5c$R[mobil5c$country==i],use="complete.obs")
    corlag5c[corlag5c$country==i,]$parks[l] <-
      cor(lag(mobil5c$parks[mobil5c$country==i],l),mobil5c$R[mobil5c$country==i],use="complete.obs")
    #
  }
}

pdf(file="Export/corlag5c.pdf") 
for (i in unique(corlag5c$country)){
  plot(retail~lag,data=corlag5c[corlag5c$country==i,],ty="l",
       main=paste0(i," 20.02.20 to 31.12.20"),
       sub="adjusted for autocorrelation - level 1 (DW test coef.)",
       ylim=c(-1,1),xlab="Lag (days)",ylab="Pearson r (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at = seq(-1,1,by=0.4))
  abline(h=0,lty=3)
  lines(grocery~lag,data=corlag5c[corlag5c$country==i,],ty="l",col=2)
  lines(transit~lag,data=corlag5c[corlag5c$country==i,],ty="l",col=3)
  lines(residential~lag,data=corlag5c[corlag5c$country==i,],ty="l",col=4)
  lines(work~lag,data=corlag5c[corlag5c$country==i,],ty="l",col=5)
  lines(parks~lag,data=corlag5c[corlag5c$country==i,],ty="l",col=6)
  legend("bottomright",legend = c(paste0("retail (", round(durb$retail[durb$country==i],2),")"),
                                  paste0("grocery (", round(durb$grocery[durb$country==i],2),")"),
                                  paste0("transit (", round(durb$transit[durb$country==i],2),")"),
                                  paste0("residential (", round(durb$residential[durb$country==i],2),")"),
                                  paste0("work (", round(durb$work[durb$country==i],2),")"),
                                  paste0("parks (", round(durb$parks[durb$country==i],2),")")),
         lty=1, col=c(1,2,3,4,5,6),cex=0.7)
}
dev.off() 





corlag5c2 <- data.frame("country"=rep(unique(mobil5c2$country),each=30),"lag"=NA,"retail"=NA,"grocery"=NA,
                        "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in unique(mobil5c2$country)) {
  for (l in 1:30) {
    #
    corlag5c2[corlag5c2$country==i,]$lag[l] <- l
    corlag5c2[corlag5c2$country==i,]$retail[l] <-
      cor(lag(mobil5c2$retail[mobil5c2$country==i ],l),mobil5c2$R[mobil5c2$country==i],use="complete.obs")
    corlag5c2[corlag5c2$country==i,]$grocery[l] <-
      cor(lag(mobil5c2$grocery[mobil5c2$country==i],l),mobil5c2$R[mobil5c2$country==i],use="complete.obs")
    corlag5c2[corlag5c2$country==i,]$transit[l] <-
      cor(lag(mobil5c2$transit[mobil5c2$country==i],l),mobil5c2$R[mobil5c2$country==i],use="complete.obs")
    corlag5c2[corlag5c2$country==i,]$residential[l] <-
      cor(lag(mobil5c2$residential[mobil5c2$country==i],l),mobil5c2$R[mobil5c2$country==i],use="complete.obs")
    corlag5c2[corlag5c2$country==i,]$work[l] <-
      cor(lag(mobil5c2$work[mobil5c2$country==i],l),mobil5c2$R[mobil5c2$country==i],use="complete.obs")
    corlag5c2[corlag5c2$country==i,]$parks[l] <-
      cor(lag(mobil5c2$parks[mobil5c2$country==i],l),mobil5c2$R[mobil5c2$country==i],use="complete.obs")
    #
  }
}

pdf(file="Export/corlag5c2.pdf") 
for (i in unique(corlag5c2$country)){
  plot(retail~lag,data=corlag5c2[corlag5c2$country==i,],ty="l",
       main=paste0(i," 20.02.20 to 31.12.20"),
       sub="adjusted for autocorrelation - level 2 (DW test coef.)",
       ylim=c(-1,1),xlab="Lag (days)",ylab="Pearson r (Rt ~ mobility)",axes=FALSE)
  axis(side = 1, at = 1:30)
  axis(side = 2, at = seq(-1,1,by=0.4))
  abline(h=0,lty=3)
  lines(grocery~lag,data=corlag5c2[corlag5c2$country==i,],ty="l",col=2)
  lines(transit~lag,data=corlag5c2[corlag5c2$country==i,],ty="l",col=3)
  lines(residential~lag,data=corlag5c2[corlag5c2$country==i,],ty="l",col=4)
  lines(work~lag,data=corlag5c2[corlag5c2$country==i,],ty="l",col=5)
  lines(parks~lag,data=corlag5c2[corlag5c2$country==i,],ty="l",col=6)
  legend("bottomright",legend = c(paste0("retail (", round(durb2$retail[durb2$country==i],2),")"),
                                  paste0("grocery (", round(durb2$grocery[durb2$country==i],2),")"),
                                  paste0("transit (", round(durb2$transit[durb2$country==i],2),")"),
                                  paste0("residential (", round(durb2$residential[durb2$country==i],2),")"),
                                  paste0("work (", round(durb2$work[durb2$country==i],2),")"),
                                  paste0("parks (", round(durb2$parks[durb2$country==i],2),")")),
         lty=1, col=c(1,2,3,4,5,6),cex=0.7)
}
dev.off() 



#### 10. Grouping the countries ####
grp1 <- corlag5$country[corlag5$lag==14 & corlag5$retail>0 &
                          corlag5$grocery>0 &
                          corlag5$transit>0 &
                          corlag5$residential <0 &
                          corlag5$work>0]

grp2 <- corlag5$country[corlag5$lag==14 & corlag5$retail<0 &
                          corlag5$grocery<0 &
                          corlag5$transit<0 &
                          corlag5$residential >0 &
                          corlag5$work<0]

grp3 <- unique(corlag5$country)[which(unique(corlag5$country) %notin% c(grp1,grp2))]

print.table(list(grp1,grp2,grp3))


#### 11. Scatter plots of residential vs other mobility indicators ####
residvsmobil5 <- data.frame("country"=rep(unique(mobil5$country),each=30),"lag"=0:29,"retail"=NA,"grocery"=NA,
                            "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)

for (i in 0:29) {
  pdf(file=paste0("Export/ResidMobil5a/residlin",i,".pdf")) 
  
  
  for (k in unique(mobil5$country)){
    par(mfrow = c(3,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
    mobiltemp <- mobil5[mobil5$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$transit <- lag(mobiltemp$transit,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    mobiltemp <- na.omit(mobiltemp)
    
    plot(residential~retail,data=mobiltemp,pch=20,
         xlab="Retail mobility",ylab="Residential mobility",cex=0.7,col=1)
    lmtemp.retail <- lm(residential~retail,data=mobiltemp)
    abline(lmtemp.retail,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,retail,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.retail)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.retail)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    residvsmobil5$retail[residvsmobil5$country == k & residvsmobil5$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
      summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.retail)
    
    plot(residential~grocery,data=mobiltemp,pch=20,
         xlab="Grocery mobility",ylab="Residential mobility",cex=0.7,col=2)
    lmtemp.grocery <- lm(residential~grocery,data=mobiltemp)
    abline(lmtemp.grocery,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,grocery,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.grocery)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.grocery)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    residvsmobil5$grocery[residvsmobil5$country == k & residvsmobil5$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
      summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.grocery)
    
    plot(residential~transit,data=mobiltemp,pch=20,
         xlab="Transit mobility",ylab="Residential mobility",cex=0.7,col=3)
    lmtemp.transit <- lm(residential~transit,data=mobiltemp)
    abline(lmtemp.transit,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,transit,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.transit)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.transit)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.transit)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    residvsmobil5$transit[residvsmobil5$country == k & residvsmobil5$lag==i] <- summary(lmtemp.transit)$adj.r.squared*ifelse(
      summary(lmtemp.transit)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.transit)
    
    plot(residential~work,data=mobiltemp,pch=20,
         xlab="Work mobility",ylab="Residential mobility",cex=0.7,col=4)
    lmtemp.work <- lm(residential~work,data=mobiltemp)
    abline(lmtemp.work,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,work,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.work)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.work)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    residvsmobil5$work[residvsmobil5$country == k & residvsmobil5$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
      summary(lmtemp.work)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.work)
    
    
    plot(residential~parks,data=mobiltemp,pch=20,
         xlab="Parks mobility",ylab="Residential mobility",cex=0.7,col=6)
    lmtemp.parks <- lm(residential~parks,data=mobiltemp)
    abline(lmtemp.parks,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,parks,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.parks)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.parks)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    residvsmobil5$parks[residvsmobil5$country == k & residvsmobil5$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
      summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.parks)
    
    title(main=paste0(k,", ",i," days of lag ", "20.02.20 to 31.12.20"), line = 1, outer = TRUE)
    rm(mobiltemp)
    par(oldpar)
  }
  
 
  dev.off() 
}


mob.values <- seq(-300, 520, 1)
residvsmobil5q <- data.frame("country"=rep(unique(mobil5$country),each=30),"lag"=0:29,"retail"=NA,"grocery"=NA,
                             "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in 0:29) {
  pdf(file=paste0("Export/ResidMobil5b/residquad",i,".pdf")) 
  
  
  for (k in unique(mobil5$country)){
    par(mfrow = c(3,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
    mobiltemp <- mobil5[mobil5$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$transit <- lag(mobiltemp$transit,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    
    mobiltemp$retail2 <- mobiltemp$retail^2
    mobiltemp$grocery2 <- mobiltemp$grocery^2
    mobiltemp$transit2 <- mobiltemp$transit^2
    mobiltemp$work2 <- mobiltemp$work^2 
    mobiltemp$parks2 <- mobiltemp$parks^2
    
    mobiltemp <- na.omit(mobiltemp)
    
    plot(residential~retail,data=mobiltemp,pch=20,
         xlab="Retail mobility",ylab="Residential mobility",cex=0.7,col=1)
    lmtemp.retail <- lm(residential~retail+retail2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.retail,list(retail=mob.values, retail2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.retail)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    residvsmobil5q$retail[residvsmobil5q$country == k & residvsmobil5q$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
      summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.retail)
    
    plot(residential~grocery,data=mobiltemp,pch=20,
         xlab="Grocery mobility",ylab="Residential mobility",cex=0.7,col=2)
    lmtemp.grocery <- lm(residential~grocery+grocery2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.grocery,list(grocery=mob.values, grocery2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.grocery)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    residvsmobil5q$grocery[residvsmobil5q$country == k & residvsmobil5q$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
      summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.grocery)
    
    plot(residential~transit,data=mobiltemp,pch=20,
         xlab="Transit mobility",ylab="Residential mobility",cex=0.7,col=3)
    lmtemp.transit <- lm(residential~transit+transit2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.transit,list(transit=mob.values, transit2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.transit)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.transit)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    residvsmobil5q$transit[residvsmobil5q$country == k & residvsmobil5q$lag==i] <- summary(lmtemp.transit)$adj.r.squared*ifelse(
      summary(lmtemp.transit)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.transit)
    
    plot(residential~work,data=mobiltemp,pch=20,
         xlab="Work mobility",ylab="Residential mobility",cex=0.7,col=4)
    lmtemp.work <- lm(residential~work+work2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.work,list(work=mob.values, work2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.work)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    residvsmobil5q$work[residvsmobil5q$country == k & residvsmobil5q$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
      summary(lmtemp.work)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.work)
    
    
    plot(residential~parks,data=mobiltemp,pch=20,
         xlab="Parks mobility",ylab="Residential mobility",cex=0.7,col=6)
    lmtemp.parks <- lm(residential~parks+parks2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.parks,list(parks=mob.values, parks2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.parks)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    residvsmobil5q$parks[residvsmobil5q$country == k & residvsmobil5q$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
      summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.parks)
    
    title(main=paste0(k,", quadr. , ",i," days of lag ", "20.02.20 to 31.12.20"), line = 1, outer = TRUE)
    rm(mobiltemp)
    par(oldpar) 
  }
  
 
  dev.off() 
}

#### 12. Scatter plots of transit vs mobility ####
transitvsmobil5 <- data.frame("country"=rep(unique(mobil5$country),each=30),"lag"=0:29,"retail"=NA,"grocery"=NA,
                              "transit"=NA,"work"=NA,"parks"=NA)

for (i in 0:29) {
  pdf(file=paste0("Export/TransitMobil5a/transitlin",i,".pdf")) 
  par(mfrow = c(2,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
  
  for (k in unique(mobil5$country)){
    
    mobiltemp <- mobil5[mobil5$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    mobiltemp <- na.omit(mobiltemp)
    
    plot(transit~retail,data=mobiltemp,pch=20,
         xlab="Retail mobility",ylab="Transit mobility",cex=0.7,col=1)
    lmtemp.retail <- lm(transit~retail,data=mobiltemp)
    abline(lmtemp.retail,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,retail,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.retail)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.retail)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    transitvsmobil5$retail[transitvsmobil5$country == k & transitvsmobil5$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
      summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.retail)
    
    plot(transit~grocery,data=mobiltemp,pch=20,
         xlab="Grocery mobility",ylab="Transit mobility",cex=0.7,col=2)
    lmtemp.grocery <- lm(transit~grocery,data=mobiltemp)
    abline(lmtemp.grocery,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,grocery,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.grocery)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.grocery)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    transitvsmobil5$grocery[transitvsmobil5$country == k & transitvsmobil5$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
      summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.grocery)
    
    
    plot(transit~work,data=mobiltemp,pch=20,
         xlab="Work mobility",ylab="Transit mobility",cex=0.7,col=4)
    lmtemp.work <- lm(transit~work,data=mobiltemp)
    abline(lmtemp.work,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,work,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.work)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.work)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    transitvsmobil5$work[transitvsmobil5$country == k & transitvsmobil5$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
      summary(lmtemp.work)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.work)
    
    
    plot(transit~parks,data=mobiltemp,pch=20,
         xlab="Parks mobility",ylab="Transit mobility",cex=0.7,col=6)
    lmtemp.parks <- lm(transit~parks,data=mobiltemp)
    abline(lmtemp.parks,col="blue")
    mtext(paste0("pearson ",round(with(mobiltemp,cor(R,parks,use="complete.obs")),2),
                 "; adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),
                 "; coef. ",round(summary(lmtemp.parks)$coefficients[2,1],3),
                 "; p ",format.pval(summary(lmtemp.parks)$coefficients[2,4],3)
    ),line=-1,cex=0.7,col="blue")
    transitvsmobil5$parks[transitvsmobil5$country == k & transitvsmobil5$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
      summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.parks)
    
    title(main=paste0(k,", ",i," days of lag ", "20.02.20 to 31.12.20"), line = 1, outer = TRUE)
    rm(mobiltemp)
    
  }
  
  par(oldpar)
  dev.off() 
}


mob.values <- seq(-300, 520, 1)
transitvsmobil5q <- data.frame("country"=rep(unique(mobil5$country),each=30),"lag"=0:29,"retail"=NA,"grocery"=NA,
                               "transit"=NA,"transit"=NA,"work"=NA,"parks"=NA)
for (i in 0:29) {
  pdf(file=paste0("Export/TransitMobil5b/transitquad",i,".pdf")) 
  par(mfrow = c(2,2),mar = c(4, 4, 0.2, 0.2),oma=c(1,1,3,1))
  
  for (k in unique(mobil5$country)){
    
    mobiltemp <- mobil5[mobil5$country==k,]
    mobiltemp$retail <- lag(mobiltemp$retail,i)
    mobiltemp$grocery <- lag(mobiltemp$grocery,i)
    mobiltemp$work <- lag(mobiltemp$work,i)
    mobiltemp$parks <- lag(mobiltemp$parks,i)
    
    mobiltemp$retail2 <- mobiltemp$retail^2
    mobiltemp$grocery2 <- mobiltemp$grocery^2
    mobiltemp$work2 <- mobiltemp$work^2 
    mobiltemp$parks2 <- mobiltemp$parks^2
    
    mobiltemp <- na.omit(mobiltemp)
    
    plot(transit~retail,data=mobiltemp,pch=20,
         xlab="Retail mobility",ylab="Transit mobility",cex=0.7,col=1)
    lmtemp.retail <- lm(transit~retail+retail2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.retail,list(retail=mob.values, retail2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.retail)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.retail)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    transitvsmobil5q$retail[transitvsmobil5q$country == k & transitvsmobil5q$lag==i] <- summary(lmtemp.retail)$adj.r.squared*ifelse(
      summary(lmtemp.retail)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.retail)
    
    plot(transit~grocery,data=mobiltemp,pch=20,
         xlab="Grocery mobility",ylab="Transit mobility",cex=0.7,col=2)
    lmtemp.grocery <- lm(transit~grocery+grocery2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.grocery,list(grocery=mob.values, grocery2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.grocery)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.grocery)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    transitvsmobil5q$grocery[transitvsmobil5q$country == k & transitvsmobil5q$lag==i] <- summary(lmtemp.grocery)$adj.r.squared*ifelse(
      summary(lmtemp.grocery)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.grocery)
    
    
    plot(transit~work,data=mobiltemp,pch=20,
         xlab="Work mobility",ylab="Transit mobility",cex=0.7,col=4)
    lmtemp.work <- lm(transit~work+work2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.work,list(work=mob.values, work2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.work)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.work)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    transitvsmobil5q$work[transitvsmobil5q$country == k & transitvsmobil5q$lag==i] <- summary(lmtemp.work)$adj.r.squared*ifelse(
      summary(lmtemp.work)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.work)
    
    
    plot(transit~parks,data=mobiltemp,pch=20,
         xlab="Parks mobility",ylab="Transit mobility",cex=0.7,col=6)
    lmtemp.parks <- lm(transit~parks+parks2,data=mobiltemp)
    lines(mob.values, predict(lmtemp.parks,list(parks=mob.values, parks2=mob.values^2)), col='blue')
    mtext(paste0("adj.rsq ",round(summary(lmtemp.parks)$adj.r.squared,2),"; coef. x100 ",
                 round(summary(lmtemp.parks)$coefficients[2,1]*100,2)),line=-1,cex=0.7,col="blue")
    transitvsmobil5q$parks[transitvsmobil5q$country == k & transitvsmobil5q$lag==i] <- summary(lmtemp.parks)$adj.r.squared*ifelse(
      summary(lmtemp.parks)$coefficients[2,1]>0,1,-1
    )
    rm(lmtemp.parks)
    
    title(main=paste0(k,", quadr. , ",i," days of lag ", "20.02.20 to 31.12.20"), line = 1, outer = TRUE)
    rm(mobiltemp)
    
  }
  
  par(oldpar)
  dev.off() 
}

#### 13. Plots for p-values of corlag ####

pcorlag5 <- data.frame("country"=rep(unique(mobil5$country),each=30),"lag"=NA,"retail"=NA,"grocery"=NA,
                       "transit"=NA,"residential"=NA,"work"=NA,"parks"=NA)
for (i in unique(mobil5$country)) {
  for (l in 1:30) {
    
    pcorlag5[pcorlag5$country==i,]$lag[l] <- l
    pcorlag5[pcorlag5$country==i,]$retail[l] <-
      cor.test(lag(mobil5$retail[mobil5$country==i ],l),mobil5$R[mobil5$country==i], method = c("pearson"), na.action="na.omit")$p.value
    pcorlag5[pcorlag5$country==i,]$grocery[l] <-
      cor.test(lag(mobil5$grocery[mobil5$country==i],l),mobil5$R[mobil5$country==i],method = c("pearson"), na.action="na.omit")$p.value
    pcorlag5[pcorlag5$country==i,]$transit[l] <-
      cor.test(lag(mobil5$transit[mobil5$country==i],l),mobil5$R[mobil5$country==i],method = c("pearson"), na.action="na.omit")$p.value
    pcorlag5[pcorlag5$country==i,]$residential[l] <-
      cor.test(lag(mobil5$residential[mobil5$country==i],l),mobil5$R[mobil5$country==i],method = c("pearson"), na.action="na.omit")$p.value
    pcorlag5[pcorlag5$country==i,]$work[l] <-
      cor.test(lag(mobil5$work[mobil5$country==i],l),mobil5$R[mobil5$country==i],method = c("pearson"), na.action="na.omit")$p.value
    pcorlag5[pcorlag5$country==i,]$parks[l] <-
      cor.test(lag(mobil5$parks[mobil5$country==i],l),mobil5$R[mobil5$country==i],method = c("pearson"), na.action="na.omit")$p.value
    
  }
}

pdf(file="Export/pcorlag5.pdf") 
for (i in unique(pcorlag5$country)){
  plot(retail~lag,data=pcorlag5[pcorlag5$country==i,],ty="l",main=paste0(i," 20.02.20 to 31.12.20"),
      ylim=c(0,1),xlab="Lag (days)",ylab="p-value of Pearson r (Rt ~ mobility)")
  abline(h=0.05,lty=3)
  lines(grocery~lag,data=pcorlag5[pcorlag5$country==i,],ty="l",col=2)
  lines(transit~lag,data=pcorlag5[pcorlag5$country==i,],ty="l",col=3)
  lines(residential~lag,data=pcorlag5[pcorlag5$country==i,],ty="l",col=4)
  lines(work~lag,data=pcorlag5[pcorlag5$country==i,],ty="l",col=5)
  lines(parks~lag,data=pcorlag5[pcorlag5$country==i,],ty="l",col=6)
  legend("bottomright",legend = c("retail","grocery","transit","residential","work","parks"),
         lty=1, col=c(1,2,3,4,5,6),cex=0.7)
  mtext("0.05",side=2,line = 0, at=0.05, cex=0.7)
}
dev.off() 

#### 14. Table ####
table1 <- data.frame(country=c("Group 1",grp1,"Group 2",grp2, "Group 3",grp3),
                     cor0=NA,cor0.p=NA,cor14=NA,cor14.p=NA,rsq0l=NA,rsq0q=NA,rsq14l=NA,rsq14q=NA)

for (i in unique(mobil5$country)) {
  table1$cor0[table1$country==i] <- 
    cor(mobil5$residential[mobil5$country==i],mobil5$R[mobil5$country==i],use="complete.obs")
  table1$cor0.p[table1$country==i] <- 
    cor.test(mobil5$residential[mobil5$country==i],mobil5$R[mobil5$country==i],
             method = c("pearson"), na.action="na.omit")$p.value
  
  table1$cor14[table1$country==i] <- 
    cor(lag(mobil5$residential[mobil5$country==i],14),mobil5$R[mobil5$country==i],use="complete.obs")
  table1$cor14.p[table1$country==i] <- 
    cor.test(lag(mobil5$residential[mobil5$country==i],14),mobil5$R[mobil5$country==i],
             method = c("pearson"), na.action="na.omit")$p.value
  
  table1$rsq0l[table1$country==i] <- summary(lm(R~residential,data=data.frame(
    residential=mobil5$residential[mobil5$country==i],R=mobil5$R[mobil5$country==i])))$adj.r.squared
  
  table1$rsq14l[table1$country==i] <- summary(lm(R~residential,data=data.frame(
    residential=lag(mobil5$residential[mobil5$country==i],14),R=mobil5$R[mobil5$country==i])))$adj.r.squared
  
  table1$rsq0q[table1$country==i] <- summary(lm(R~residential+I(residential^2),data=data.frame(
    residential=mobil5$residential[mobil5$country==i],R=mobil5$R[mobil5$country==i])))$adj.r.squared
  
  table1$rsq14q[table1$country==i] <- summary(lm(R~residential+I(residential^2),data=data.frame(
    residential=lag(mobil5$residential[mobil5$country==i],14),R=mobil5$R[mobil5$country==i])))$adj.r.squared
}

write.csv(table1,"Export/table1.csv",row.names = FALSE, na = "")

