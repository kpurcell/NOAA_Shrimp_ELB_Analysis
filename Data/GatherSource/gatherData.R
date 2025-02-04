### Data gathering script for Shrimp ELB Analysis
# updated: 2013-11-7


library(foreign)
library(lattice)


#import new dbf file from spatial join off polygon 2 points
#this data attaches centroid locations for 10x10 min grid cells with all 
#6-8 elb data falling within those polygons
elb.2mo.dat<-read.dbf(file="C:/Users/kevin.purcell/Documents/NOAA_SpatialEffort_MS/mapping_2013-3-5/poly2point_2mo_sz.dbf", as.is = FALSE)
#eliminate a small number of tows in deep water
elb.2mo.dat<-subset(elb.2mo.dat, elb.2mo.dat$depth<=100)

#remove data points where centroid data is missing
elb.2mo.dat<- subset(elb.2mo.dat, elb.2mo.dat$cent_lon!=0)
elb.2mo.dat<- subset(elb.2mo.dat, elb.2mo.dat$ZONE!=0)


#rename some columns in the new dataset
colnames(elb.2mo.dat)[12]<-'grid_cell'
colnames(elb.2mo.dat)[3]<-'vesselID'
colnames(elb.2mo.dat)[10]<-'depth'
colnames(elb.2mo.dat)[17]<-'sz'
elb.2mo.dat$FID_1<-NULL
elb.2mo.dat$FID_3<-NULL
elb.2mo.dat$FID_1_1<-NULL
elb.2mo.dat$Id<-NULL

#create date stuff
#create a calendar week vector
elb.2mo.dat$DATE<-paste(elb.2mo.dat$yr, elb.2mo.dat$mon, sep="-")
elb.2mo.dat$DATE<-paste(elb.2mo.dat$DATE, elb.2mo.dat$day, sep="-")
#elb.2mo.dat$DATE <- as.POSIXlt(elb.2mo.dat$DATE)
elb.2mo.dat$CW <- strftime(elb.2mo.dat$DATE,format="%W")
elb.2mo.dat$jd <- strftime(elb.2mo.dat$DATE, format="%j")
elb.2mo.dat$CW<-as.numeric(elb.2mo.dat$CW)
elb.2mo.dat$jd<-as.numeric(elb.2mo.dat$jd)

# NOTES ##
# This dataset was preprocessed in GIS by:
# * Importing the raw ELB data and spatially mapping fishing locations
# * Overlaying using the fishnet function in arcCatalog a 10 x 10 min grid
# * These two layers were joined by assinging each fishing location to a grid cell
# * SEAMAP survey environmental and depth data were imported into arcMAP
# * polygon fishing effort was match with environmental data at 2 mon resolution
# * ADD MORE METHODS

elb.1wk.dat<-read.dbf(file="C:/Users/kevin.purcell/Documents/NOAA_SpatialEffort_MS/mapping_2013-3-5/poly2point_1wk.dbf",
                      as.is = FALSE)

#remove data points where centroid data is missing
elb.1wk.dat<- subset(elb.1wk.dat, elb.1wk.dat$cent_lon!=0)

#rename some columns in the new dataset

colnames(elb.1wk.dat)[17]<-'grid_cell'
colnames(elb.1wk.dat)[2]<-'vesselID'
elb.1wk.dat$FID_1<-NULL
elb.1wk.dat$Id<-NULL


#eliminate a small number of tows in deep water
elb.1wk.dat<-subset(elb.1wk.dat, elb.1wk.dat$depth<=100)


# #The effort of the fishery is a factor in the model.  We have two sources of data for fishery effort the ELB and the SHRCOM data.
# tot.eff<-aggregate(towhours ~ yr, data=elb.2mo.dat, FUN="sum") #ELB eff agg per JCK discuss
# colnames(tot.eff)[2]<-'tot.eff'
# 
# ###
# windows(width=12,height=8,record=T)
# plot(tot.eff$tot.eff~ tot.eff$yr, type="b",
#      ylab="Shrimping Effort (tow hours)",
#      xlab="Year",
#      main="ELB Effort Time Series")
# grid(col="gray")
# ###


#ELB data on effort shows an increase from 2004-2009 with a big decline in 2010 and then a subsequent increase in 2010.  My thinking is that this represents sampling rather than the behavior of the fishery.  

# import shrcom time series and examine
shrcom.dat<-read.csv("C:/Users/kevin.purcell/Documents/comm_analysis/offshore_6011.csv", sep=",",header=T)
# convert effort in SHRCOM to hours rather than days fished
shrcom.dat$hrs<-(shrcom.dat$effort*24)
# limit the season
shrcom.sum<-subset(shrcom.dat, shrcom.dat$Season==2)
# limit the area to Louisiana (3) and Texas (4)
shrcom.sum<-subset(shrcom.dat, shrcom.dat$area>=3)
# aggregate the effort to a yearly time series
shrcom.yr<-aggregate(hrs ~ yr, data=shrcom.sum, FUN="sum")
# limit the time series to match the elb data
shrcom.yr<-subset(shrcom.yr, shrcom.yr$yr>=2004&shrcom.yr$yr<=2010)
shrcom.mean<-mean(shrcom.yr$hrs)
shrcom.yr$sthrs<-(shrcom.yr$hrs/shrcom.mean)


### Plot shrcom effort time series
# windows(width=12,height=8,record=T)
# plot(shrcom.yr$hrs~ shrcom.yr$yr, type="b",
#      ylab="Shrimping Effort (tow hours)",
#      xlab="Year",
#      main="SHRCOM Effort Time Series")
# grid(col="gray")




# shrimp abundance data itegrated to the 10x10 grid.
abun.dat<-read.csv("C:/Users/kevin.purcell/Documents/NOAA_SpatialEffort_MS/grid2cpue.csv", sep=",",header=T)

abun.dat<-subset(abun.dat, abun.dat$month>=6&abun.dat$month<=8)
#summary(abun.dat)
#create a calendar week vector
abun.dat$DATE<-paste(abun.dat$YR, abun.dat$month, sep="-")
abun.dat$DATE<-paste(abun.dat$DATE, abun.dat$day, sep="-")
#abun.dat$DATE <- as.POSIXlt(abun.dat$DATE)
abun.dat$CW <- strftime(abun.dat$DATE,format="%W")
abun.dat$jd <- strftime(abun.dat$DATE, format="%j")
abun.dat$CW<-as.numeric(abun.dat$CW)
abun.dat$jd<-as.numeric(abun.dat$jd)

abun.avg=aggregate(cpue ~ cent_lon + cent_lat + YR,
                   data=abun.dat, FUN=mean)
#summary(abun.avg)
#boxplot(abun.avg$cpue ~ abun.avg$YR)
#hist(abun.avg$cpue)
#stem(abun.avg$cpue)
#abun.avg<-subset(abun.avg, abun.avg$cpue<10)


#fuel price data
fuel.dat<-read.table("C:/Users/kevin.purcell/Documents/NOAA_SpatialEffort_MS/la_fuel_price.csv", sep=",", header=T)
fuel.dat$DATE<-paste(fuel.dat$yr, fuel.dat$mo, sep="-")
fuel.dat$DATE<-paste(fuel.dat$DATE, fuel.dat$day, sep="-")
fuel.dat$CW <- strftime(fuel.dat$DATE,format="%W")
fuel.dat$jd <- strftime(fuel.dat$DATE, format="%j")
fuel.dat$CW<-as.numeric(fuel.dat$CW)
fuel.dat$jd<-as.numeric(fuel.dat$jd)
fuel.agg<-aggregate(la_fuel_price ~ yr, data=fuel.dat, FUN=mean)

###
# windows(width=12,height=8,record=T)
# boxplot(fuel.dat$la_fuel_price~fuel.dat$yr,
#         main="Annual Fuel Price Variation")
# grid(col="gray")

###
# windows(width=12,height=8,record=T)
# boxplot(fuel.dat$la_fuel_price~fuel.dat$CW,
#         xlab="Calendar Week",
#         ylab="Fuel Price",
#         main="Fuel Price Variation")
# grid(col="gray")

# Could plot out price by week a panel for each year-->use a loop

#price data
ppnd.dat=read.csv("C:/Users/kevin.purcell/Documents/NOAA_SpatialEffort_MS/ppnd.csv", sep=",",header=T)
#summary(ppnd.dat)
ppnd.dat$PRICEPPND<-ppnd.dat$PRICEPPND/100
colnames(ppnd.dat)[1]<-'sz'
colnames(ppnd.dat)[2]<-'yr'
colnames(ppnd.dat)[5]<-'ppnd'
#limit ppnd data
ppnd.dat<-subset(ppnd.dat, ppnd.dat$ppnd<=6)
ppnd.yr<-aggregate(ppnd~yr+CW, data=ppnd.dat, FUN=mean)

# windows(width=12,height=8,record=T)
# boxplot(ppnd.dat$ppnd~ppnd.dat$CW,
#         main="Shrimp Price Variation by CW")
# grid(col="gray")

###---###---###---###
## Hypoxia DATA     #
###---###---###---###

hypoxia.dat=read.table("C:/Users/kevin.purcell/Documents/NOAA_SpatialEffort_MS/analysis/hypoxia_metrics.csv", sep=",",header=T)
colnames(hypoxia.dat)[1]<-'yr'
#names(hypoxia.dat)

# windows(width=12,height=8,record=T)
# plot(hypoxia.dat$areaNR~hypoxia.dat$yr, type="p", pch=15, col="red",
#      ylab="Areial Extent of Hypoxia (sq km)",
#      xlab="Year",
#      main="Area of Hypoxia Time Series")
# lines(hypoxia.dat$areaOB~hypoxia.dat$yr, type="p", pch=16, col="blue")
# grid(col="gray")
# legend("bottomright", pch=c(15,16),col=c("red","blue"), legend=c("Rabalais","Obenour"), bty="n")
hypoxia.dat$areaNR<-NULL
hypoxia.dat$volOB<-NULL

## END OF PROGRAM



## TODOs ########
















## Really this is the end 
