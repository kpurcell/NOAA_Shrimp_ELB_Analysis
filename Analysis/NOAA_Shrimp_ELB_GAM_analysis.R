# NOAA Shrimp ELB Analaysis script
# updated: 2013-11-7


#####
#Clear the junk
graphics.off()
rm(list=ls(all=TRUE))

setwd("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis")
source("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Data\\GatherSource\\gatherData.R")
source("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Data\\merge2moData.R")
source("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Data\\merge1wkData.R")

# Dependent packages
library(mgcv)
library(maptools)
library(maps)
library(mapdata)
library(xtable)
library(texreg)



gam.2mo.dur.la<-gam.2mo.dur.la[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","towhours","cent_lon","cent_lat")]    
gam.2mo.dur.la<-na.omit(gam.2mo.dur.la)


m.2mo.dur.la<-gam(log(towhours+1) ~ factor(yr) + la_fuel_price + hrs +
                    + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                    s(cent_lon,cent_lat, by=do),
                  data=gam.2mo.dur.la)

nspm.2mo.dur.la<-gam(log(towhours+1) ~ factor(yr) + la_fuel_price  + hrs 
                     + s(do) + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat),
                     data=gam.2mo.dur.la)

gam.check(m.2mo.dur.la)

summary(nspm.2mo.dur.la)

summary(m.2mo.dur.la)

gam.2mo.dur.tx<-gam.2mo.dur.tx[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","towhours","cent_lon","cent_lat")]    
gam.2mo.dur.tx<-na.omit(gam.2mo.dur.tx)

m.2mo.dur.tx<-gam(log(towhours+1) ~ factor(yr) + la_fuel_price  + hrs 
                  + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                    s(cent_lon,cent_lat, by=do),
                  data=gam.2mo.dur.tx)

nspm.2mo.dur.tx<-gam(log(towhours+1) ~ factor(yr) + la_fuel_price  + hrs 
                     + s(do) + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat),
                     data=gam.2mo.dur.tx)

gam.check(m.2mo.dur.tx)

summary(m.2mo.dur.tx)

#####################################
# Louisiana tow count
gam.2mo.tow.count.la<-gam.2mo.tow.count.la[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","tow.cnt","areaOB","cent_lon","cent_lat")]    
gam.2mo.tow.count.la<-na.omit(gam.2mo.tow.count.la)


m.2mo.tow.count.la<-gam(tow.cnt ~ factor(yr) + la_fuel_price  + hrs   
                        + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                          s(cent_lon,cent_lat, by=do), family=poisson,
                        data=gam.2mo.tow.count.la)

nspm.2mo.tow.count.la<-gam(tow.cnt ~ factor(yr) + la_fuel_price  + hrs   
                           + s(do) + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat)
                           , family=poisson,
                           data=gam.2mo.tow.count.la)

gam.check(m.2mo.tow.count.la)

summary(m.2mo.tow.count.la)

#########################################
# Texas tow count
gam.2mo.tow.count.tx<-gam.2mo.tow.count.tx[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","tow.cnt","cent_lon","cent_lat")]    
gam.2mo.tow.count.tx<-na.omit(gam.2mo.tow.count.tx)

m.2mo.tow.count.tx<-gam(tow.cnt ~ factor(yr) + la_fuel_price  + hrs   
                        + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                          s(cent_lon,cent_lat, by=do), family=poisson,
                        data=gam.2mo.tow.count.tx)
nspm.2mo.tow.count.tx<-gam(tow.cnt ~ factor(yr) + la_fuel_price  + hrs   
                           + s(do) + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat), family=poisson,
                           data=gam.2mo.tow.count.tx)

summary(m.2mo.tow.count.tx)

#############################################
# Louisiana Average Tow Duration
gam.2mo.avg.dur.la<-gam.2mo.avg.dur.la[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","avg.dur","cent_lon","cent_lat")]    
gam.2mo.avg.dur.la<-na.omit(gam.2mo.avg.dur.la)

m.2mo.avg.dur.la<-gam(avg.dur ~ factor(yr) + la_fuel_price  + hrs   
                      + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                        s(cent_lon,cent_lat, by=do), data=gam.2mo.avg.dur.la)

nspm.2mo.avg.dur.la<-gam(avg.dur ~ factor(yr) + la_fuel_price  + hrs   
                         + s(do) + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat),
                         data=gam.2mo.avg.dur.la)

summary(m.2mo.avg.dur.la)


######################################
# Texas tow count
gam.2mo.avg.dur.tx<-gam.2mo.avg.dur.tx[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","avg.dur","cent_lon","cent_lat")]    
gam.2mo.avg.dur.tx<-na.omit(gam.2mo.avg.dur.tx)

m.2mo.avg.dur.tx<-gam(avg.dur ~ factor(yr) + la_fuel_price  + hrs   
                      + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                        s(cent_lon,cent_lat, by=do), 
                      data=gam.2mo.avg.dur.tx)

nspm.2mo.avg.dur.tx<-gam(avg.dur ~ factor(yr) + la_fuel_price  + hrs   
                         + s(do) + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat), 
                         data=gam.2mo.avg.dur.tx)

summary(m.2mo.avg.dur.tx)

# Single Year models for vis.gam comparison

la.2008<-subset(gam.2mo.dur.la, gam.2mo.dur.la$yr==2008)
tx.2008<-subset(gam.2mo.dur.tx, gam.2mo.dur.tx$yr==2008)
la.2009<-subset(gam.2mo.dur.la, gam.2mo.dur.la$yr==2009)
tx.2009<-subset(gam.2mo.dur.tx, gam.2mo.dur.tx$yr==2009)

m2008.la<-gam(log(towhours+1) ~ la_fuel_price + hrs +
                + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                s(cent_lon,cent_lat, by=do),
              data=la.2008)
m2008.tx<-gam(log(towhours+1) ~ la_fuel_price + hrs +
                + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                s(cent_lon,cent_lat, by=do),
              data=tx.2008)
m2009.la<-gam(log(towhours+1) ~ la_fuel_price + hrs +
                + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                s(cent_lon,cent_lat, by=do),
              data=la.2009)
m2009.tx<-gam(log(towhours+1) ~ la_fuel_price + hrs +
                + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                s(cent_lon,cent_lat, by=do),
              data=tx.2009)

# # ADD source files to the three figure scripts
source("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Analysis\\ResultsFigures\\Figure1.R")
# source("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Analysis\\ResultsFigures\\Figure2.R")
# source("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Analysis\\ResultsFigures\\Figure3.R")
# source("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Analysis\\ResultsFigures\\Figure4.R")
# 
# # Summary table requested by JKC
# # 
# #Coded to pull together the table data
# TABLEINPUT<-subset(elb.2mo.dat, elb.2mo.dat$yr<=2010)
# 
# YEAR<-unique(TABLEINPUT$yr)
# 
# #this should be a loop but no time
# input04<-subset(TABLEINPUT, TABLEINPUT$yr==2004)
# input05<-subset(TABLEINPUT, TABLEINPUT$yr==2005)
# input06<-subset(TABLEINPUT, TABLEINPUT$yr==2006)
# input07<-subset(TABLEINPUT, TABLEINPUT$yr==2007)
# input08<-subset(TABLEINPUT, TABLEINPUT$yr==2008)
# input09<-subset(TABLEINPUT, TABLEINPUT$yr==2009)
# input10<-subset(TABLEINPUT, TABLEINPUT$yr==2010)
# VESSEL2004<-length(unique(input04$vesselID))
# VESSEL2005<-length(unique(input05$vesselID))
# VESSEL2006<-length(unique(input06$vesselID))
# VESSEL2007<-length(unique(input07$vesselID))
# VESSEL2008<-length(unique(input08$vesselID))
# VESSEL2009<-length(unique(input09$vesselID))
# VESSEL2010<-length(unique(input10$vesselID))
# 
# 
# VESSELNUM<-c(VESSEL2004,VESSEL2005,VESSEL2006,VESSEL2007,VESSEL2008,VESSEL2009,VESSEL2010)
# TOWNUMIN<-aggregate(towhours~yr, data=TABLEINPUT, FUN=length)
# TOWNUM<-TOWNUMIN$towhours
# TOTEFFORT<-shrcom.yr$hrs
# AREAHYPOXIC<-hypoxia.dat$areaOB[hypoxia.dat$yr>=2004&hypoxia.dat$yr<=2010]
# 
# TABLEDATA<-cbind(YEAR,VESSELNUM,TOWNUM,TOTEFFORT,AREAHYPOXIC)
# 
# #sort TABLEDATA by YEAR ????
# TABLE1<-xtable(TABLEDATA)
# sink("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\table1.tex")
# print.xtable(TABLE1, type="latex")
# unlink("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\table1.tex")
# sink(NULL)
# 
# screenreg(list(nspm.2mo.dur.la,m.2mo.dur.la,nspm.2mo.dur.tx,m.2mo.dur.tx), 
#           custom.model.names=c("NS-LA","SP-LA", "NS-TX", "SP-TX"),
#           custom.coef.names=c("Intercept","2005","2006","2007","2008","2009",
#                               "2010", "Fuel Price", "Fishery Effort",
#                               "s(DO)","s(Depth)","s(Price)","s(Julian Day)",
#                               "s(Lon., Lat.)", "s(Lon.,Lat):DO"),
#           single.row=TRUE)
# 
# #Regression results table
# sink("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\table2.tex")
# texreg(list(nspm.2mo.dur.la,m.2mo.dur.la,nspm.2mo.dur.tx,m.2mo.dur.tx), 
#        custom.model.names=c("NS-LA","SP-LA", "NS-TX", "SP-TX"),
#        custom.coef.names=c("Intercept","2005","2006","2007","2008","2009",
#                            "2010", "Fuel Price", "Fishery Effort",
#                            "s(DO)","s(Depth)","s(Price)","s(Julian Day)",
#                            "s(Lon., Lat.)", "s(Lon.,Lat):DO"),
#        single.row=TRUE,
#        use.packages=TRUE)
# unlink("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\table2.tex")
# sink(NULL)
# 
# ## Due to problems with the poisson model for tow counts and no intercept values the second part of this table
# ## was added by hand to the latex file.
# # sink("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\table3.tex")
# # texreg(list(m.2mo.avg.dur.la,m.2mo.avg.dur.tx), 
# #                custom.model.names=c("Average Duration-LA","Average Duration-TX"),
# #                custom.coef.names=c("Intercept","2005","2006","2007","2008","2009",
# #                                    "2010", "Fuel Price", "Fishery Effort",
# #                                    "s(Depth)","s(Price)","s(Julian Day)",
# #                                    "s(Lon., Lat.)", "s(Lon.,Lat):DO"),
# #                single.row=TRUE,
# #                use.packages=TRUE)
# # unlink("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\table3.tex")
# # sink(NULL)
# ```
# 
# 
# 
# 
# 
# 
# 
# sessionInfo()
# #save.image(file=paste(Sys.Date(), ".RData", sep=""))

