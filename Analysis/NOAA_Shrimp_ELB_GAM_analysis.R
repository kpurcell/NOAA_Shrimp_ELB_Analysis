# NOAA Shrimp ELB Analaysis script
# updated: 2013-11-7


#####
#Clear the junk
graphics.off()
rm(list=ls(all=TRUE))


source("C:\\Users\\Kevin.Purcell\\Documents\\NOAA_Shrimp_ELB_Analysis\\Data\\GatherSource\\gatherData.R")
source("C:\\Users\\Kevin.Purcell\\Documents\\NOAA_Shrimp_ELB_Analysis\\Data\\merge2moData.R")
source("C:\\Users\\Kevin.Purcell\\Documents\\NOAA_Shrimp_ELB_Analysis\\Data\\merge1wkData.R")

# Dependent packages
library(mgcv)
library(maptools)
library(maps)
library(mapdata)

###########################################

summary(gam.1wk.dur)
gam.1wk.dur<-subset(gam.1wk.dur, gam.1wk.dur$sz<=21)
gam.1wk.dur<-gam.1wk.dur[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","towhours","cent_lon","cent_lat")]    
#remove na's
gam.1wk.dur<-na.omit(gam.1wk.dur)

# gam.1wk.dur.short<-subset(gam.1wk.dur, gam.1wk.dur$yr>=2006)
# gam.1wk.dur.2004<-subset(gam.1wk.dur, gam.1wk.dur$yr==2004)
# gam.1wk.dur.2005<-subset(gam.1wk.dur, gam.1wk.dur$yr==2005)
# gam.1wk.dur.2006<-subset(gam.1wk.dur, gam.1wk.dur$yr==2006)
# gam.1wk.dur.2007<-subset(gam.1wk.dur, gam.1wk.dur$yr==2007)
# gam.1wk.dur.2008<-subset(gam.1wk.dur, gam.1wk.dur$yr==2008)
# gam.1wk.dur.2009<-subset(gam.1wk.dur, gam.1wk.dur$yr==2009)
# gam.1wk.dur.2010<-subset(gam.1wk.dur, gam.1wk.dur$yr==2010)


#GOM model
m.1wk<-gam(log(towhours+1) ~ factor(yr) + la_fuel_price  + hrs + 
             s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) +
             s(cent_lon,cent_lat, by=do), data=gam.1wk.dur)

gam.check(m.1wk)
summary(m.1wk)

#Get slope coefficients for gom2 model
pred<-predict(m.1wk, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,9]/gam.1wk.dur$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,9]/gam.1wk.dur$do
pred.slope.up.do<-(pred[[1]][,9]+pred.slope.se.do)/gam.1wk.dur$do
pred.slope.low.do<-(pred[[1]][,9]-pred.slope.se.do)/gam.1wk.dur$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]


par(mfrow=c(2,2))
hist(pred.slope.do, main='All slopes, do')
hist(pred.slope.se.do, main='Standard error, do')
hist(pred.slope.do[sign.slope.pos.do], main='Significantly positive, do')
hist(pred.slope.do[sign.slope.neg.do], main='Significantly negative, do')

max.do<-max(abs(pred.slope.do))

windows(width=12,height=8,record=T)
par(mfrow=c(2,2))
plot(m.1wk, select=1, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)")
text(.4, -0.72, "A", cex=1.5,font=2)
plot(m.1wk, select=2, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)")
text(3, -2, "B", cex=1.5,font=2)
plot(m.1wk, select=3, shade=T, all.terms=T, scale=0,
     xlab="Julian Day")
vis.gam(m.1wk, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")
symbols(gam.1wk.dur$cent_lon[sign.slope.pos.do],gam.1wk.dur$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.1*max(pred.slope.do[sign.slope.pos.do])/max(max.do),add=T,fg='white',bg='white')
symbols(gam.1wk.dur$cent_lon[sign.slope.neg.do],gam.1wk.dur$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.1*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max(max.do),add=T,fg='blue',bg='blue')
map("worldHires", fill=T, col="grey",add=T)



###
###
###

# gam.2mo.dur<-gam.2mo.dur[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","towhours","areaOB","cent_lon","cent_lat")]    
# gam.2mo.dur<-na.omit(gam.2mo.dur)
# 
# gom2<-gam(log(towhours+1) ~ factor(yr) + la_fuel_price  + hrs + areaOB + 
#             + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
#             s(cent_lon,cent_lat, by=do),
#           data=gam.2mo.dur)
# 
# gam.check(gom2)
# summary(gom2)
# 
# 
# par(mfrow=c(2,2))
# plot(gom2, select=1, shade=T, all.terms=T, scale=0,
#      xlab="Depth (m)")
# text(.4, -0.72, "A", cex=1.5,font=2)
# plot(gom2, select=2, shade=T, all.terms=T, scale=0,
#      xlab="Price per pound (USD)")
# text(3, -2, "B", cex=1.5,font=2)
# plot(gom2, select=3, shade=T, all.terms=T, scale=0,
#      xlab="Julian Day")
# text(0.4, -0.6, "C", cex=1.5,font=2)
# plot(gom2, select=4, se=F, rug=F, scheme=2,
#      xlab="Longitude",
#      ylab="Latitude",
#      main="Effect of space") 
# map("state",fill=T,col="gray",add=T)
# text(1.563, -0.44, "D", cex=1.5,font=2)
# 
# 
# par(mfrow=c(1,2))
# plot(gom2, select=5, se=F, rug=F, scheme=2,
#      xlab="Longitude",
#      ylab="Latitude",
#      main="Effect of space") 
# map("state",fill=T,col="gray",add=T)
# 
# plot(gom2, select=6, se=F, rug=F, scheme=2,
#      xlab="Longitude",
#      ylab="Latitude",
#      main="Effect of do") 
# map("state",fill=T,col="gray",add=T)
# 
# 
# 
# #Get slope coefficients for gom2 model
# pred<-predict(gom2, type='terms')
# pred.slope<-(pred[,10]) #number is the column or specific term you want to produce plots for
# hist(pred.slope, main='All Slopes')
# max.t<-max(abs(pred.slope))
# 
# par(mfrow=c(1,2))
# vis.gam(gom2, view=c("cent_lon", "cent_lat"), 
#         plot.type="contour", type="response", contour="black",
#         color="topo", n.grid=50, too.far=0.2,
#         main="", ylab="Latitude", xlab="Longitude")
# map("worldHires", fill=T, col="grey",add=T)
# 
# 
# 
# plot(gam.2mo.dur$cent_lon, gam.2mo.dur$cent_lat, pch="", 
#      ylab="Latitude", xlab="Longitude")
# # vis.gam(la2b, view=c("cent_lon", "cent_lat"), 
# #         plot.type="contour", type="link", contour="black",
# #         color="gray", n.grid=50, too.far=0.5,
# #         main="", ylab="Latitude", xlab="Longitude")
# symbols(gam.2mo.dur$cent_lon[pred.slope>=0],gam.2mo.dur$cent_lat[pred.slope>=0],
#         circle=pred.slope[pred.slope>=0],inches=0.2*max(pred.slope[pred.slope>=0])/max.t,add=T,fg='red')
# symbols(gam.2mo.dur$cent_lon[pred.slope<=0],gam.2mo.dur$cent_lat[pred.slope<=0],
#         circle=abs(pred.slope[pred.slope<=0]),inches=0.2*max(abs(pred.slope[pred.slope<=0]))/max.t,add=T,fg='blue')
# map("worldHires", fill=T, col="grey",add=T)

#################################
# Louisiana model
gam.2mo.dur.la<-gam.2mo.dur.la[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","towhours","areaOB","cent_lon","cent_lat")]    
gam.2mo.dur.la<-na.omit(gam.2mo.dur.la)

m.2mo.dur.la<-gam(log(towhours+1) ~ factor(yr) + la_fuel_price  + hrs   
                  + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                  s(cent_lon,cent_lat, by=do),
                  data=gam.2mo.dur.la)

gam.check(m.2mo.dur.la)
summary(m.2mo.dur.la)

#Get slope coefficients for gom2 model
pred<-predict(m.2mo.dur.la, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.dur.la$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.dur.la$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.dur.la$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.dur.la$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]


par(mfrow=c(2,2))
hist(pred.slope.do, main='All slopes, do')
hist(pred.slope.se.do, main='Standard error, do')
hist(pred.slope.do[sign.slope.pos.do], main='Significantly positive, do')
hist(pred.slope.do[sign.slope.neg.do], main='Significantly negative, do')

max.do<-max(abs(pred.slope.do))

windows(width=12,height=8,record=T)
par(mfrow=c(2,2))
plot(m.2mo.dur.la, select=1, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)")
text(.4, -0.72, "A", cex=1.5,font=2)
plot(m.2mo.dur.la, select=2, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)")
text(3, -2, "B", cex=1.5,font=2)
plot(m.2mo.dur.la, select=3, shade=T, all.terms=T, scale=0,
     xlab="Julian Day")
text(0.4, -0.6, "C", cex=1.5,font=2)
plot(m.2mo.dur.la, select=1, shade=T, all.terms=T, scale=0,
     xlab="Year")
text(0.4, -0.6, "C", cex=1.5,font=2)
plot(m.2mo.dur.la, all.terms=T)

vis.gam(m.2mo.dur.la, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")
symbols(gam.2mo.dur.la$cent_lon[sign.slope.pos.do],gam.2mo.dur.la$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.2*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.dur.la$cent_lon[sign.slope.neg.do],gam.2mo.dur.la$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.2*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')
map("worldHires", fill=T, col="grey",add=T)

#############################
# Texas model
gam.2mo.dur.tx<-gam.2mo.dur.tx[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","towhours","areaOB","cent_lon","cent_lat")]    
gam.2mo.dur.tx<-na.omit(gam.2mo.dur.tx)

m.2mo.dur.tx<-gam(log(towhours+1) ~ factor(yr) + la_fuel_price  + hrs + areaOB + do +
                    + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                    s(cent_lon,cent_lat, by=do),
                  data=gam.2mo.dur.tx)

gam.check(m.2mo.dur.tx)
summary(m.2mo.dur.tx)

#Get slope coefficients for gom2 model
pred<-predict(m.2mo.dur.tx, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,9]/gam.2mo.dur.tx$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,9]/gam.2mo.dur.tx$do
pred.slope.up.do<-(pred[[1]][,9]+pred.slope.se.do)/gam.2mo.dur.tx$do
pred.slope.low.do<-(pred[[1]][,9]-pred.slope.se.do)/gam.2mo.dur.tx$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]


par(mfrow=c(2,2))
hist(pred.slope.do, main='All slopes, do')
hist(pred.slope.se.do, main='Standard error, do')
hist(pred.slope.do[sign.slope.pos.do], main='Significantly positive, do')
hist(pred.slope.do[sign.slope.neg.do], main='Significantly negative, do')

max.do<-max(abs(pred.slope.do))

windows(width=12,height=8,record=T)
par(mfrow=c(2,2))
plot(m.2mo.dur.tx, select=1, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)")
text(.4, -0.72, "A", cex=1.5,font=2)
plot(m.2mo.dur.tx, select=2, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)")
text(3, -2, "B", cex=1.5,font=2)
plot(m.2mo.dur.tx, select=3, shade=T, all.terms=T, scale=0,
     xlab="Julian Day")
text(0.4, -0.6, "C", cex=1.5,font=2)

vis.gam(m.2mo.dur.tx, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")
symbols(gam.2mo.dur.tx$cent_lon[sign.slope.pos.do],gam.2mo.dur.tx$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.2*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.dur.tx$cent_lon[sign.slope.neg.do],gam.2mo.dur.tx$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.2*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')
map("worldHires", fill=T, col="grey",add=T)

#####################################
# Louisiana tow count
gam.2mo.tow.count.la<-gam.2mo.tow.count.la[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","tow.cnt","areaOB","cent_lon","cent_lat")]    
gam.2mo.tow.count.la<-na.omit(gam.2mo.tow.count.la)

m.2mo.tow.count.la<-gam(tow.cnt ~ factor(yr) + la_fuel_price  + hrs   
                  + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                    s(cent_lon,cent_lat, by=do), family=poisson,
                  data=gam.2mo.tow.count.la)

gam.check(m.2mo.tow.count.la)
summary(m.2mo.tow.count.la)
plot(m.2mo.tow.count.la)

#Get slope coefficients for gom2 model
pred<-predict(m.2mo.tow.count.la, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.tow.count.la$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.tow.count.la$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.tow.count.la$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.tow.count.la$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]


par(mfrow=c(2,2))
hist(pred.slope.do, main='All slopes, do')
hist(pred.slope.se.do, main='Standard error, do')
hist(pred.slope.do[sign.slope.pos.do], main='Significantly positive, do')
hist(pred.slope.do[sign.slope.neg.do], main='Significantly negative, do')

max.do<-max(abs(pred.slope.do))

windows(width=12,height=8,record=T)
par(mfrow=c(2,2))
plot(m.2mo.tow.count.la, select=1, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)")
text(.4, -0.72, "A", cex=1.5,font=2)
plot(m.2mo.tow.count.la, select=2, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)")
text(3, -2, "B", cex=1.5,font=2)
plot(m.2mo.tow.count.la, select=3, shade=T, all.terms=T, scale=0,
     xlab="Julian Day")
text(0.4, -0.6, "C", cex=1.5,font=2)

vis.gam(m.2mo.tow.count.la, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")
symbols(gam.2mo.tow.count.la$cent_lon[sign.slope.pos.do],gam.2mo.tow.count.la$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.2*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.tow.count.la$cent_lon[sign.slope.neg.do],gam.2mo.tow.count.la$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.2*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')
map("worldHires", fill=T, col="grey",add=T)



#########################################
# Texas tow count
gam.2mo.tow.count.tx<-gam.2mo.tow.count.tx[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","tow.cnt","areaOB","cent_lon","cent_lat")]    
gam.2mo.tow.count.tx<-na.omit(gam.2mo.tow.count.tx)

m.2mo.tow.count.tx<-gam(tow.cnt ~ factor(yr) + la_fuel_price  + hrs   
                        + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                          s(cent_lon,cent_lat, by=do), family=poisson,
                        data=gam.2mo.tow.count.tx)

gam.check(m.2mo.tow.count.tx)
summary(m.2mo.tow.count.tx)
plot(m.2mo.tow.count.tx)

#Get slope coefficients for gom2 model
pred<-predict(m.2mo.tow.count.tx, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.tow.count.tx$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.tow.count.tx$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.tow.count.tx$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.tow.count.tx$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]


par(mfrow=c(2,2))
hist(pred.slope.do, main='All slopes, do')
hist(pred.slope.se.do, main='Standard error, do')
hist(pred.slope.do[sign.slope.pos.do], main='Significantly positive, do')
hist(pred.slope.do[sign.slope.neg.do], main='Significantly negative, do')

max.do<-max(abs(pred.slope.do))

windows(width=12,height=8,record=T)
par(mfrow=c(2,2))
plot(m.2mo.tow.count.tx, select=1, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)")
text(.4, -0.72, "A", cex=1.5,font=2)
plot(m.2mo.tow.count.tx, select=2, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)")
text(3, -2, "B", cex=1.5,font=2)
plot(m.2mo.tow.count.tx, select=3, shade=T, all.terms=T, scale=0,
     xlab="Julian Day")
text(0.4, -0.6, "C", cex=1.5,font=2)

vis.gam(m.2mo.tow.count.tx, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")
symbols(gam.2mo.tow.count.tx$cent_lon[sign.slope.pos.do],gam.2mo.tow.count.tx$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.2*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.tow.count.tx$cent_lon[sign.slope.neg.do],gam.2mo.tow.count.tx$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.2*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')
map("worldHires", fill=T, col="grey",add=T)

#############################################
# Louisiana Average Tow Duration
gam.2mo.avg.dur.la<-gam.2mo.avg.dur.la[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","avg.dur","cent_lon","cent_lat")]    
gam.2mo.avg.dur.la<-na.omit(gam.2mo.avg.dur.la)

m.2mo.avg.dur.la<-gam(avg.dur ~ factor(yr) + la_fuel_price  + hrs   
                        + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                          s(cent_lon,cent_lat, by=do), data=gam.2mo.avg.dur.la)

gam.check(m.2mo.avg.dur.la)
summary(m.2mo.avg.dur.la)
plot(m.2mo.avg.dur.la)

#Get slope coefficients for gom2 model
pred<-predict(m.2mo.avg.dur.la, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.avg.dur.la$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.avg.dur.la$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.avg.dur.la$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.avg.dur.la$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]


par(mfrow=c(2,2))
hist(pred.slope.do, main='All slopes, do')
hist(pred.slope.se.do, main='Standard error, do')
hist(pred.slope.do[sign.slope.pos.do], main='Significantly positive, do')
hist(pred.slope.do[sign.slope.neg.do], main='Significantly negative, do')

max.do<-max(abs(pred.slope.do))

windows(width=12,height=8,record=T)
par(mfrow=c(2,2))
plot(m.2mo.avg.dur.la, select=1, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)")
text(.4, -0.72, "A", cex=1.5,font=2)
plot(m.2mo.avg.dur.la, select=2, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)")
text(3, -2, "B", cex=1.5,font=2)
plot(m.2mo.avg.dur.la, select=3, shade=T, all.terms=T, scale=0,
     xlab="Julian Day")
text(0.4, -0.6, "C", cex=1.5,font=2)

vis.gam(m.2mo.avg.dur.la, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")
symbols(gam.2mo.avg.dur.la$cent_lon[sign.slope.pos.do],gam.2mo.avg.dur.la$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.1*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.avg.dur.la$cent_lon[sign.slope.neg.do],gam.2mo.avg.dur.la$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.1*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')
map("worldHires", fill=T, col="grey",add=T)



######################################
# Texas tow count
gam.2mo.avg.dur.tx<-gam.2mo.avg.dur.tx[,c("yr","jd","hrs","do","depth", "ppnd", "la_fuel_price","avg.dur","cent_lon","cent_lat")]    
gam.2mo.avg.dur.tx<-na.omit(gam.2mo.avg.dur.tx)

m.2mo.avg.dur.tx<-gam(avg.dur ~ factor(yr) + la_fuel_price  + hrs   
                        + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                          s(cent_lon,cent_lat, by=do), family=poisson,
                        data=gam.2mo.avg.dur.tx)

gam.check(m.2mo.avg.dur.tx)
summary(m.2mo.avg.dur.tx)
plot(m.2mo.avg.dur.tx)

#Get slope coefficients for gom2 model
pred<-predict(m.2mo.avg.dur.tx, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.avg.dur.tx$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.avg.dur.tx$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.avg.dur.tx$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.avg.dur.tx$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]


par(mfrow=c(2,2))
hist(pred.slope.do, main='All slopes, do')
hist(pred.slope.se.do, main='Standard error, do')
hist(pred.slope.do[sign.slope.pos.do], main='Significantly positive, do')
hist(pred.slope.do[sign.slope.neg.do], main='Significantly negative, do')

max.do<-max(abs(pred.slope.do))

windows(width=12,height=8,record=T)
par(mfrow=c(2,2))
plot(m.2mo.avg.dur.tx, select=1, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)")
text(.4, -0.72, "A", cex=1.5,font=2)
plot(m.2mo.avg.dur.tx, select=2, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)")
text(3, -2, "B", cex=1.5,font=2)
plot(m.2mo.avg.dur.tx, select=3, shade=T, all.terms=T, scale=0,
     xlab="Julian Day")
text(0.4, -0.6, "C", cex=1.5,font=2)

vis.gam(m.2mo.avg.dur.tx, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")
symbols(gam.2mo.avg.dur.tx$cent_lon[sign.slope.pos.do],gam.2mo.avg.dur.tx$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.2*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.avg.dur.tx$cent_lon[sign.slope.neg.do],gam.2mo.avg.dur.tx$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.2*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')
map("worldHires", fill=T, col="grey",add=T)

