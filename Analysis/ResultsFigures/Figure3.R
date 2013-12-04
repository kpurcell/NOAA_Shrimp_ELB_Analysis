###########################################################
# NOAA_Shrimp_ELB_GAM_Analysis
# Script for Fig. 2 - a comparison of 2D spatial smooth terms for both LA and TX
# Inputs will come from spatial models and include slopes
#
# Code by: KM Purcell
# updated: 2013-12-4
# TODO -> add letters, add symbol scale, standardize surface contour colors
#         add code for pdf creation
###########################################################
tiff(filename="C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure\\Figure1.tif",
     width=6.83, height=9.19, units="in", res=300)


par(mfrow=c(2,2))
## Average tow duration

#Get slope coefficients for avg tow dur LA model
pred<-predict(m.2mo.avg.dur.la, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.avg.dur.la$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.avg.dur.la$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.avg.dur.la$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.avg.dur.la$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]
max.do<-max(abs(pred.slope.do))

#Plot model predictions surfaces for LA model
vis.gam(m.2mo.avg.dur.la, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")


# Plot significant slope effects
symbols(gam.2mo.avg.dur.la$cent_lon[sign.slope.pos.do],gam.2mo.avg.dur.la$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.15*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.avg.dur.la$cent_lon[sign.slope.neg.do],gam.2mo.avg.dur.la$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.15*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')

#add coastline
map("worldHires", fill=T, col="grey",add=T)


#Get slope coefficients for avg tow dur TX model
pred<-predict(m.2mo.avg.dur.tx, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.avg.dur.tx$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.avg.dur.tx$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.avg.dur.tx$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.avg.dur.tx$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]
max.do<-max(abs(pred.slope.do))

#Plot model predicted surface for Tx model
vis.gam(m.2mo.avg.dur.tx, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")

#plot significant slopes
symbols(gam.2mo.avg.dur.tx$cent_lon[sign.slope.pos.do],gam.2mo.avg.dur.tx$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.2*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.avg.dur.tx$cent_lon[sign.slope.neg.do],gam.2mo.avg.dur.tx$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.2*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')

#add coastline
map("worldHires", fill=T, col="grey",add=T)

##
## Tow density
##


#Get slope coefficients for tow density LA model
pred<-predict(m.2mo.tow.count.la, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.tow.count.la$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.tow.count.la$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.tow.count.la$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.tow.count.la$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]
max.do<-max(abs(pred.slope.do))


#plot model predicted surface
vis.gam(m.2mo.tow.count.la, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")

#plot significant slope coefficients
symbols(gam.2mo.tow.count.la$cent_lon[sign.slope.pos.do],gam.2mo.tow.count.la$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.2*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.tow.count.la$cent_lon[sign.slope.neg.do],gam.2mo.tow.count.la$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.2*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')

#plot coastline
map("worldHires", fill=T, col="grey",add=T)



#Get slope coefficients for tow density TX model
pred<-predict(m.2mo.tow.count.tx, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.tow.count.tx$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.tow.count.tx$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.tow.count.tx$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.tow.count.tx$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]
max.do<-max(abs(pred.slope.do))

#plot model predictive surface for TX model for tow density
vis.gam(m.2mo.tow.count.tx, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")

#Plot significant slope coefficients
symbols(gam.2mo.tow.count.tx$cent_lon[sign.slope.pos.do],gam.2mo.tow.count.tx$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.2*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.tow.count.tx$cent_lon[sign.slope.neg.do],gam.2mo.tow.count.tx$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.2*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')

#plot coastline
map("worldHires", fill=T, col="grey",add=T)




