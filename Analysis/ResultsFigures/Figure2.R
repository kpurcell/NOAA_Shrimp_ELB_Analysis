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
tiff(filename="C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure\\Figure2.tif",
     width=6.83, height=8, units="in", res=300)

par(mfrow=c(2,1))

#Get slope coefficients for  LA model
pred<-predict(m.2mo.dur.la, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.dur.la$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.dur.la$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.dur.la$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.dur.la$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]
max.do<-max(abs(pred.slope.do))

# plot model output surface predictions
vis.gam(m.2mo.dur.la, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.2,
        main="", ylab="Latitude", xlab="Longitude")


#plot symbols that = significant slopes
symbols(gam.2mo.dur.la$cent_lon[sign.slope.pos.do],gam.2mo.dur.la$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],
        inches=0.15*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,
        add=T,fg='white',bg='white')
symbols(gam.2mo.dur.la$cent_lon[sign.slope.neg.do],gam.2mo.dur.la$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),
        inches=0.15*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,
        add=T,fg='blue',bg='blue')

# add coastline
map("worldHires", fill=T, col="grey",add=T)



#Get slope coefficients for TX model
pred<-predict(m.2mo.dur.tx, type='terms', se.fit=T)
pred.slope.do<-pred[[1]][,8]/gam.2mo.dur.tx$do #number is the column or specific term you want to produce plots for
pred.slope.se.do<-1.96*pred[[2]][,8]/gam.2mo.dur.tx$do
pred.slope.up.do<-(pred[[1]][,8]+pred.slope.se.do)/gam.2mo.dur.tx$do
pred.slope.low.do<-(pred[[1]][,8]-pred.slope.se.do)/gam.2mo.dur.tx$do
sign.slope.pos.do<-(1:length(pred.slope.do))[pred.slope.low.do>0]
sign.slope.neg.do<-(1:length(pred.slope.do))[pred.slope.up.do<0]
max.do<-max(abs(pred.slope.do))


# plot model output surface predications
vis.gam(m.2mo.dur.tx, view=c("cent_lon", "cent_lat"), 
        plot.type="contour", type="response", contour="black",
        color="gray", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")

# plot symbols that = significant slope effects
symbols(gam.2mo.dur.tx$cent_lon[sign.slope.pos.do],gam.2mo.dur.tx$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.15*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='white',bg='white')
symbols(gam.2mo.dur.tx$cent_lon[sign.slope.neg.do],gam.2mo.dur.tx$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.15*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='blue',bg='blue')

# add coastline
map("worldHires", fill=T, col="grey",add=T)





