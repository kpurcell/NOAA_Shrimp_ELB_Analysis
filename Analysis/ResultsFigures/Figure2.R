###########################################################
# NOAA_Shrimp_ELB_GAM_Analysis
# Script for Fig. 2 - a comparison of 2D spatial smooth terms for both LA and TX
# Inputs will come from spatial models and include slopes
#
# Code by: KM Purcell
# updated: 2013-12-13
# TODO -> add letters, add symbol scale, standardize surface contour colors
#         add code for pdf creation
###########################################################
postscript(file="C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure\\Figure2.ps", 
           horizontal=F,
           onefile=F,
           width=3.27, height=9.19,
           family="Arial",
           pointsize=12)

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
        color="topo", n.grid=50, too.far=0.4, zlim=c(0.5,4.5),
        main="", ylab="Latitude", xlab="Longitude", ylim=c(27.5,30))


#plot symbols that = significant slopes
symbols(gam.2mo.dur.la$cent_lon[sign.slope.pos.do],gam.2mo.dur.la$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],
        inches=0.15*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,
        add=T,fg='black', bg='black')
symbols(gam.2mo.dur.la$cent_lon[sign.slope.neg.do],gam.2mo.dur.la$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),
        inches=0.15*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,
        add=T,fg='red', bg='red')

# add coastline
map("worldHires", fill=T, col="grey",add=T)
# add text label
text(-93.6, 29.9, "(a)", cex=1,font=2)

#add bubble legend
symbols(rep(-89.2,4),c(27.65,27.85,28.05,28.25),
        circles=seq(max.do,max.do/5,length=4),add=T,inches=0.10, fg="black")
text(-89.5,c(27.65,27.85,28.05,28.25),format(round(seq(max.do,max.do/5,length=4),2)), cex=0.8)



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
        color="topo", n.grid=50, too.far=0.4, zlim=c(0.5,4.5),
        main="", ylab="Latitude", xlab="Longitude", ylim=c(25.3,29.8))

# plot symbols that = significant slope effects
symbols(gam.2mo.dur.tx$cent_lon[sign.slope.pos.do],gam.2mo.dur.tx$cent_lat[sign.slope.pos.do],
        circle=pred.slope.do[sign.slope.pos.do],inches=0.15*max(range(pred.slope.do[sign.slope.pos.do]))/max.do,add=T,fg='black', bg='black')
symbols(gam.2mo.dur.tx$cent_lon[sign.slope.neg.do],gam.2mo.dur.tx$cent_lat[sign.slope.neg.do],
        circle=abs(pred.slope.do[sign.slope.neg.do]),inches=0.15*max(range(pred.slope.do[sign.slope.pos.do], finite=T))/max.do,add=T,fg='red', bg='red')

# add coastline
map("worldHires", fill=T, col="grey",add=T)
# add text label
text(-97.14, 29.55, "(b)", cex=1,font=2)

#add bubble legend
symbols(rep(-94,4),c(25.6,25.9,26.2,26.5),
        circles=seq(max.do,max.do/5,length=4),add=T,inches=0.10, fg="black")
text(-94.2,c(25.6,25.9,26.2,26.5),format(round(seq(max.do,max.do/5,length=4),2)), cex=0.8)
dev.off()




