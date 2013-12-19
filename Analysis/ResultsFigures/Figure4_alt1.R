# Testing alternative approach to tow density and avg tow duratino
la.avg.dur.2008<-subset(gam.2mo.avg.dur.la, gam.2mo.avg.dur.la$yr==2008)
la.avg.dur.2009<-subset(gam.2mo.avg.dur.la, gam.2mo.avg.dur.la$yr==2009)
la.tow.count.2008<-subset(gam.2mo.tow.count.la, gam.2mo.tow.count.la$yr==2008)
la.tow.count.2009<-subset(gam.2mo.tow.count.la, gam.2mo.tow.count.la$yr==2009)


avg.dur.2008<-gam(avg.dur ~ la_fuel_price + hrs +
                + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                s(cent_lon,cent_lat, by=do),
                data=la.avg.dur.2008)
avg.dur.2009<-gam(avg.dur ~ la_fuel_price + hrs +
                + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                s(cent_lon,cent_lat, by=do),
                data=la.avg.dur.2009)
tow.cnt.2008<-gam(tow.cnt ~ la_fuel_price + hrs +
                + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                s(cent_lon,cent_lat, by=do), family=poisson,
                data=la.tow.count.2008)
tow.cnt.2009<-gam(tow.cnt ~ la_fuel_price + hrs +
                + s(depth) + s(ppnd) + s(jd) + s(cent_lon,cent_lat) + 
                s(cent_lon,cent_lat, by=do), family=poisson,
                data=la.tow.count.2009)

windows(width=12, height=12)
par(mfrow=c(2,2))

#Plot model predictions surfaces for LA model
vis.gam(avg.dur.2008, view=c("cent_lon", "cent_lat"),zlim=c(3,9),
        plot.type="contour", type="response", contour="black",
        color="topo", n.grid=50, too.far=0.2,
        main="High Hypoxia", ylab="Latitude", xlab="Longitude")
map("worldHires", fill=T, col="grey",add=T)
# add text label
text(-93.6, 29.82, "(a)", cex=1,font=2)

#Plot model predicted surface for Tx model
vis.gam(avg.dur.2009, view=c("cent_lon", "cent_lat"),zlim=c(3,9),
        plot.type="contour", type="response", contour="black",
        color="topo", n.grid=50, too.far=0.4,
        main="Low Hypoxia", ylab="Latitude", xlab="Longitude")
#add coastline
map("worldHires", fill=T, col="grey",add=T)
# add text label
text(-93.6, 29.82, "(b)", cex=1,font=2)

vis.gam(tow.cnt.2008, view=c("cent_lon", "cent_lat"),zlim=c(-1,30),
        plot.type="contour", type="response", contour="black",
        color="topo", n.grid=50, too.far=0.2,
        main="", ylab="Latitude", xlab="Longitude")
#add coastline
map("worldHires", fill=T, col="grey",add=T)
# add text label
text(-93.6, 29.82, "(c)", cex=1,font=2)
#Plot model predictions surfaces for LA model
vis.gam(tow.cnt.2009, view=c("cent_lon", "cent_lat"), zlim=c(-1,30),
        plot.type="contour", type="response", contour="black",
        color="topo", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")

#add coastline
map("worldHires", fill=T, col="grey",add=T)
# add text label
text(-93.6, 29.82, "(d)", cex=1,font=2)




