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

#Plot model predictions surfaces for LA model
vis.gam(m2008.la, view=c("cent_lon", "cent_lat"), zlim=c(-1,5),
        plot.type="contour", type="response", contour="black",
        color="topo", n.grid=50, too.far=0.4,
        main="High Hypoxia", ylab="Latitude", xlab="Longitude")
map("worldHires", fill=T, col="grey",add=T)


#Plot model predicted surface for Tx model
vis.gam(m2009.la, view=c("cent_lon", "cent_lat"),zlim=c(-1,5),
        plot.type="contour", type="response", contour="black",
        color="topo", n.grid=50, too.far=0.4,
        main="Low Hypoxia", ylab="Latitude", xlab="Longitude")
#add coastline
map("worldHires", fill=T, col="grey",add=T)


vis.gam(m2008.tx, view=c("cent_lon", "cent_lat"),zlim=c(-1,5),
        plot.type="contour", type="response", contour="black",
        color="topo", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")
#add coastline
map("worldHires", fill=T, col="grey",add=T)

#Plot model predictions surfaces for LA model
vis.gam(m2009.tx, view=c("cent_lon", "cent_lat"), zlim=c(-1,5),
        plot.type="contour", type="response", contour="black",
        color="topo", n.grid=50, too.far=0.4,
        main="", ylab="Latitude", xlab="Longitude")

#add coastline
map("worldHires", fill=T, col="grey",add=T)




