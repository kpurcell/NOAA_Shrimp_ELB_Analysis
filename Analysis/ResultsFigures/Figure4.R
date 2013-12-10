###########################################################
# NOAA_Shrimp_ELB_GAM_Analysis
# Script for Fig. 4 - a comparison of 1D  smooth terms for both LA and TX alternate response variables
# Inputs will come from spatial models and include slopes
#
# Code by: KM Purcell
# updated: 2013-12-10
# TODO -> adjust lat scal on LA plots to make room for a label in the upper left
#         Make room in left hand margin for Region???
#         
###########################################################
#tiff(filename="C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure\\Figure1.tif",
#     width=6.83, height=9.19, units="in", res=300)

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

