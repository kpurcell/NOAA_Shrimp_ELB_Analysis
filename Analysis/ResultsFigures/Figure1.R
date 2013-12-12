###########################################################
# NOAA_Shrimp_ELB_GAM_Analysis
# Script for Fig. 1 - a comparison of 1D smooth terms for both LA and TX
# Inputs will come from non-spatial models due to their inclusion of DO as a smooth
#
# Code by: KM Purcell
# updated: 2013-12-4
###########################################################
#tiff(filename="C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure\\Figure1.tif",
#    width=6.83, height=9.19, units="in", res=300)
windows(width=8, height=12)
par(mfrow=c(4,2))

#DO
plot(nspm.2mo.dur.la, select=1, shade=T, all.terms=T, scale=0,
     xlab="Dissolved Oxygen (mg l ^-1)",
     ylab="Effect of Dissolved Oxygen")
text(3, -2, "A", cex=1.5,font=2)
plot(nspm.2mo.dur.tx, select=1, shade=T, all.terms=T, scale=0,
     xlab="Dissolved Oxygen (mg l^-1",
     ylab="")
text(3, -2, "B", cex=1.5,font=2)


#Depth
plot(nspm.2mo.dur.la, select=2, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)",
     ylab="Effect of Depth")
text(3, -2, "C", cex=1.5,font=2)
plot(nspm.2mo.dur.tx, select=2, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)",
     ylab="")
text(3, -2, "D", cex=1.5,font=2)


#Price
plot(nspm.2mo.dur.la, select=3, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)",
     ylab="Effect of Price")
text(0.4, -0.6, "E", cex=1.5,font=2)
plot(nspm.2mo.dur.tx, select=3, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)",
     ylab="")
text(0.4, -0.6, "F", cex=1.5,font=2)


#Day of Year
plot(nspm.2mo.dur.la, select=4, shade=T, all.terms=T, scale=0,
     xlab="Julian Day",
     ylab="Effect of Julian Day")
text(0.4, -0.6, "G", cex=1.5,font=2)
plot(nspm.2mo.dur.tx, select=4, shade=T, all.terms=T, scale=0,
     xlab="Julian Day",
     ylab="")
text(0.4, -0.6, "H", cex=1.5,font=2)
#dev.off()