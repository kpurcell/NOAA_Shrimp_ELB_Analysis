###########################################################
# NOAA_Shrimp_ELB_GAM_Analysis
# Script for Fig. 1 - a comparison of 1D smooth terms for both LA and TX
# Inputs will come from non-spatial models due to their inclusion of DO as a smooth
#
# Code by: KM Purcell
# updated: 2013-12-13
###########################################################
bitmap("C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure\\Figure1.tiff", 
       height = 9.19, width = 5, units = 'in', type="tifflzw", res=300)


windows(width=5, height=9.19)
par(mfrow=c(4,2))

#DO
plot(nspm.2mo.dur.la, select=1, shade=T, all.terms=T, scale=0,
     xlab="Dissolved Oxygen (mg l ^-1)",
     ylab="s(Dissolved Oxygen)")
text(6.2, 0.12, "(a)", cex=1,font=2)
plot(nspm.2mo.dur.tx, select=1, shade=T, all.terms=T, scale=0,
     xlab="Dissolved Oxygen (mg l^-1",
     ylab="")
text(6.7, 0.19, "(b)", cex=1,font=2)


#Depth
plot(nspm.2mo.dur.la, select=2, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)",
     ylab="Effect of Depth")
text(95, 0.41, "(c)", cex=1,font=2)
plot(nspm.2mo.dur.tx, select=2, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)",
     ylab="")
text(82, 0.48, "(d)", cex=1,font=2)


#Price
plot(nspm.2mo.dur.la, select=3, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)",
     ylab="Effect of Price")
text(5.8, 0.2, "(e)", cex=1,font=2)
plot(nspm.2mo.dur.tx, select=3, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)",
     ylab="")
text(5.2, 0.22, "(f)", cex=1,font=2)


#Day of Year
plot(nspm.2mo.dur.la, select=4, shade=T, all.terms=T, scale=0,
     xlab="Julian Day",
     ylab="Effect of Julian Day")
text(241, 0.15, "(g)", cex=1,font=2)
plot(nspm.2mo.dur.tx, select=4, shade=T, all.terms=T, scale=0,
     xlab="Julian Day",
     ylab="")
text(242,0.17, "(h)", cex=1,font=2)
dev.off()