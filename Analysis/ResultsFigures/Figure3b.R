###########################################################
# NOAA_Shrimp_ELB_GAM_Analysis
# Script for Fig. 3A - a comparison of 1D smooth terms for both LA and TX
# Inputs will come from non-spatial models for avg tow duration for the DO term
#
# Code by: KM Purcell
# updated: 2013-12-10
# TODO -> 
###########################################################
tiff(filename="C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure\\Figure1.tif",
     width=6.83, height=9.19, units="in", res=300)

windows(width=8, height=12)
par(mfrow=c(2,1))

#DO
plot(nspm.2mo.tow.count.la, select=1, shade=T, all.terms=T, scale=0,
     xlab="Dissolved Oxygen (mg l ^-1)",
     ylab="Effect of Dissolved Oxygen")
abline(h=0)
text(3, -2, "A", cex=1.5,font=2)
plot(nspm.2mo.tow.count.tx, select=1, shade=T, all.terms=T, scale=0,
     xlab="Dissolved Oxygen (mg l^-1",
     ylab="")
abline(h=0)
text(3, -2, "B", cex=1.5,font=2)



