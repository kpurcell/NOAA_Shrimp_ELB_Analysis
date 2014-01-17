###########################################################
# NOAA_Shrimp_ELB_GAM_Analysis
# Script for Fig. 5 - a comparison of 1D smooth terms for both LA and TX
# Inputs will come from non-spatial models for avg tow duration for the DO term
#
# Code by: KM Purcell
# updated: 2013-12-10
# TODO -> 
###########################################################

postscript(file="C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure\\Figure5.ps", 
           horizontal=F,
           onefile=F,
           width=3.27, height=9.19,
           family="Arial",
           pointsize=12)
par(mfrow=c(2,1))

#DO
plot(nspm.2mo.tow.count.la, select=1, shade=T, all.terms=T, scale=0,
     xlab=expression(paste("Dissolved Oxygen"~ (mg~L^{2}))),
     ylab="Effect of Dissolved Oxygen",
     ylim=c(-1.2,0.22))
#abline(h=0)
text(0.55,0.19, "(a)", cex=1,font=2)
plot(nspm.2mo.tow.count.tx, select=1, shade=T, all.terms=T, scale=0,
     xlab=expression(paste("Dissolved Oxygen"~ (mg~L^{2}))),
     ylab="")
#abline(h=0)
text(1.75, .35, "(b)", cex=1,font=2)
dev.off()