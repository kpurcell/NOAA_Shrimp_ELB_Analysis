###########################################################
# NOAA_Shrimp_ELB_GAM_Analysis
# Script for Fig. 1 - a comparison of 1D smooth terms for both LA and TX
# Inputs will come from non-spatial models due to their inclusion of DO as a smooth
#
# Code by: KM Purcell
# updated: 2013-12-13
###########################################################
pdf(file="C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure1.pdf",
    width = 4.86, height = 9.19, 
    family = "Arial", 
    paper = "special", 
    onefile = FALSE)

# postscript(file="C:\\Users\\Kevin.Purcell\\Documents\\GitHub\\NOAA_Shrimp_ELB_Analysis\\Presentation\\Article\\figure\\Figure1.eps", horizontal=F,
#            onefile=F,
#            width=6.83, height=9.19,
#            family="Arial",
#            pointsize=12)

par(mfrow=c(4,2))

#DO
plot(nspm.2mo.dur.la, select=1, shade=T, all.terms=T, scale=0,
     xlab="Dissolved Oxygen (mg l ^-1)",
     ylab="s(Dissolved Oxygen)")
text(6.2, 0.12, "A", cex=1,font=2)
plot(nspm.2mo.dur.tx, select=1, shade=T, all.terms=T, scale=0,
     xlab="Dissolved Oxygen (mg l^-1",
     ylab="")
text(6.7, 0.19, "B", cex=1,font=2)


#Depth
plot(nspm.2mo.dur.la, select=2, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)",
     ylab="Effect of Depth")
text(95, 0.41, "C", cex=1,font=2)
plot(nspm.2mo.dur.tx, select=2, shade=T, all.terms=T, scale=0,
     xlab="Depth (m)",
     ylab="")
text(82, 0.48, "D", cex=1,font=2)


#Price
plot(nspm.2mo.dur.la, select=3, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)",
     ylab="Effect of Price")
text(5.8, 0.2, "E", cex=1,font=2)
plot(nspm.2mo.dur.tx, select=3, shade=T, all.terms=T, scale=0,
     xlab="Price per pound (USD)",
     ylab="")
text(5.2, 0.22, "F", cex=1,font=2)


#Day of Year
plot(nspm.2mo.dur.la, select=4, shade=T, all.terms=T, scale=0,
     xlab="Julian Day",
     ylab="Effect of Julian Day")
text(241, 0.15, "G", cex=1,font=2)
plot(nspm.2mo.dur.tx, select=4, shade=T, all.terms=T, scale=0,
     xlab="Julian Day",
     ylab="")
text(242,0.17, "H", cex=1,font=2)

dev.off()