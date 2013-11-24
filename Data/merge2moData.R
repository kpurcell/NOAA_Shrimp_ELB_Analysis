# NOAA Shrimp ELB spatial analysis
# mergeScript
# purpose: to merge the various data sets and generate gam ready data frames
# updated: 2013-11-7



#Average out the environmental data per cell and integrate the data sets into modeling data sets


#aggregate response variables to polygons
grid.2mo.avg=aggregate(cbind(do, depth) ~ grid_cell * cent_lon * cent_lat* yr * jd,
                       data=elb.2mo.dat, FUN=mean)
tx.cells=length(grid.2mo.avg$do[grid.2mo.avg$cent_lon<(-94)])
la.cells=length(grid.2mo.avg$do[grid.2mo.avg$cent_lon>(-94)])


###
#aggregate 2mo responses to grid cells by jd
g2mo.dur<-aggregate(towhours ~ grid_cell * cent_lon * cent_lat * yr * jd ,
                    data=elb.2mo.dat, FUN=sum)
g2mo.avg.dur<-aggregate(towhours ~ grid_cell * cent_lon * cent_lat * yr * jd,
                        data=elb.2mo.dat, FUN=mean)
colnames(g2mo.avg.dur)[6]<-'avg.dur'
g2mo.tow.count<-aggregate(towhours ~ grid_cell * cent_lon * cent_lat* yr * jd,
                          data=elb.2mo.dat, FUN=length)
colnames(g2mo.tow.count)[6]<-'tow.cnt'
g2mo.vess.count<-aggregate(vesselID ~ grid_cell * cent_lon * cent_lat* yr * jd,
                           data=elb.2mo.dat, FUN=length)
colnames(g2mo.vess.count)[6]<-'vess.cnt'
###


#merge with avg environmental conditions
gam.2mo.dur<-merge(g2mo.dur, grid.2mo.avg, all.x=T,
                   by=c("grid_cell", "cent_lon", "cent_lat", "yr", "jd"))
colnames(abun.avg)[3]<-'yr'
gam.2mo.dur<-merge(gam.2mo.dur, abun.avg, all.x=T,
                   by=c("cent_lon", "cent_lat", "yr"))


gam.2mo.dur$sz<-gam.2mo.dur$grid_cell
gam.2mo.dur$sz[gam.2mo.dur$cent_lon>-90&gam.2mo.dur$cent_lon<=-89]=13
gam.2mo.dur$sz[gam.2mo.dur$cent_lon>-91&gam.2mo.dur$cent_lon<=-90]=14
gam.2mo.dur$sz[gam.2mo.dur$cent_lon>-92&gam.2mo.dur$cent_lon<=-91]=15
gam.2mo.dur$sz[gam.2mo.dur$cent_lon>-93&gam.2mo.dur$cent_lon<=-92]=16
gam.2mo.dur$sz[gam.2mo.dur$cent_lon>-94&gam.2mo.dur$cent_lon<=-93]=17

gam.2mo.dur$sz[gam.2mo.dur$cent_lon>-95&gam.2mo.dur$cent_lon<=-94]=18
gam.2mo.dur$sz[gam.2mo.dur$cent_lon<=-95&gam.2mo.dur$cent_lat>28]=19
gam.2mo.dur$sz[gam.2mo.dur$cent_lat>=27&gam.2mo.dur$cent_lat<28]=20
gam.2mo.dur$sz[gam.2mo.dur$cent_lat>=26&gam.2mo.dur$cent_lat<27]=21

gam.2mo.dur<-merge(gam.2mo.dur, ppnd.dat,all.x=T,
                   by=c("yr", "jd", "sz"))

gam.2mo.dur<-merge(gam.2mo.dur, hypoxia.dat,all.x=T,
                   by="yr")
gam.2mo.dur<-merge(gam.2mo.dur, shrcom.yr, all.x=T,
                   by="yr")

fuel.agg<-aggregate(la_fuel_price ~ yr + CW, data=fuel.dat, FUN=mean)
gam.2mo.dur=merge(gam.2mo.dur, fuel.agg, by=c("yr","CW"), all.x=T)

#summary(gam.2mo.dur)

#merge 
gam.2mo.avg.dur<-merge(g2mo.avg.dur, grid.2mo.avg, all.x=T,
                       by=c("grid_cell", "cent_lon", "cent_lat", "yr", "jd"))
colnames(abun.avg)[3]<-'yr'
gam.2mo.avg.dur<-merge(gam.2mo.avg.dur, abun.avg, all.x=T,
                       by=c("cent_lon", "cent_lat", "yr"))

gam.2mo.avg.dur$sz<-gam.2mo.avg.dur$grid_cell
gam.2mo.avg.dur$sz[gam.2mo.avg.dur$cent_lon>-90&gam.2mo.avg.dur$cent_lon<=-89]=13
gam.2mo.avg.dur$sz[gam.2mo.avg.dur$cent_lon>-91&gam.2mo.avg.dur$cent_lon<=-90]=14
gam.2mo.avg.dur$sz[gam.2mo.avg.dur$cent_lon>-92&gam.2mo.avg.dur$cent_lon<=-91]=15
gam.2mo.avg.dur$sz[gam.2mo.avg.dur$cent_lon>-93&gam.2mo.avg.dur$cent_lon<=-92]=16
gam.2mo.avg.dur$sz[gam.2mo.avg.dur$cent_lon>-94&gam.2mo.avg.dur$cent_lon<=-93]=17

gam.2mo.avg.dur$sz[gam.2mo.avg.dur$cent_lon>-95&gam.2mo.avg.dur$cent_lon<=-94]=18
gam.2mo.avg.dur$sz[gam.2mo.avg.dur$cent_lon<=-95&gam.2mo.avg.dur$cent_lat>28]=19
gam.2mo.avg.dur$sz[gam.2mo.avg.dur$cent_lat>=27&gam.2mo.avg.dur$cent_lat<28]=20
gam.2mo.avg.dur$sz[gam.2mo.avg.dur$cent_lat>=26&gam.2mo.avg.dur$cent_lat<27]=21

gam.2mo.avg.dur<-merge(gam.2mo.avg.dur, ppnd.dat,all.x=T,
                       by=c("yr", "jd", "sz"))

gam.2mo.avg.dur<-merge(gam.2mo.avg.dur, hypoxia.dat,all.x=T,
                       by="yr")
gam.2mo.avg.dur<-merge(gam.2mo.avg.dur, shrcom.yr, all.x=T,
                       by="yr")
gam.2mo.avg.dur=merge(gam.2mo.avg.dur, fuel.agg, by=c("yr","CW"), all.x=T)

#merge with avg environmental conditions
gam.2mo.tow.count<-merge(g2mo.tow.count, grid.2mo.avg, all.x=T,
                         by=c("grid_cell", "cent_lon", "cent_lat", "yr", "jd"))
colnames(abun.avg)[3]<-'yr'
gam.2mo.tow.count<-merge(gam.2mo.tow.count, abun.avg, all.x=T,
                         by=c("cent_lon", "cent_lat", "yr"))

gam.2mo.tow.count$sz<-gam.2mo.tow.count$grid_cell
gam.2mo.tow.count$sz[gam.2mo.tow.count$cent_lon>-90&gam.2mo.tow.count$cent_lon<=-89]=13
gam.2mo.tow.count$sz[gam.2mo.tow.count$cent_lon>-91&gam.2mo.tow.count$cent_lon<=-90]=14
gam.2mo.tow.count$sz[gam.2mo.tow.count$cent_lon>-92&gam.2mo.tow.count$cent_lon<=-91]=15
gam.2mo.tow.count$sz[gam.2mo.tow.count$cent_lon>-93&gam.2mo.tow.count$cent_lon<=-92]=16
gam.2mo.tow.count$sz[gam.2mo.tow.count$cent_lon>-94&gam.2mo.tow.count$cent_lon<=-93]=17

gam.2mo.tow.count$sz[gam.2mo.tow.count$cent_lon>-95&gam.2mo.tow.count$cent_lon<=-94]=18
gam.2mo.tow.count$sz[gam.2mo.tow.count$cent_lon<=-95&gam.2mo.tow.count$cent_lat>28]=19
gam.2mo.tow.count$sz[gam.2mo.tow.count$cent_lat>=27&gam.2mo.tow.count$cent_lat<28]=20
gam.2mo.tow.count$sz[gam.2mo.tow.count$cent_lat>=26&gam.2mo.tow.count$cent_lat<27]=21

gam.2mo.tow.count<-merge(gam.2mo.tow.count, ppnd.dat,all.x=T,
                         by=c("yr", "jd", "sz"))

gam.2mo.tow.count<-merge(gam.2mo.tow.count, hypoxia.dat,all.x=T,
                         by="yr")
gam.2mo.tow.count<-merge(gam.2mo.tow.count, shrcom.yr, all.x=T,
                         by="yr")
# names(gam.2mo.tow.count)
# summary(gam.2mo.tow.count)
gam.2mo.tow.count=merge(gam.2mo.tow.count, fuel.agg, by=c("yr","CW"), all.x=T)


#make two data sets a LA and TX
gam.2mo.dur.la<-subset(gam.2mo.dur, gam.2mo.dur$cent_lon>=-93.83)
gam.2mo.dur.tx<-subset(gam.2mo.dur, gam.2mo.dur$cent_lon< -93.83)
gam.2mo.avg.dur.la<-subset(gam.2mo.avg.dur, gam.2mo.avg.dur$cent_lon>=-93.83)
gam.2mo.avg.dur.tx<-subset(gam.2mo.avg.dur, gam.2mo.avg.dur$cent_lon< -93.83)
gam.2mo.tow.count.la<-subset(gam.2mo.tow.count, gam.2mo.tow.count$cent_lon>=-93.83)
gam.2mo.tow.count.tx<-subset(gam.2mo.tow.count, gam.2mo.tow.count$cent_lon< -93.83)
# gam.2mo.vess.count.la<-subset(gam.2mo.vess.count, gam.2mo.vess.count$cent_lon>=-93.83)
# gam.2mo.vess.count.tx<-subset(gam.2mo.vess.count, gam.2mo.vess.count$cent_lon< -93.83)








