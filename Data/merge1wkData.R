# NOAA Shrimp ELB spatial analysis
# mergeScript
# purpose: to merge the various data sets and generate gam ready data frames
# updated: 2013-11-7



#Average out the environmental data per cell and integrate the data sets into modeling data sets


#aggregate response variables to polygons
grid.1wk.avg=aggregate(cbind(do, depth) ~ grid_cell * cent_lon * cent_lat* yr * jd,
                       data=elb.1wk.dat, FUN=mean)
tx.cells=length(grid.1wk.avg$do[grid.1wk.avg$cent_lon<(-94)])
la.cells=length(grid.1wk.avg$do[grid.1wk.avg$cent_lon>(-94)])


###
#aggregate 1wk responses to grid cells by jd
g1wk.dur<-aggregate(towhours ~ grid_cell * cent_lon * cent_lat * yr * jd ,
                    data=elb.1wk.dat, FUN=sum)
g1wk.avg.dur<-aggregate(towhours ~ grid_cell * cent_lon * cent_lat * yr * jd,
                        data=elb.1wk.dat, FUN=mean)
colnames(g1wk.avg.dur)[6]<-'avg.dur'
g1wk.tow.count<-aggregate(towhours ~ grid_cell * cent_lon * cent_lat* yr * jd,
                          data=elb.1wk.dat, FUN=length)
colnames(g1wk.tow.count)[6]<-'tow.cnt'
g1wk.vess.count<-aggregate(vesselID ~ grid_cell * cent_lon * cent_lat* yr * jd,
                           data=elb.1wk.dat, FUN=length)
colnames(g1wk.vess.count)[6]<-'vess.cnt'
###


#merge with avg environmental conditions
gam.1wk.dur<-merge(g1wk.dur, grid.1wk.avg, all.x=T,
                   by=c("grid_cell", "cent_lon", "cent_lat", "yr", "jd"))
colnames(abun.avg)[3]<-'yr'
gam.1wk.dur<-merge(gam.1wk.dur, abun.avg, all.x=T,
                   by=c("cent_lon", "cent_lat", "yr"))


gam.1wk.dur$sz<-gam.1wk.dur$grid_cell
gam.1wk.dur$sz[gam.1wk.dur$cent_lon>-90&gam.1wk.dur$cent_lon<=-89]=13
gam.1wk.dur$sz[gam.1wk.dur$cent_lon>-91&gam.1wk.dur$cent_lon<=-90]=14
gam.1wk.dur$sz[gam.1wk.dur$cent_lon>-92&gam.1wk.dur$cent_lon<=-91]=15
gam.1wk.dur$sz[gam.1wk.dur$cent_lon>-93&gam.1wk.dur$cent_lon<=-92]=16
gam.1wk.dur$sz[gam.1wk.dur$cent_lon>-94&gam.1wk.dur$cent_lon<=-93]=17

gam.1wk.dur$sz[gam.1wk.dur$cent_lon>-95&gam.1wk.dur$cent_lon<=-94]=18
gam.1wk.dur$sz[gam.1wk.dur$cent_lon<=-95&gam.1wk.dur$cent_lat>28]=19
gam.1wk.dur$sz[gam.1wk.dur$cent_lat>=27&gam.1wk.dur$cent_lat<28]=20
gam.1wk.dur$sz[gam.1wk.dur$cent_lat>=26&gam.1wk.dur$cent_lat<27]=21

gam.1wk.dur<-merge(gam.1wk.dur, ppnd.dat,all.x=T,
                   by=c("yr", "jd", "sz"))

gam.1wk.dur<-merge(gam.1wk.dur, hypoxia.dat,all.x=T,
                   by="yr")
gam.1wk.dur<-merge(gam.1wk.dur, shrcom.yr, all.x=T,
                   by="yr")

fuel.agg<-aggregate(la_fuel_price ~ yr + CW, data=fuel.dat, FUN=mean)
gam.1wk.dur=merge(gam.1wk.dur, fuel.agg, by=c("yr","CW"), all.x=T)

#summary(gam.1wk.dur)

#merge 
gam.1wk.avg.dur<-merge(g1wk.avg.dur, grid.1wk.avg, all.x=T,
                       by=c("grid_cell", "cent_lon", "cent_lat", "yr", "jd"))
colnames(abun.avg)[3]<-'yr'
gam.1wk.avg.dur<-merge(gam.1wk.avg.dur, abun.avg, all.x=T,
                       by=c("cent_lon", "cent_lat", "yr"))

gam.1wk.avg.dur$sz<-gam.1wk.avg.dur$grid_cell
gam.1wk.avg.dur$sz[gam.1wk.avg.dur$cent_lon>-90&gam.1wk.avg.dur$cent_lon<=-89]=13
gam.1wk.avg.dur$sz[gam.1wk.avg.dur$cent_lon>-91&gam.1wk.avg.dur$cent_lon<=-90]=14
gam.1wk.avg.dur$sz[gam.1wk.avg.dur$cent_lon>-92&gam.1wk.avg.dur$cent_lon<=-91]=15
gam.1wk.avg.dur$sz[gam.1wk.avg.dur$cent_lon>-93&gam.1wk.avg.dur$cent_lon<=-92]=16
gam.1wk.avg.dur$sz[gam.1wk.avg.dur$cent_lon>-94&gam.1wk.avg.dur$cent_lon<=-93]=17

gam.1wk.avg.dur$sz[gam.1wk.avg.dur$cent_lon>-95&gam.1wk.avg.dur$cent_lon<=-94]=18
gam.1wk.avg.dur$sz[gam.1wk.avg.dur$cent_lon<=-95&gam.1wk.avg.dur$cent_lat>28]=19
gam.1wk.avg.dur$sz[gam.1wk.avg.dur$cent_lat>=27&gam.1wk.avg.dur$cent_lat<28]=20
gam.1wk.avg.dur$sz[gam.1wk.avg.dur$cent_lat>=26&gam.1wk.avg.dur$cent_lat<27]=21

gam.1wk.avg.dur<-merge(gam.1wk.avg.dur, ppnd.dat,all.x=T,
                       by=c("yr", "jd", "sz"))

gam.1wk.avg.dur<-merge(gam.1wk.avg.dur, hypoxia.dat,all.x=T,
                       by="yr")
gam.1wk.avg.dur<-merge(gam.1wk.avg.dur, shrcom.yr, all.x=T,
                       by="yr")
gam.1wk.avg.dur=merge(gam.1wk.avg.dur, fuel.agg, by=c("yr","CW"), all.x=T)

#merge with avg environmental conditions
gam.1wk.tow.count<-merge(g1wk.tow.count, grid.1wk.avg, all.x=T,
                         by=c("grid_cell", "cent_lon", "cent_lat", "yr", "jd"))
colnames(abun.avg)[3]<-'yr'
gam.1wk.tow.count<-merge(gam.1wk.tow.count, abun.avg, all.x=T,
                         by=c("cent_lon", "cent_lat", "yr"))

gam.1wk.tow.count$sz<-gam.1wk.tow.count$grid_cell
gam.1wk.tow.count$sz[gam.1wk.tow.count$cent_lon>-90&gam.1wk.tow.count$cent_lon<=-89]=13
gam.1wk.tow.count$sz[gam.1wk.tow.count$cent_lon>-91&gam.1wk.tow.count$cent_lon<=-90]=14
gam.1wk.tow.count$sz[gam.1wk.tow.count$cent_lon>-92&gam.1wk.tow.count$cent_lon<=-91]=15
gam.1wk.tow.count$sz[gam.1wk.tow.count$cent_lon>-93&gam.1wk.tow.count$cent_lon<=-92]=16
gam.1wk.tow.count$sz[gam.1wk.tow.count$cent_lon>-94&gam.1wk.tow.count$cent_lon<=-93]=17

gam.1wk.tow.count$sz[gam.1wk.tow.count$cent_lon>-95&gam.1wk.tow.count$cent_lon<=-94]=18
gam.1wk.tow.count$sz[gam.1wk.tow.count$cent_lon<=-95&gam.1wk.tow.count$cent_lat>28]=19
gam.1wk.tow.count$sz[gam.1wk.tow.count$cent_lat>=27&gam.1wk.tow.count$cent_lat<28]=20
gam.1wk.tow.count$sz[gam.1wk.tow.count$cent_lat>=26&gam.1wk.tow.count$cent_lat<27]=21

gam.1wk.tow.count<-merge(gam.1wk.tow.count, ppnd.dat,all.x=T,
                         by=c("yr", "jd", "sz"))

gam.1wk.tow.count<-merge(gam.1wk.tow.count, hypoxia.dat,all.x=T,
                         by="yr")
gam.1wk.tow.count<-merge(gam.1wk.tow.count, shrcom.yr, all.x=T,
                         by="yr")
#names(gam.1wk.tow.count)
#summary(gam.1wk.tow.count)
#hist(gam.1wk.tow.count$sz)
colnames(gam.1wk.tow.count)[13]<-'area.hypoxic'

gam.1wk.tow.count=merge(gam.1wk.tow.count, fuel.agg, by=c("yr","CW"), all.x=T)


#make two data sets a LA and TX
gam.1wk.dur.la<-subset(gam.1wk.dur, gam.1wk.dur$cent_lon>=-93.83)
gam.1wk.dur.tx<-subset(gam.1wk.dur, gam.1wk.dur$cent_lon< -93.83)
gam.1wk.avg.dur.la<-subset(gam.1wk.avg.dur, gam.1wk.avg.dur$cent_lon>=-93.83)
gam.1wk.avg.dur.tx<-subset(gam.1wk.avg.dur, gam.1wk.avg.dur$cent_lon< -93.83)
gam.1wk.tow.count.la<-subset(gam.1wk.tow.count, gam.1wk.tow.count$cent_lon>=-93.83)
gam.1wk.tow.count.tx<-subset(gam.1wk.tow.count, gam.1wk.tow.count$cent_lon< -93.83)
# gam.1wk.vess.count.la<-subset(gam.1wk.vess.count, gam.1wk.vess.count$cent_lon>=-93.83)
# gam.1wk.vess.count.tx<-subset(gam.1wk.vess.count, gam.1wk.vess.count$cent_lon< -93.83)

               



