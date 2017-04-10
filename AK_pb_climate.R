rm(list=ls())
setwd("/Users/RyanFitzsimmons/Desktop/alaska_project/materials/climate")

master=read.table("AK_pb_environment.txt",header=T,sep=" ")
summary(master)
master=aggregate(x~model.climate.tableName,data=master, sum)
master

master.clim=data.frame()
for(i in 1:dim(master)[1])
	{
	id=master$model.climate.tableName[i]
	dat=read.csv(paste(id,".csv",sep=""),header=T,sep=",",skip=7)
	master.clim=rbind(master.clim,data.frame(id,dat))
	}
	
summary(master.clim)
str(master.clim)

summary(master.clim)

library(lubridate)
master.clim$date=format(strptime(master.clim$yday, format="%j"), format="%m-%d") 
master.clim$mm.dd=as.Date(master.clim$date, format="%m-%d")
master.clim$mm.dd=as.POSIXlt(master.clim$mm.dd)
master.clim$month=month(master.clim$mm.dd)
master.clim$day=day(master.clim$mm.dd)
master.clim[55:65,]
master.clim[360:370,]

master.clim$rad=master.clim$srad..W.m.2.*master.clim$dayl..s./1000000
summary(master.clim)
summary(master.clim[master.clim$rad>30,])
master.clim$vp..P.=master.clim$vp..Pa./1000
summary(master.clim)
str(master.clim)

library(RSQLite)
db.conn <<- dbConnect(RSQLite::SQLite(),
 dbname="AK_PB_climate.sqlite" )

for(i in levels(master.clim$id))
	{
	dat=master.clim[master.clim$id==i,c()]
	}
	
dbDisconnect(db.conn)
