master=read.table("AK_qa_environment.txt",header=T,sep=" ")
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
##Converting julian day into month day variables  ## By the way. I love lubridate!
library(lubridate)
master.clim$date=format(strptime(master.clim$yday, format="%j"), format="%m-%d") 
master.clim$mm.dd=as.Date(master.clim$date, format="%m-%d")
master.clim$mm.dd=as.POSIXlt(master.clim$mm.dd)
master.clim$month=month(master.clim$mm.dd)
master.clim$day=day(master.clim$mm.dd)
master.clim[55:65,] # looking good
master.clim[360:370,] # looking good here too

##
# calculating daily radiation sum
###
master.clim$rad=master.clim$srad..W.m.2.*master.clim$dayl..s./1000000 # in MJ/m²/day
summary(master.clim) # looks reasonable, albeit the max values are quite high
summary(master.clim[master.clim$rad>30,])
# vpd in kPa
master.clim$vp..Pa.=master.clim$vp..Pa./1000
summary(master.clim)
str(master.clim)
# looks ok, although I would expect the max VPD values to be higher


### write sqlite database
library(RSQLite)
# getting them in the right order and with the correct column headings - see http://iland.boku.ac.at/ClimateData
# NB: first need to create an empty sqlite db with that name in the respective folder
db.conn <<- dbConnect(RSQLite::SQLite(), dbname="AK_potr_climate.sqlite" )

for(i in levels(master.clim$id))
	{
	dat=master.clim[master.clim$id==i,c(2,13,14,9,8,5,15,10)] 
	names(dat)[4]="min_temp"
	names(dat)[5]="max_temp"
	names(dat)[6]="prec"
	names(dat)[8]="vpd"
	
	dbWriteTable(db.conn, name=paste(i),dat, row.names=F)
	}
	
	
dbDisconnect(db.conn)
