rm(list=ls())


master=read.table("young_sl.txt", header=T,sep="\t")
summary(master)
str(master)
id=read.table("acf.txt", header=T, sep="")
summary(id)

master$total.dens=master$dens.PICGLA+master$dens.PICMAR+master$dens.BETNEO+master$dens.POPTRE+master$dens.POPBAL+master$count.sap.PICMAR+master$count.sap.PICGLA+master$count.sap.BETNEO+master$count.sap.POPTRE
master$prop.BETNEO=(master$dens.BETNEO+master$count.sap.BETNEO)/master$total.dens
hist(master$prop.BETNEO)

master$prop.POPTRE=(master$dens.POPTRE+master$count.sap.POPTRE)/master$total.dens
hist(master$prop.POPTRE)

master.pb=subset(master,prop.BETNEO>.25&permafrost==0)
master.pbnon=subset(master,prop.BETNEO<0.3)
str(master.pb)
master.pb
summary(master.pb)
summary(master.pbnon)

master.qa=subset(master,prop.POPTRE>.25&permafrost==0)
master.qanon=subset(master,prop.POPTRE<0.3)
str(master.qa)
master.qa=cbind(id,master.qa)
summary(master.qa)
summary(master.qanon)
# grid
###Winslow added!!! you can add this downbelow also to write out a grid for aspen too. 
id=matrix(1:52,ncol=26,byrow=T) id
write.table(id,file="grid.txt",col.names=T,row.names=F,quote=F)
########
##Writing the Paper Birch Environment File##

x=c(0:24)
y=c(0:1)

xi=rep(x,length(y))
yi=c(rep(0,length(x)),rep(1,length(x)))

env.pb=data.frame()
for(i in 1:dim(master.pb)[1])
	{
	env.pb=rbind(env.pb,data.frame(
		id=i,
		x=xi[i],
		y=yi[i],
		model.climate.tableName=master.pb$id[i],
		model.site.availableNitrogen=25,       #####################Let's set this to 25 to start. I calculated this based on estimates of Nitrogen pools in Melvin et al. 2015. I took figure 5, multiplied 100000(meters squared per ha) and divided by 100,000 (grams in kg).
		model.site.soilDepth=30,
		model.site.pctSand=34,
		model.site.pctSilt=33,
		model.site.pctClay=33
		))
	}	

summary(env.pb)
levels(env.pb$model.climate.tableName)
env.pb
str(env.pb)
write.table(env.pb,file="AK_pb_environment.txt",col.names=T,row.names=F,quote=F)

##Writing the Aspen Environment File##

x=c(0:20)
y=c(0:2)

xi=rep(x,length(y))
yi=c(rep(0,length(x)),rep(1,length(x)),rep(2,length(x)))

env.qa=data.frame()
for(i in 1:dim(master.qa)[1])
	{
	env.qa=rbind(env.qa,data.frame(
		   id=i,
		   x=xi[i],
		   y=yi[i],
		   model.climate.tableName=master.qa$id[i],
		   model.site.availableNitrogen=55,
		   model.site.soilDepth=30,
		   model.site.pctSand=34,
		   model.site.pctSilt=33,
		   model.site.pctClay=33
		   ))
	}
	
summary(env.qa)
levels(env.qa$model.climate.tableName)
env.qa
str(env.qa)
write.table(env.qa,file="AK_qa_environment.txt", col.names=T, row.names=F,quote=F)

