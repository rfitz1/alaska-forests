stands=read.table("young_sl.txt",header=T,sep="\t")
trees=read.table("bs_aspen_twenty.txt",header=T,sep="\t")
env=read.table("AK_qa_environment.txt",header=T,sep="")
summary(stands)
str(stands)
summary(stands)
summary(env)
stands$total.dens=stands$dens.PICGLA+stands$dens.PICMAR+stands$dens.BETNEO+stands$dens.POPTRE+stands$dens.POPBAL+
stands$count.sap.PICMAR+stands$count.sap.PICGLA+stands$count.sap.BETNEO+stands$count.sap.POPTRE
stands$prop.POPTRE=(stands$dens.POPTRE+stands$count.sap.POPTRE)/stands$total.dens
stands$POPTRE.tdens=(stands$dens.POPTRE+stands$count.sap.POPTRE)
hist(stands$prop.POPTRE)
stands.ta=subset(stands, prop.POPTRE>.25&permafrost==0)
summary(stands.ta)
str(stands.ta)
hist(log(stands.ta$POPTRE.tdens),breaks=100)

levels(stands.ta$siteid.x)
str(stands.ta)
trees.ta=subset(trees,Species=="TA")
str(trees.ta)
hist(trees.ta$Height,breaks=100)

library(fitdistrplus)
fg=fitdist(trees.ta$Height,"gamma")
fg
summary(fg)
plot(fg)
x=trees.ta$Height
d=density(trees.ta$Height)
plot(d)
curve(dgamma(x,shape=1.88,rate=0.0099),
	add=TRUE,
	col="blue") #1.748214832  0.007420978

set.seed(4)
gamma=rgamma(n=(25*63),shape=1.88,rate=.0099)
hist(gamma,breaks=100)
gamma=data.frame(gamma)
str(gamma)
gamma=subset(gamma,gamma<400)
str(gamma)
gamma2=data.frame(gamma[1:115,])
gamma2
colnames(gamma2)[1]="gamma"
gamma=rbind(gamma,gamma2)
str(gamma)

str(env)
id=env$id
id
id=rep(id, each=25)
id
id=data.frame(id)

summary(stands.ta)
count=data.frame(stands.ta$POPTRE.tdens)
count
set.seed(9)
x3 <- data.frame(runif(63, 8998, 66279))	#Changed the limits from 9707-71500
colnames(x3)[1]="density"
hist(x3$density)
count=rep(x3$density, each=25)
count=data.frame(count)
str(count)
summary(count)
count
str(id)
gamma=data.frame(gamma)
str(gamma)
heights=cbind(id,gamma)
heights=cbind(heights,count)
summary(heights)

sapinit=cbind.data.frame(
	stand_id=heights$id,
	species="Potr",             ########The name here needs to match the name in the species parameter database.
	count=round((heights$count/25), digits=2),
	height_from=round((heights$gamma/100)-0.1,digits=2),
	height_to=round((heights$gamma/100)+0.1,digits=2),
	age=20
)

summary(sapinit)
str(sapinit)
head(sapinit)

write.table(sapinit,file="qa_stand_model_init.txt",col.names=T,row.names=F,sep=";")


####Aspen>4m#### ##Actually DBH is in mm!

stands=read.table("young_sl.txt",header=T,sep="\t")
trees=read.table("bs_aspen_twenty.txt",header=T,sep="\t")
env=read.table("AK_qa_environment.txt",header=T,sep="")
summary(stands)
str(stands)
summary(stands)

stands$total.dens=stands$dens.PICGLA+stands$dens.PICMAR+stands$dens.BETNEO+stands$dens.POPTRE+stands$dens.POPBAL+
stands$count.sap.PICMAR+stands$count.sap.PICGLA+stands$count.sap.BETNEO+stands$count.sap.POPTRE
stands$prop.POPTRE=(stands$dens.POPTRE+stands$count.sap.POPTRE)/stands$total.dens
stands$POPTRE.tdens=(stands$dens.POPTRE+stands$count.sap.POPTRE)
hist(stands$prop.POPTRE)
stands.ta=subset(stands, prop.POPTRE>.25&permafrost==0)
summary(stands.ta)
str(stands.ta)
hist(log(stands.ta$POPTRE.tdens),breaks=100)

levels(stands.ta$siteid.x)
str(stands.ta)
trees.ta=subset(trees,Species=="TA")
x=trees.ta$Height
d=density(trees.ta$Height)
plot(d)

str(trees.ta)
trees.ta4=subset(trees.ta,Height>400 & Height<=500)
str(trees.ta4)
summary(trees.ta4)

x=trees.ta4$DBH
trees.ta4$DBH=as.numeric(trees.ta4$DBH)
d=density(trees.ta4$DBH)
plot(d)

library(fitdistrplus)
fg=fitdist(trees.ta4$DBH,"gamma")
fg									##Shape=8.872165 Rate=0.3434465
summary(fg)
plot(fg)
x=trees.ta4$DBH
d=density(trees.ta4$DBH)
plot(d)
curve(dgamma(x,shape=8.852,rate=0.372),
	add=TRUE,
	col="blue") 

#Setting 5 trees to be representative of a stand#
#Need 315 trees total#

set.seed(8)
gamma=rgamma(n=(5*63),shape=8.852,rate=0.372)
hist(gamma,breaks=100)
gamma=data.frame(gamma)
str(gamma)

str(env)
id=env$id
id
id=rep(id,each=5)
id
id=data.frame(id)

summary(stands.ta)
count=data.frame(stands.ta$POPTRE.tdens)
count
set.seed=(2)
x4 <- data.frame(runif(63, 709, 5221))   ##took the ratio 115/1575 and multiplied it to both sides of the limit to get the range of densities for aspens>4m##
colnames(x4)[1]="density"
hist(x4$density)
count=rep(x4$density, each=5)
count=data.frame(count)
str(count)
summary(count)
count
str(id)
gamma=data.frame(gamma)
str(gamma)
DBH=cbind(id,gamma)
DBH=cbind(DBH,count)
summary(DBH)

##Writing the text file##

sapinit=cbind.data.frame(
	stand_id=DBH$id,
	species="Potr", ### See above
	count=round((DBH$count/5), digits=2),
	DBH_from=round((DBH$gamma)-0.1,digits=2),
	DBH_to=round((DBH$gamma)+0.1,digits=2),
	age=20
)

summary(sapinit)
str(sapinit)
head(sapinit)

write.table(sapinit,file="qa>4_stand_model_init.txt",col.names=T,row.names=F,sep=";")