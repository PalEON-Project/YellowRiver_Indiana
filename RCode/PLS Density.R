#4-3-2017
#historic density

setwd("~/Dropbox/YR_Manuscript/YellowRiver_Indiana")

wdens = read.csv("./Data/West_Density_4-2017.csv",stringsAsFactors = FALSE)
edens = read.csv("./Data/East_Density_4-2017.csv",stringsAsFactors = FALSE)

wdens$Line="West"
edens$Line="East"
head(wdens)
head(edens)

dim(wdens) #[1] 1742    6
dim(edens) #[1] 1626    6


#calculating means and SEs for each
dWW=mean(wdens$density,na.rm=TRUE)
dWW #[1] 41.32793

seWW=sd(wdens$density,na.rm=TRUE)/sqrt(length(which(!is.na(wdens$density))))
seWW #[1] 2.763947

dEE=mean(edens$density,na.rm=TRUE)
dEE #[1] 175.1579

seEE=sd(edens$density,na.rm=TRUE)/sqrt(length(which(!is.na(edens$density))))
seEE #[1] 6.40166

density=list(dWW,seWW,dEE,seEE)
density

name=c("West density","West SE","East density","East SE")

names(density)=name
density

totaldens=merge(wdens,edens,all=TRUE)
dim(totaldens)

a=t.test(density~Line, var.equal=T, data=totaldens, na.rm=TRUE)
a

denst=list(density,a)
denst

sink(file = paste("./Results/PLSdensity_YR.txt", sep=""))
denst
sink()





dens=merge(wdens,edens, all=TRUE)
dim(dens) #[1] 3368    6
head(dens)


dens$Type="NA"

for (i in 1:length(dens$Type)){
  if (dens$density[i] < 0.5){
    dens$Type[i]="Prairie"
  }else{
    if (dens$density[i] >47){
      dens$Type[i]="Forest"
    }else{
      dens$Type[i]="Savanna"
    }
  }
}

View(dens)

write.csv(dens,"./Data/PLSDensity_Type.csv")

#next step: go to line div code FULL.r to add distance from midline!




#the ISLAND density
isl=read.csv("./Data/Island_density.csv",stringsAsFactors = FALSE)
tot=read.csv("./Data/PLSDensity_Type.csv",stringsAsFactors = FALSE)
head(isl)
head(tot)
tot=tot[,2:7]   #subset only what we need
head(tot)
isl$Line="Island"  #"Line" designation as island
head(isl)

denstot=merge(isl,tot, all=TRUE) #THE MERGE: REG POINTS AND ISLAND POINTS for DENSITY
dim(denstot) #[1] 3690
dim(tot) #[1] 3368 
dim(isl) #[1] 322
322+3368 #le check


mid=mean(isl$density) #[1] 85.81832
sid
sid=sd(isl$density,na.rm=TRUE)/sqrt(length(which(!is.na(isl$density))))#[1] 8.519587

island=list(mid,sid)
names(island)<-c("Mean density island", "se density island")
island

dddW=subset(tot, Line=="West")   #find mean and se of west
mwd=mean(dddW$density) #[1] 41.32793
swd=sd(dddW$density,na.rm=TRUE)/sqrt(length(which(!is.na(dddW$density))))#[1] 2.763947
dddE<-subset(tot, Line=="East")  
med=mean(dddE$density) #[1] 175.1579
sed=sd(dddE$density,na.rm=TRUE)/sqrt(length(which(!is.na(dddE$density))))#[1] 6.40166
med
sed

#anova for PCA score
anovaDENS<- aov(density~Line, data=denstot)
summary(anovaDENS)
tuk=TukeyHSD(anovaDENS)
tuk

aovD=list(island,anovaDENS,tuk)
aovD


#all list output density!

densalloutput=list(denst,aovD)
densalloutput

sink(file = paste("./Results/PLSdensity_YR.txt", sep=""))
densalloutput
sink()


#bar graph PCA score 1 island!
Md <- c(mwd,med,mid)
Sd <- c(swd,sed,sid)



#function for error bars
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#creating plot
par(mar=c(5,5,5,5))
Pd <-barplot(Md, ylim=c(0,200),col = c('#ff663e','#751aff', 'darkseagreen3'), 
            names.arg=c("West","East",""), ylab='Density (trees/ha)',
            cex.axis=2,cex.lab=2,cex.names=2)       
error.bar(Pd,Md,Sd)  
woot <- error.bar(Pd,Md,Sd)
#segments(P, sw, se, si)

#CODE FOR WRITING PNG
png(width = 7, height = 7, units = 'in', res = 400, filename = "./Figures/islandbarDENS.png")
dev.off()


#"Eastern \n Oak Area"







