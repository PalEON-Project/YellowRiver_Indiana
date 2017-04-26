setwd("~/Documents/YellowRiver_IN")

treess=read.csv("./Data/EWAllData2-28.csv")
dim(treess) #6126, [1] 6126   56
head(treess)
View(treess)
str(treess$MUKEY)


#mote text for more tests

#####running counts, #corners and #trees
  E=subset(treess,Line=="East")
    nrow(E) #3122 points in the east
    Etree<-subset(E, TreeNo=='tree1' | TreeNo=='tree2'| TreeNo=='tree3'| TreeNo=='tree4')
      nrow(Etree) #3059 trees in the east
          ET=subset(E,Group_=='Oak' | Group_=='Other')
            nrow(ET) #3059, just a check :)
      Ewater<-subset(E, Group_=='Water')
        nrow(Ewater) #46 water points in the east
      Eprairie<-subset(E, Group_=="No tree")
        nrow(Eprairie) #17 prairie points in the east
  W=subset(treess,Line=="West")
    nrow(W) #3004 points in the west
    Wtree<-subset(W, TreeNo=='tree1' | TreeNo=='tree2'| TreeNo=='tree3'| TreeNo=='tree4')
      nrow(Wtree) #2633 trees in the west
          WT=subset(W,Group_=='Oak' | Group_=='Other')
            nrow(WT) #2633, just a check :)
      Wwater<-subset(W, Group_=='Water')
        nrow(Wwater) #223
      Wprairie<-subset(W, Group_=="No tree")
        nrow(Wprairie) #148
      






#subset east and west; includes trees, prairie and water corners
WWW<-subset(treess, Line=="West")
dim(WWW) #3004 x 56
str(WWW)
EEE<-subset(treess, Line=="East")
dim(EEE) #3122 x 56
View(EEE)


#select rows that i want means and SEs of


treesel=treess[,c("ID","Line","TDiameter","temp","precip","DEM","sandtotal_",
                  "silttotal_","claytotal_","om_r","ksat_r","awc_r","kwfact","NCCPI")]


head(treesel)
dim(treesel) # 6126 x 14
View(treesel)




#comparing diameters in oak and non-oak buffers, PLS notes
#adjustment for inches to cm, so run separately from environmental variables
meanWWWdiam<-mean(WWW$TDiameter,na.rm=TRUE)*2.54
seW<-sd(WWW$TDiameter,na.rm=TRUE)/sqrt(length(which(!is.na(WWW$TDiameter))))*2.54

meanEEEdiam<-mean(EEE$TDiameter,na.rm=TRUE)*2.54
seE<-sd(EEE$TDiameter,na.rm=TRUE)/sqrt(length(which(!is.na(EEE$TDiameter))))*2.54

t=t.test(TDiameter~Line,var.equal=T, data=treess,na.rm=TRUE)
t

diam=list(meanWWWdiam,seW,meanEEEdiam,seE,t)
names(diam)=c("W mean","w se","e mean","e se","t")
diam



#####Code for t-tests!
#output is a list

head(treesel)
colnames(treesel)
#function to run two-sample t-test
#comparing tree diameter and environmental variables in E and W
#this includes ALL trees, water corners and prairie corners

results=list(1,1,1,1,1,1,1,1,1,1,1)
length(results)
varnames=c("temp","precip","DEM","sandtotal_","silttotal_","claytotal_",
           "om_r","ksat_r","awc_r","kwfact","NCCPI")
length(varnames)   #11

for (i in 4:14){
  a=t.test(treesel[,i]~Line, var.equal=T, data=treesel, na.rm=TRUE)
  results[[i-3]]=a
  names(results)=varnames
}


results
length(results)

#function to find means and standard errors for E and W


#subset cols like above, but for separated E and W tree/points

Wsel=WWW[,c("ID","Line","TDiameter","temp","precip","DEM","sandtotal_",
            "silttotal_","claytotal_","om_r","ksat_r","awc_r","kwfact","NCCPI")]
as.numeric(Wsel$temp)
as.numeric(Wsel$precip)
as.numeric(Wsel$DEM)
str(Wsel)
head(Wsel)
dim(Wsel)  #3004 x 14

Esel=EEE[,c("ID","Line","TDiameter","temp","precip","DEM","sandtotal_",
            "silttotal_","claytotal_","om_r","ksat_r","awc_r","kwfact","NCCPI")]
as.numeric(Esel$temp)
as.numeric(Esel$precip)
as.numeric(Esel$DEM)
head(Esel)
dim(Esel)  #3122 x 14

#define empty vectors to fill with output
meansE=rep(0,11)
seE=rep(0,11)
meansW=rep(0,11)
seW=rep(0,11)

lab=c("temp","precip","DEM","sandtotal_",
      "silttotal_","claytotal_","om_r","ksat_r","awc_r","kwfact","NCCPI")
length(lab)
#run the for loop that goes through the selected rows!
for(i in 4:14){
  meansW[i-3]=mean(Wsel[,i],na.rm=TRUE)
    names(meansW)<-lab
  seW[i-3]=sd(Wsel[,i],na.rm=TRUE)/sqrt(length(which(!is.na(Wsel[,i]))))
    names(seW)<-lab
  meansE[i-3]=mean(Esel[,i],na.rm=TRUE)
    names(meansE)<-lab
  seE[i-3]=sd(Esel[,i],na.rm=TRUE)/sqrt(length(which(!is.na(Esel[,i]))))
    names(seE)<-lab
  meanse=list(meansW,seW,meansE,seE)
    names(meanse)=c("WEST MEANS","WEST SE","EAST MEANS","EAST SE")
}
meanse


alloutbutPCA=list(diam,meanse,results)
alloutbutPCA

#print this list of lists


sink(file = paste("./Results/Means_Ttests_results.txt", sep=""))
alloutbutPCA
sink()









####################################################
#principal component analysis
#we are running a principac component analysis using the following variables"
#sand, silt, clay, awc, ksat, temp, precip, elevation.
#the scores were added to the YR_Buffer_Clim_Dem_Soils_PCA.csv document.
names(treess)
PCAdata<-treess[,c("temp","precip","DEM","sandtotal_",
                  "silttotal_","claytotal_","om_r","ksat_r","awc_r","Line","ID","POINT_X","POINT_Y")]
dim(PCAdata) #6126 *p
View(PCAdata)

names(PCAdata)
#[1] "temp"       "precip"     "DEM"        "sandtotal_" "silttotal_" "claytotal_" "om_r"      
#[8] "ksat_r"     "awc_r"      "Line"       "ID"        "POINT_X"      "POINT_Y"

PCA<-prcomp(na.omit(PCAdata[1:9]), retx=TRUE, scale. = TRUE)
summary(PCA)
  #54.1%, then 12.8%, then 11.2%

#loadings
loadings <-PCA$rotation
head(loadings,9)


#output for prcomp
#PRINTED THIS ABOVE WITH THE REST OF THE OUTPUT, BUT JUST SWITCH TO DO IT SEPARATELY HERE
outputPCAprelim <- list(summary(PCA), loadings)
outputPCAprelim


#scores
scores <- PCA$x
head(scores,5)
dim(scores) #there are NAs in this, and need to join to original data #6035 x 8
View(scores)

nona<-na.omit(PCAdata) #remove NA from original pca data too
dim(nona)     #correct number of rows, so can add a column
head(nona)

a <- cbind(nona$ID, scores) 
cecil <- cbind(nona$Line, a)
josie <- cbind(nona$POINT_Y,cecil)
carlos <- cbind(nona$POINT_X, josie)
View(carlos)
colnames(carlos)[4] <- "ID"
colnames(carlos)[3] <- "Line"
colnames(carlos)[2] <- "POINT_Y"
colnames(carlos)[1] <- "POINT_X"
#matrix, so 2s are trees in oak buffer; 1s are trees in non-oak buffer

write.table(carlos,file='./Data/scoresEW.csv', sep=",", col.names=NA)
scoresfile <- read.csv("./Data/scoresEW.csv")
View(scoresfile)
head(scoresfile)
tail(scoresfile)


#now let's run t-tests for first 3 components!

comp1=t.test(PC1~Line,var.equal=T, data=scoresfile,na.rm=TRUE)
comp2=t.test(PC2~Line,var.equal=T, data=scoresfile,na.rm=TRUE)
comp3=t.test(PC3~Line,var.equal=T, data=scoresfile,na.rm=TRUE)
comp1
comp2
comp3


wscore=subset(scoresfile,Line==2)   #SEs for first component scores 
escore=subset(scoresfile,Line==1)
Wsec1<-sd(wscore$PC1,na.rm=TRUE)/sqrt(length(which(!is.na(wscore$PC1))))
Esec1<-sd(escore$PC1,na.rm=TRUE)/sqrt(length(which(!is.na(escore$PC1))))
Wsec1
Esec1

pcaT=list(comp1,comp2,comp3)
pcaT #also added this output to cumulative output above




#output for PCA, loadings and t.tests

allpcaresults = list(outputPCAprelim,pcaT)
sink(file = paste("./Results/AllPCAresults.txt", sep=""))
allpcaresults
sink()





#################################
#drainage class: categorical
#################################
#got the drainage class from the muaggatt table
#joined in ArcGIS march 3 2017


#okay NEW drainage from muaggatt table!
drain=read.csv("./Data/drainageEWraw.csv")
library(plyr)
drainc=join(treess,drain,by="MUKEY",type="left")
View(drainc)
josie<-count(drainc,c("Line","drainagecl")) #frequency table
View(josie)
write.table(josie,file='./Data/drainageEW_table.csv', sep=",", col.names=NA)





#################################
#CUrrent land use
#################################
#species composition of vegetation in oak buffer
#uses subsetted data from above!

library(plyr)
aaa<-count(WWW,"CLASS") #freq table west, oak
View(aaa)
str(aaa)

Percentage<-(aaa$freq/nrow(WWW))*100 #add percentages based on # in oak/west
WestM<-cbind(aaa,Percentage)
WestM


write.table(WestM,file='./Data/WestMod.csv', sep=",", col.names=NA)

#species composition of vegetation in non-oak buffer

bbb<-count(EEE,"CLASS")
View(bbb)
str(bbb)

Percentage1<-bbb$freq/nrow(EEE)*100
EastM<-cbind(bbb,Percentage1)
EastM

write.table(EastM,file='./Data/EastMod.csv', sep=",", col.names=NA)


#
#
#



#################################
#composition
#################################

#species composition of vegetation in WEST

#for the FIA data, percent composition is out of all TREES, not points
#so the subsetted dataframes should be the TREES ones not the total points one
library(plyr)
A<-count(Wtree,"L3_tree") #freq table west, oak
View(A)
str(A)

Percentage<-(A$freq/nrow(Wtree))*100 #add percentages based on # in oak/west
WestC<-cbind(A,Percentage)
WestC
sum(WestC$Percentage)


write.table(WestC,file='./Data/WestComp4-11-17.csv', sep=",", col.names=NA)

#species composition of vegetation in EAST

B<-count(Etree,"L3_tree")
View(B)
str(B)

Percentage1<-B$freq/nrow(Etree)*100
EastC<-cbind(B,Percentage1)
EastC

write.table(EastC,file='./Data/EastComp4-13-17.csv', sep=",", col.names=NA)

###oak breakdown in WEST
c<-count(Wtree,"L2_tree")
View(c)
str(c)

Percentage2<-c$freq/nrow(Wtree)*100
OakCBR<-cbind(c,Percentage2)
OakCBR
str(OakCBR)

write.table(OakCBR,file='./Data/WestcompBreakdown4-11-17.csv', sep=",", col.names=NA)


#oak breakdown in EAST
ob<-count(Etree,"L2_tree")
View(ob)
str(ob)

Percentage5<-ob$freq/nrow(Etree)*100
NonoakCBR<-cbind(ob,Percentage5)
NonoakCBR
str(NonoakCBR)

write.table(NonoakCBR,file='./Data/EastcompBreakdown4-13-17.csv', sep=",", col.names=NA)



#######################################################################
#######################################################################
#figures
#######################################################################
#######################################################################


#################
#histogram of PC 1 score

library(ggplot2)

#read in main file- 5573 trees
#Buf<-read.csv("./Data/YR_Buffer_Clim_Dem_Soils_PCA.csv", header=TRUE)
#right now, using scoresfile from above




scoresfile <- read.csv("scoresEW.csv")
View(scoresfile)
  scoresfile$Line[scoresfile$Line=="2"]<-"West"
  scoresfile$Line[scoresfile$Line=="1"]<-"East"
str(scoresfile)
as.character(scoresfile$Line)

library(ggplot2)
g=ggplot(scoresfile, aes(PC1, fill=Line)) 
g + geom_histogram(binwidth=.25, alpha = 0.75, position = 'identity') + 
  scale_fill_manual(values=c("grey20", "grey70")) + 
  xlab("Component 1 scores") + ylab("Tree count") + theme_bw()






 
  
  
  


##################################################
#island
#################################################

pc=read.csv("scoresEW.csv") #put xy coordinates on this, by ID
dim(pc)

island=read.csv("IslandPoints.csv")
View(island)
dim(island)


#we can look at comp in the island
library(plyr)

iscomp<-count(island,"L3_tree")
View(iscomp)
str(iscomp)

PercentageIII<-iscomp$freq/nrow(island)*100
IslandC<-cbind(iscomp,PercentageIII)
IslandC
View(IslandC)
sum(IslandC$PercentageIII)
write.table(IslandC,file='IslandComp.csv', sep=",", col.names=NA)


#this is to join up island data with PCA scores
#instead of making tons of comparisons, this is a nice summary stat
library(plyr)
combo=join(x=island, y=pc, by="POINT_X", type="left")
dim(combo)  #joined PC output to island 617 by 68
View(combo)

subis=combo[,c("Line","PC1")]  #gotta put them all together
  subis$Line="Island"
submain=pc[,c("Line","PC1")]
  submain[submain=="2"]<-"West"
  submain[submain=="1"]<-"East"
  View(submain)
View(subis)
as.factor(submain$Line)
as.factor(subis$Line)
as.numeric(submain$PC1)
as.numeric(subis$PC1)
pctotal=merge(subis,submain, all=TRUE) #THE MERGE: REG POINTS AND ISLAND POINTS
View(pctotal)
dim(pctotal)
617 + 6126

mi=mean(subis$PC1) #[1] 0.5518835
si=sd(subis$PC1,na.rm=TRUE)/sqrt(length(which(!is.na(subis$PC1))))#[1] 0.06372023

jjjW=subset(submain, Line=="West")   #find mean and se of west
mw=mean(jjjW$PC1) #[1] -1.531985
sw=sd(jjjW$PC1,na.rm=TRUE)/sqrt(length(which(!is.na(jjjW$PC1))))#[1] 0.03378106
jjjE<-subset(submain, Line=="East")  
me=mean(jjjE$PC1) #[1] 1.474082
se=sd(jjjE$PC1,na.rm=TRUE)/sqrt(length(which(!is.na(jjjE$PC1))))#[1] 0.02417854


#anova for PCA score
anovaPC<- aov(PC1~Line, data=pctotal)
summary(anovaPC)
ug=TukeyHSD(anovaPC)
ug

mean(island$DEM)#[1] 249.1943
sd(island$DEM,na.rm=TRUE)/sqrt(length(which(!is.na(island$DEM))))#[1] 0.03485493




#bar graph PCA score 1 island!
M <- c(mw,me,mi)
S <- c(sw,se,si)



#function for error bars
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#creating plot
par(mar=c(5,5,5,5))
P <-barplot(M, ylim=c(-1.5,1.5), col = c('#ff663e','#751aff', 'darkseagreen3'), 
            names.arg=c("West","East","Eastern \n Oak Area"), ylab='PCA Component 1 Score',
            cex.axis=2,cex.lab=2,cex.names=2)       
error.bar(P,M,S)  
woot <- error.bar(P,M,S)
#segments(P, sw, se, si)

#CODE FOR WRITING PNG
png(width = 7, height = 7, units = 'in', res = 400, filename = "islandbar.png")
dev.off()






#simple histograms of diameters
hist(treess$TDiameter,xlim=c(0,15),breaks=60)
range(treess$TDiameter,na.rm=TRUE)
which(treess$TDiameter == 16)
#90 for 5, 283 for 6
library(ggplot2)
ggplot(data=treess, aes(x=TDiameter)) +
  geom_bar(stat="bin",binwidth=1) +
  scale_x_discrete(breaks=seq(1,10,1),limits=c("0","10"))

mean(treess$TDiameter,na.rm=TRUE) #15.7, this is in inches




