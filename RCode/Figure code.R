#figure making
#3/31/2017
#setwd("~/Dropbox/YR_Manuscript/YellowRiver_Indiana")
#setwd("~/Documents/YR/YellowRiver_Indiana")
setwd("~/Documents/YellowRiver_IN")
#figure 1
#did not use R: ArcMap, and Transeau's map


#Figure 2:
#two histograms: one showing east/west comp, other showing density

library(ggplot2)


#oak/non oak histogram, by x-coordinate distance from midline
c=read.csv("./Data/EWalldata_distfromline.csv")  #already created csv
c$distance
#probably want to subset the oak and non-oak groups
unique(c$Group_)
oakother=subset(c, Group_=="Oak" | Group_=="Other") #Now only 5692

oakother$Group_= gsub("Other","Non-Oak",oakother$Group_)

#in black and white
h=ggplot(oakother, aes(distance, fill=Group_))
h + geom_histogram(binwidth=1000, alpha = 0.75, position = 'identity') + 
  scale_fill_manual(values=c("grey10", "grey80")) + 
  xlab("Distance (m) from Intersection Line") + 
  ylab("Tree Count") + 
  guides(fill=guide_legend(title="Tree")) +
  theme_bw() +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=28,face="bold"))

#in color
h=ggplot(oakother, aes(distance, fill=Group_))
h + geom_histogram(binwidth=1000, alpha = 0.75, position = 'identity') + 
  scale_fill_manual(values=c('#5353c6','#ff8c66')) + 
  xlab("Distance (m) from Intersection Line") + 
  ylab("Tree Count") + 
  guides(fill=guide_legend(title="Tree")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=28,face="bold"))





#CODE FOR WRITING PNG
png(width = 10, height = 10, units = 'in', res = 400, filename = "./Figures/ComphistCOL4-6-17.png")
dev.off()

#density histogram!


d=read.csv("./Data/PLSDensity_Type_Distance.csv", stringsAsFactors = FALSE)
sf=subset(d,Type=="Savanna" | Type=='Forest')

#sf$Type= gsub("Forest","zForest",sf$Type)

savannaforest=sf
savannaforest




#savannaforest=sf[order(sf$Type,decreasing=TRUE),]
#View(savannaforest)



h=ggplot(savannaforest, aes(distance, fill=Type))
h + geom_histogram(binwidth=1000, alpha = 0.75, position = 'identity') + 
  scale_fill_manual(values=c('#5353c6','#ff8c66')) + 
  xlab("Distance (m) from Intersection Line") + 
  ylab("Corner Count") + 
  guides(fill=guide_legend(title="Type")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=28,face="bold"))

png(width = 10, height = 10, units = 'in', res = 400, filename = "./Figures/Denshist.png")
dev.off()







#Fig 3

# has PCA biplot and two scatter plots


#FOR PCA AND BIPLOT!
treess=read.csv("./Data/EWAllData2-28.csv")

PCAdata<-treess[,c("temp","precip","DEM","sandtotal_","silttotal_",
                   "claytotal_","om_r","ksat_r","awc_r","Line","ID","POINT_X","POINT_Y")]

names(PCAdata)
names=c("Temp","Precip","DEM","Sand","Silt","Clay","OM","Ksat","Awc","Line","ID","POINT_X","POINT_Y")
colnames(PCAdata)<-names
head(PCAdata)
PCA<-prcomp(na.omit(PCAdata[1:9]), retx=TRUE, scale. = TRUE)
summary(PCA)



library(ggplot2)


#run ggbiplotedited code!




#b/w labels
g=ggbiplotedited(PCA, obs.scale = 1, var.scale = 1, ellipse = FALSE,
                 groups = PCAdata$Line, labels.size = 1, varname.size = 6, varname.adjust =1.5, alpha = 0) +
  geom_point(aes(shape=PCAdata$Line,colour=PCAdata$Line,var.axes=TRUE)) +
  scale_shape_manual(values=c(4,1), name="Area") +
  scale_color_manual(values=c('#ffffff33','#00000033'), name="Area") +
  scale_shape_discrete(name  ="Area") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="grey70")) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", 
                                  face="bold", size=18)) + theme(legend.position = "top")
g

png(width = 11, height = 6, units = 'in', res = 300, filename = "./Figures/PCAbw.png")
dev.off()

#g$layers <- c(geom_point(data = PCAdata, aes(x = Comp.1, y = Comp.2, color = "black")), g$layers)
g$layers
g$layers <- c(g$layers,g$layers[[2]])
g
#myplot$layers <- c(myplot$layers, myplot$layers[[2]])
g$layers[[1]]



#b/w with NO labels
g=ggbiplotedited(PCA, obs.scale = 1, var.scale = 1, ellipse = FALSE,
                 groups = PCAdata$Line, labels.size = 1, varname.size = 0, varname.adjust =1.5, alpha = 0) +
  geom_point(aes(shape=PCAdata$Line,colour=PCAdata$Line,var.axes=TRUE)) +
  scale_shape_manual(values=c(4,1), name="Area") +
  scale_color_manual(values=c('#ffffff33','#00000033'), name="Area") +
  scale_shape_discrete(name  ="Area") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="grey70")) + 
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", 
                                  face="bold", size=18)) + theme(legend.position = "top")
g
#g$layers <- c(geom_point(data = PCAdata, aes(x = Comp.1, y = Comp.2, color = "black")), g$layers)
g$layers
g$layers <- c(g$layers,g$layers[[2]])
g
#myplot$layers <- c(myplot$layers, myplot$layers[[2]])
g$layers[[1]]

png(width = 11, height = 6, units = 'in', res = 300, filename = "./Figures/PCAbwNOlabs_4-3.png")
dev.off()






#scatter plot for elevation


elevdist=read.csv("./Data/EWalldata_distfromline.csv")  #already created csv
View(elevdist)

plot(elevdist$distance,elevdist$DEM,abline(v=0),cex.axis=2.5) 

png(width = 10, height = 9, units = 'in', res = 400, filename = "./Figures/DEMvLine_3-27.png")
dev.off()

#scatter plot for PC1

pcdist=read.csv('./Data/PC_distfromline.csv')
plot(pcdist$distance,pcdist$PC1,xlab=NULL,
     ylab=NULL,abline(v=0),cex.axis=2.5) #this is the same but using permanent csv

#CODE FOR WRITING PNG
png(width = 10, height = 9, units = 'in', res = 400, filename = "./Figures/PC1vLine_3-27.png")
dev.off()




#figure 4: no r work


#Fig S1: buffers, no r work

#Fig S2: oak breakdown...


#done in GIS but a reproduction of the code here to look at percentages
#this is also found in the larger code
#not really necessary, not producing the output csv here
treess=read.csv("./Data/EWAllData2-28.csv")
WWW<-subset(treess, Line=="West")
dim(WWW) #3004 x 56
str(WWW)

library(plyr)
c<-count(WWW,"verbatim")
View(c)
str(c)

Percentage<-c$freq/nrow(WWW)*100
OakCBR<-cbind(c,Percentage)
OakCBR
str(OakCBR)


#fig S3 island
#the map has no code, everything selected for in GIS


#fig S4 island bar plot comparing PC1 scores
#the bigger combinations and everything in larger code file
#but will reproduce the code that went into the bar graph here


pc=read.csv("./Data/scoresEW.csv") #put xy coordinates on this, by ID
dim(pc)

island=read.csv("./Data/IslandPoints.csv")
View(island)
dim(island)

#this is to join up island data with PCA scores
#instead of making tons of comparisons, this is a nice summary stat
library(plyr)
combo=join(x=island, y=pc, by="POINT_X", type="left")
dim(combo)  #joined PC output to island 617 by 68
View(combo)

#gotta put them all together
subis=combo[,c("Line","PC1")]  #just line and PC from island
subis$Line="Island"             #calling the island "line" -> Island
submain=pc[,c("Line","PC1")] #just line and PC from main dataset
submain[submain=="2"]<-"West"   #right now is 2: set it -> West
submain[submain=="1"]<-"East"   #right now is 1L set it -> East
View(submain)
View(subis)
as.factor(submain$Line)
as.factor(subis$Line)
as.numeric(submain$PC1)
as.numeric(subis$PC1)
pctotal=merge(subis,submain, all=TRUE) #THE MERGE: REG POINTS AND ISLAND POINTS
View(pctotal)
dim(pctotal) #6743 * 2
617 + 6126   #yay 6743

mi=mean(subis$PC1) #[1] 0.5518835
si=sd(subis$PC1,na.rm=TRUE)/sqrt(length(which(!is.na(subis$PC1))))#[1] 0.06372023

jjjW=subset(submain, Line=="West")   #find mean and se of west
mw=mean(jjjW$PC1) #[1] -1.531985
sw=sd(jjjW$PC1,na.rm=TRUE)/sqrt(length(which(!is.na(jjjW$PC1))))#[1] 0.03378106
jjjE<-subset(submain, Line=="East")  
me=mean(jjjE$PC1) #[1] 1.474082
se=sd(jjjE$PC1,na.rm=TRUE)/sqrt(length(which(!is.na(jjjE$PC1))))#[1] 0.02417854

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
png(width = 7, height = 7, units = 'in', res = 400, filename = "./Figures/islandbar.png")
dev.off()


#still need to add code in here for density bar plot!









