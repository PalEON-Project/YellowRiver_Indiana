#4-25-2017
#historic density

#setwd("~/Documents/YR/YellowRiver_Indiana")
setwd("~/Documents/YellowRiver_IN")

#import E and W separately from GIS: do not yet have "Line" designation
wdens.init = read.csv("./Data/West_Density_4-21-17.csv",stringsAsFactors = FALSE)
edens.init = read.csv("./Data/East_Density_4-21-17.csv",stringsAsFactors = FALSE)

#round the coordinates so that thhey can be joined with the PLS
round(wdens.init$POINT_X, digits = 2)
round(edens.init$POINT_X, digits = 2)
length(unique(wdens.init$POINT_X)) #1741
length(unique(edens.init$POINT_X)) #1627

#add line designation: allows joining at end, and for calculating distance from line
wdens.init$Line="West"
edens.init$Line="East"
head(wdens.init)
head(edens.init)

dim(wdens.init) #[1] 1741    6
dim(edens.init) #[1] 1627    6
1741+1627 #3368  = 3383-15 so missing the No Data points = good

#bring the density files together, now that they have east/west designations
#this is so that the ENTIRE density file can be matched with the PLS notes
totaldens=merge(wdens.init,edens.init,all=TRUE)
dim(totaldens) #3368 by 6




#now for the PLS notes
#this has the notree and nodata designations
#we want to exclude the nodata from this fil
#and then we want to join it with the density
#and then we will exclude the water points
pls = read.csv("./Data/YR_corners_4-20-17_UTM.csv", stringsAsFactors=FALSE)
#this has both coordinate systems: (x) for albers and (X_UTM for NAD 83)
length(unique(pls$x))  #3383 
round(pls$x, digits = 2)     #round just like the density shapefile data
length(unique(pls$x)) #still 3383
head(pls)

plssel = pls[,c("x","water","notree","nodata")]  #subset the large PLS dataframe
unique(plssel$nodata) #only yes and ""
plssel.rem.ND = plssel[plssel$nodata!="yes",]
dim(plssel.rem.ND) #3368! removed the NAs. now the same length as the density data
names(plssel.rem.ND)[1] = "POINT_X"   #rename this column name to match the one in the dens file
head(plssel.rem.ND)


#JOIN THE DENSITY FILE WITH THE PLS SELECTED DATAFRAME
library(plyr)
density.clean=join(x=totaldens, y=plssel.rem.ND, by="POINT_X", type="left")
dim(density.clean)  #3368 by 9!
head(density.clean)


#Now the density file has water designations
#can take those out
unique(density.clean$water) #only yes and ""
dens.rem.Wat = density.clean[density.clean$water==" ",]
unique(dens.rem.Wat$water)
dim(dens.rem.Wat) #3099
#there are 46 (east) and 223 (west) water points based on overall code
#which equals 269
#3368 total data corners - 269 = 3099
#so everything checks out!
3368-(46+223)


#rename it because it's the important one
finaldens = dens.rem.Wat

#subset the NEW file by east and west so that means can be calculated for each side
#they were subset before, but they included nodata and water
wdens=subset(finaldens, Line=="West") 
dim(wdens) #1518

edens=subset(finaldens, Line=="East")
dim(edens) #1581
1518+1581 #3099 math checks out
 

#calculating means and SEs for each
dWW=mean(wdens$density,na.rm=TRUE)
dWW #[1] exactly 47 trees/ha (47.1)

seWW=sd(wdens$density,na.rm=TRUE)/sqrt(length(which(!is.na(wdens$density))))
seWW #[1] 3.15

dEE=mean(edens$density,na.rm=TRUE)
dEE #[1] 183

seEE=sd(edens$density,na.rm=TRUE)/sqrt(length(which(!is.na(edens$density))))
seEE #[1] 6.856

density=list(dWW,seWW,dEE,seEE)
density

name=c("West density","West SE","East density","East SE")

names(density)=name
density



a=t.test(density~Line, var.equal=T, data=finaldens, na.rm=TRUE)
a

denst=list(density,a)
denst

sink(file = paste("./Results/PLSdensity_YR_4-25-17.txt", sep=""))
denst
sink()

#printed these results on 4-25-17,rewrote old file that did not have updated corrections
#these are the txt results...





#now going to add prairie savanna forest designations based on
  #Anderson and Anderson 1975
finaldens$Type="NA"

for (i in 1:length(finaldens$Type)){
  if (finaldens$density[i] < 0.5){
      finaldens$Type[i]="Prairie"
  }else{
    if (finaldens$density[i] >47){
      finaldens$Type[i]="Forest"
    }else{
      finaldens$Type[i]="Savanna"
    }
  }
}


View(finaldens)
head(finaldens) # FID density basal  POINT_X  POINT_Y Line water notree nodata
dim(finaldens) #3099

write.csv(finaldens,"./Data/PLSDensity_Type_4-25-17.csv")
#next step: go to line div code FULL.r to add distance from midline!





#the ISLAND density

#ok, so: 
  #1: Get density data to island points
    #in albers...
  #2: remove water points
  #3: calculate average density


#read in island density file: does not have notree/water designation
islanddd=read.csv("./Data/Island_density_albers_4-25-17.csv",stringsAsFactors = FALSE)
dim(islanddd) #322 by 5    
head(islanddd)


#back to plssel    this has all the PLS notes but only POINT_X water notree and nodata
plssel.for.is=plssel
names(plssel.for.is)[1] = "POINT_X" 




#the join of pls designations with bare density file! adding water notree nodata to the island corners
island.dens.pls=join(x=plssel.for.is, y=islanddd, by="POINT_X", type="right")
dim(island.dens.pls)
head(island.dens.pls)

#remove nodata and water points
is.rem.ND = island.dens.pls[island.dens.pls$nodata!="yes",]   #remove NA from PLS data for island
dim(is.rem.ND) #322, no nodata points
is.rem.W = is.rem.ND[is.rem.ND$water==" ",]   #remove NA from PLS data for island
dim(is.rem.W)
#315!


#now to calculate average
mid=mean(is.rem.W$density) #[1] 85.81832
mid #88.38
sid=sd(is.rem.W$density,na.rm=TRUE)/sqrt(length(which(!is.na(is.rem.W$density))))#[1] 8.519587
sid #9.007362



#to run ANOVA, will compare island density with east and west densitites

#what are the columns in the main density and the island density files?
head(finaldens) # FID density basal  POINT_X  POINT_Y Line water notree nodata
head(is.rem.W) #POINT_X water notree nodata FID   density     basal  POINT_Y
is.rem.W$Line="Island"  #make "Line" designation "Island"

#gotta combine with main density file to run ANOVA
is.se.aov=is.rem.W[c("POINT_X","density","Line")]
head(is.se.aov)
main.se.aov=finaldens[c("POINT_X","density","Line")]
head(main.se.aov)

island=list(mid,sid)
names(island)<-c("Mean density island", "se density island")
island

#Merge it
dens.aov=merge(is.se.aov,main.se.aov, all=TRUE) #THE MERGE: REG POINTS AND ISLAND POINTS for DENSITY
dim(dens.aov) #[1] 3414
dim(main.se.aov) #[1] 3099
dim(is.se.aov) #[1] 315
315 + 3099 #le check   = 3414


#going to export this file so that I can use it in the "figure code" file
write.table(dens.aov,file='./Data/DensitywithIsland.csv', sep=",", col.names=NA)

#ANOVA!
#anova for density
anovaDENS<- aov(density~Line, data=dens.aov)
aovsum = summary(anovaDENS)
tuk=TukeyHSD(anovaDENS)
tuk

#listing output so far: ANOVA, averages, tukey
aovD=list(island,aovsum,tuk)
aovD

#all list output density!

densalloutput=list(denst,aovD)
densalloutput

sink(file = paste("./Results/PLSdensity_YR_4-25-17.txt", sep=""))
densalloutput
sink()


