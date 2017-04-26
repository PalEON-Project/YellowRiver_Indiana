#distance from line 
#3-31
setwd("~/Documents/YellowRiver_IN")

#this code combines Jody's code for density distance from line with Caitlin's 
#code for oak/non-oak distance from line and code for PC score dist from line
#there are three line calculations here:
  #original data, in Albers, for comparing composition
  #pca data, in Albers, for comparing environment
  #density data, in UTM, for comparing structure
#we have two projections because data from collaborators came from both NAD 1983 UTM zone 16 
  #and from NAD 1983 Great Lakes and St. Lawrence Albers



#read in line in Albers projection line
theline = read.csv('./Data/IntersectVertices_project.csv', stringsAsFactors = FALSE)
head(theline)
View(theline)






#TO JOIN THE DISTANCE FROM LINE TO MAIN DATASET:
#do not use UTM, use albers


forline=read.csv("./Data/EWAllData2-28.csv")
theline = read.csv('./Data/IntersectVertices_Project.csv', stringsAsFactors = FALSE)
#this is the same as the treess file used in other places
#but i am adding a column so I do not want to mess things up
#so diff name
head(forline)

distance = 1:length(forline$POINT_Y)

for (i in 1:length(forline$POINT_Y)) {
  dists <- (((theline$POINT_X - forline$POINT_X[i])^2) + ((theline$POINT_Y - forline$POINT_Y[i])^2))^.5 # euclidean distances to all points on the line from the tree
  # Get nearest distance to line
  distance[i] <- min(dists)
  # is the tree on the east or west side of the line? Negative values = west side (i.e., Oak Buffer)
  if (forline$Line[i] == 'West') distance[i] <- -distance[i]
  
}
forline$distance = distance
head(forline)
write.csv(forline, file = './Data/EWalldata_distfromline.csv')



#***



#PC DISTANCE FROM LINE!
#again, do not use UTM, use Albers

scoresfile=read.csv("./Data/scoresEW.csv",stringsAsFactors = FALSE) #output from PCA with EW assignment
View(scoresfile)   #this has PC scores, point_x, point_y
colnames(scoresfile)
head(scoresfile)

theline = read.csv('./Data/IntersectVertices_Project.csv', stringsAsFactors = FALSE)
View(theline)
#create new column for for loop output
distance = 1:length(scoresfile$POINT_Y)


#for loop to get the euclidean distance
for (i in 1:length(scoresfile$POINT_Y)) {
  dists <- (((theline$POINT_X - scoresfile$POINT_X[i])^2) + ((theline$POINT_Y - scoresfile$POINT_Y[i])^2))^.5 # euclidean distances to all points on the line from the tree
  # Get nearest distance to line
  distance[i] <- min(dists)
  # is the tree on the east or west side of the line? Negative values = west side (i.e., Oak Buffer)
  if (scoresfile$Line[i] == '2') distance[i] <- -distance[i]
  
}

scoresfile$distance = distance
head(scoresfile)
write.csv(scoresfile, file = './Data/PC_distfromline.csv')




#***



#density distance from line
#this does NOT use UTM anymore
#will stay in Albers
rm(list=ls())
setwd("~/Documents/YellowRiver_IN")




theline = read.csv('./Data/IntersectVertices_project.csv', stringsAsFactors = FALSE)
density = read.csv('./Data/PLSDensity_Type_4-25-17.csv', stringsAsFactors = FALSE) 
#NOTE - the YRDensity_Type_Buffer data is in the Albers projection

str(theline)
head(theline)
str(density)
head(density)

#create new column for the output from the for loop
distance = 1:length(density$POINT_Y)

#for loop to get the euclidean distance
for (i in 1:length(density$POINT_Y)) {
  dists <- (((theline$POINT_X - density$POINT_X[i])^2) + ((theline$POINT_Y - density$POINT_Y[i])^2))^.5 # euclidean distances to all points on the line from the tree
  # Get nearest distance to line
  distance[i] <- min(dists)
  # is the tree on the east or west side of the line? Negative values = west side (i.e., Oak Buffer)
  if (density$Line[i] == 'West') distance[i] <- -distance[i]
  
}

#add the distance as a new column to the original density dataframe 
density$distance = distance
head(density)
View(density)
write.csv(density, file = './Data/PLSDensity_Type_Distance_4-25-17.csv')




#next step: make the histogram in the figures r script!

