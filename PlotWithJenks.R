#------------------------------------
# Simple mapping routine using plot method for spatial polygons data frame
#------------------------------------
simpclass <- function(theData,numclasses,colour)
{
  library(RColorBrewer)
  
  #because breakpoints and colours are related (# colours = breaks-1)
  numclasses <- numclasses + 1 
  
  #using something akin to jenks natural breaks
  clusters <- kmeans(theData,numclasses)
  jbreaks <- clusters$centers[order(clusters$centers),]
  for(i in 1:(numclasses))
  {
    if(i==1)
    {
      breaks <- min(theData)
    }
    if(i > 1 & i < numclasses)
    {
      breaks <- append(breaks,jbreaks[i])
    }
    if(i == numclasses)
    {
      breaks <- append(breaks,max(theData))
    }
  }

  #changing colours using colour brewer
  colour_ramp <- brewer.pal(n = numclasses-1, name = colour)

  #break labels
  for(i in 1:(numclasses-1))
  {
    if (i == 1)
    {
      break_labels <- c(paste(as.character(round(jbreaks[i],2)),"-",as.character(round(jbreaks[i+1],2))))
    }
    if(i > 1 & i < (numclasses-1))
    {
      temp <- c(paste(as.character(round(jbreaks[i],2)),"-",as.character(round(jbreaks[i+1],2))))
      break_labels <- c(break_labels,temp)
    }
    if (i == (numclasses-1))
    {
      temp <- c(paste(as.character(round(jbreaks[(numclasses-1)],2)),"+"))
      break_labels <- c(break_labels,temp)      
    }
  }
  return(list(breaks,colour_ramp,break_labels))
}

#------------------------------------
# Implementation
#------------------------------------

library(rgdal)
#Import a shapefile
Honduras <- readOGR("c:\\temp\\HND_adm2.shp","HND_adm2")
Honduras <- spTransform(Honduras,CRS("+proj=longlat +datum=WGS84"))

hist(Honduras@data$perc_fores)
summary(Honduras@data$perc_fores)

#second argument must take a value of 3 to 9 colours
output <- simpclass(Honduras@data$perc_fores,9,"Greens")

#using plot
#the findInterval function identifies the intervals according to the breaks.  all.inside=TRUE
#ensures that the colours cover the range of data
plot(Honduras,col=output[[2]][findInterval(Honduras$perc_fores,output[[1]],all.inside=TRUE)])
legend("topleft",legend=output[[3]],fill=output[[2]])

#test the colour ramp against some values to make sure it's correct
Honduras_1 <- Honduras[Honduras@data$perc_fores >= 1.33,]
plot(Honduras_1,border="red",add=TRUE)


