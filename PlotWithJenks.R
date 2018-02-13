#------------------------------------
# Simple mapping routine using plot method for spatial polygons data frame
#------------------------------------
{
#Import a shapefile
Honduras <- readOGR("c:\\temp\\HND_adm2.shp","HND_adm2")
Honduras <- spTransform(Honduras,CRS("+proj=longlat +datum=WGS84"))

hist(Honduras@data$perc_fores)
summary(Honduras@data$perc_fores)

#second argument must take a value of 3 to 9 colours

#using plot
#the findInterval function identifies the intervals according to the breaks.  all.inside=TRUE
#ensures that the colours cover the range of data
plot(Honduras,col=output[[2]][findInterval(Honduras$perc_fores,output[[1]],all.inside=TRUE)])
legend("topleft",legend=output[[3]],fill=output[[2]])

#test the colour ramp against some values to make sure it's correct
Honduras_1 <- Honduras[Honduras@data$perc_fores >= 1.33,]
plot(Honduras_1,border="red",add=TRUE)


