#'@title DELEDotDensityMap
#'@description
#'DotDensity mapping data by state
#'
#'@param data a data set that contains at least your delegations  and the variable to represent
#'@param DELEcode a numeric that indicate the colum number that contains the delegations code
#'@param var a numeric that indicate the colum number that contains  your variable
#'@param DotWeight the dot weight
#'@param polyEdgColor the boundries color
#'@param polyColor the polygons filling color
#'@param OneDFor the unit (exp: 60 to present 60 votes per dot )
#'@param prov : it have to be in format : providers$... and the provider you want to have in the background you can see all the providers by typing View(providers) or going to https://leaflet-extras.github.io/leaflet-providers/preview/ where you can fin all the providers and how they look like
#'@param dotColor the dots color
#'
#'
#'
#'@return a map with your data
#'
#'@examples
#'testD <- cbind(tnMAPDELE@data[["NAME_2"]],tnMAPDELE@data[["HASC_2"]],tnMAPDELE@data[["NAME_1"]],rnorm(268,50,20))
#'testD=as.data.frame(testD)
#'testD[,4]=as.numeric(testD[,4])
#'DELEDotDensityMap(testD,DELEcode = 2, polyEdgColor= "red" ,var=4,OneDFor = 60)
#'
#'
#'@export


DELEDotDensityMap=function(data,DELEcode,var, DotWeight=0.5,polyEdgColor="blue",OneDFor=50,dotColor="lightgoldenrodyellow",prov=providers$NASAGIBS.ViirsEarthAtNight2012,polyColor="midnightblue"){
  library(leaflet)
  library(maptools)
  l=match(tnMAPDELE$HASC_2,data[,DELEcode])
  n=rep(0,268)
  n=as.numeric(unlist(data[l,var], use.names=FALSE))

  n=n/OneDFor

  k1=dotsInPolys(tnMAPDELE,x=as.integer(n),compatible = T)

  dat=NULL
  dat2=NULL

  for (i in 1:268) {
    if(n[i]>0){
      for(j in 1:n[i]){
        dat=rbind(dat,k1[[i]][[j,1]])
        dat2=rbind(dat2,k1[[i]][[j,2]])}

    }
  }
  dataxy=cbind(dat,dat2)

  m=leaflet(tnMAPDELE)%>%addProviderTiles(provider = prov)%>%
    addPolygons(data=tnMAPDELE,
                fillColor=polyColor,
                color = polyEdgColor,
                fillOpacity=10,
                weight=1.1,
                opacity=0.7)%>%
    addCircles(data = dataxy,lng = dataxy[,1],lat = dataxy[,2], weight  =  DotWeight,color =dotColor,fill = T,fillColor = dotColor,opacity = 1)

  m
}
