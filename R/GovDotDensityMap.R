

#'@title GovDotDensityMap
#'@descreption
#'DotDensity mapping data by state
#'
#'@param data a data set that contains at least your states  and the variable to represent
#'@param Govcode a numeric that indicate the colum number that contains the states code
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
##'testG <- cbind(tnMAP$NAME_1,rnorm(n = 24,mean = 20,sd = 13),tnMAP$HASC_1)
#'testG=as.data.frame(testG)
#'testG[,2]=as.numeric(testG[,2])
#'GovDotDensityMap(testG,Govcode = 3,var=2,prov = providers$OpenStreetMap)
#'
#'@export

GovDotDensityMap=function(data,Govcode,var, DotWeight=0.5,polyEdgColor="blue",OneDFor=1,dotColor="lightgoldenrodyellow",prov=providers$NASAGIBS.ViirsEarthAtNight2012,polyColor="midnightblue"){
  library(leaflet)
  library(maptools)
  l=match(tnMAP$HASC_1,data[,Govcode])
  n=rep(0,24)

  n=as.numeric(unlist(data[l,var], use.names=FALSE))

  n=n/OneDFor

  k1=dotsInPolys(tnMAP,x=as.integer(n),compatible = T)
dat=0
dat2=0
for (i in 1:24) {
    for(j in 1:n[i]){
      dat=rbind(dat,k1[[i]][[j,1]])
      dat2=rbind(dat2,k1[[i]][[j,2]])

    }
  }
dat=dat[-1]
dat2=dat2[-1]
  dataxy=cbind(dat,dat2)

  m=leaflet(tnMAP)%>%addProviderTiles(provider = prov)%>%
    addPolygons(data=tnMAP,
                fillColor=polyColor,
                color = polyEdgColor,
                fillOpacity=10,
                weight=1.1,
                opacity=0.7)%>%
    addCircles(data = dataxy,lng = dataxy[,1],lat = dataxy[,2],color =dotColor,fill = T,weight = DotWeight,fillColor = dotColor,opacity = 1)

  m
}
