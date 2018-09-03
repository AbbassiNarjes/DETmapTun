#'Gov_function
#'mapping data by state
#'
#'@param data a data set that contains at least your states and the variable to represent
#'@param GOVcode a numeric that indicate the colum number that contains the states code
#'@param var a numeric that indicate the colum number that contains  your variable
#'@param mapTitle the map title
#'@param titleColor the color for the title
#'@param mapColor the map color
#'@param prov : it have to be in format : providers$... and the provider you want to have in the background you can see all the providers by typing View(providers) or going to https://leaflet-extras.github.io/leaflet-providers/preview/ where you can fin all the providers and how they look like
#'@param showVar1 : the variables you want to show in the highlight box on the map
#'@param showVar2 : the variables you want to show in the highlight box on the map
#'@param addLeg : a boolean to set to true in you want to have legend by the side of the map
#'
#'
#'

#'@return a map with your data
#'
#'@examples
#'
#'testG <- cbind(tnMAP$NAME_1,rnorm(n = 24,mean = 20,sd = 13),tnMAP$HASC_1)
#'testG=as.data.frame(testG)
#'testG[,2]=as.numeric(testG[,2])
#'Gov_function(data = testG,var=2,mapTitle= "new MAp ",titleColor = "red",mapColor =c("white","red"),GOVcode =  3,addLeg = T,prov = providers$OpenStreetMap,showVar1 = 1,showVar2 = 2)
#'
#'@export
Gov_function <- function(data,var,mapTitle="",mapColor=NULL,titleColor="black",GOVcode,showVar1,showVar2,addLeg=T,prov=providers$Esri.WorldImagery)
{library(leaflet)
  library(devtools)
  library(htmlwidgets)
  library(htmltools)

  i=match(tnMAP$HASC_1,data[,GOVcode])

  dataB<- cbind.data.frame(data[i,],tnMAP$HASC_1)
  m=NULL
  m=leaflet(dataB)
  m=m%>% addProviderTiles(provider = prov)

  labels<-sprintf("<strong>%s<br/></strong>%g<br/>",dataB[,showVar1],dataB[,showVar2])
  labels<-paste(labels)%>%lapply(htmltools::HTML)

  if(is.null(mapColor)){
    m=m %>% addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
    m=m%>% addPolylines(data=tnMAP,
                        col="black",
                        weight=1.1,
                        opacity=0.7,
                        highlight=highlightOptions(weight=4.0,
                                                   color="#FFFFFF",
                                                   fillOpacity = 0.7,
                                                   bringToFront = TRUE),
                        label=labels,
                        labelOptions=labelOptions( style = list("font-weight" = "normal",
                                                                padding = "3px 8px"),
                                                   textsize = "15px",
                                                   direction = "auto"))


  }

  else{
    col<-colorRampPalette(mapColor)

    pal<-colorNumeric(col(24),domain = dataB[,var],n=24)
    m=m%>% addPolygons(data=tnMAP,
                       fillColor=~pal(dataB[,var]),
                       fillOpacity=10,
                       col="black",
                       weight=1.1,
                       opacity=0.7,
                       highlight=highlightOptions(weight=4.0,
                                                  color="#FFFFFF",
                                                  fillOpacity = 0.7,
                                                  bringToFront = TRUE),
                       label=labels,
                       labelOptions=labelOptions( style = list("font-weight" = "normal",
                                                               padding = "3px 8px"),
                                                  textsize = "15px",
                                                  direction = "auto"))
    if (addLeg == T){
      m<-m%>%addLegend(data=dataB,
                       pal=pal,
                       values=~dataB[,var],
                       opacity=1.5,
                       title = colnames(dataB)[var])}
  }
  addTitle = function(object,
                      text,
                      color = titleColor,
                      fontSize = "20px",
                      fontFamily = "Sans",
                      leftPosition = 50,
                      topPosition = 2){

    htmlwidgets::onRender(object, paste0("
                                         function(el,x){
                                         h1 = document.createElement('h1');
                                         h1.innerHTML = '", text ,"';
                                         h1.id='titleh1';
                                         h1.style.color = '",color,"';
                                         h1.style.fontSize = '",fontSize,"';
                                         h1.style.fontFamily='",fontFamily,"';
                                         h1.style.position = 'fixed';
                                         h1.style['-webkit-transform']='translateX(-50%)';
                                         h1.style.left='",leftPosition ,"%';
                                         h1.style.top='",topPosition,"%';
                                         document.body.appendChild(h1);
                                         }"))
}
  addTitle(m,mapTitle,color=titleColor,fontSize = "30px",leftPosition=30)
  }
