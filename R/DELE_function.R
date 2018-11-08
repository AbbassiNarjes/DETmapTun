#' @title DELE_function
#'
#' @description mapping data by delegation
#'
#'@param data a data set that contains at least your states and the variable to represent
#'@param DELEcode a numeric that indicate the colum number that contains the Delegations code
#'@param var a numeric that indicate the colum number that contains  your variable
#'@param mapTitle the map title
#'@param titleColor the color for the title
#'@param mapColor the map color
#'@param prov : it have to be in format : providers$... and the provider you want to have in the background you can see all the providers by typing View(providers) or going to https://leaflet-extras.github.io/leaflet-providers/preview/ where you can fin all the providers and how they look like
#'@param showVar1 : the variables you want to show in the highlight box on the map
#'@param showVar2 : the variables you want to show in the highlight box on the map
#'@param showVar3 : the variables you want to show in the highlight box on the map
#'@param addLeg : a boolean to set to true in you want to have legend by the side of the map
#'@param GovBoundaryColor : the color for the states boundaries
#'
#'@return a map with your data
#'
#'@examples
#'
#'testD <- cbind(tnMAPDELE@data[["NAME_2"]],tnMAPDELE@data[["HASC_2"]],tnMAPDELE@data[["NAME_1"]],rnorm(268,50,20))
#'testD=as.data.frame(testD)
#'testD[,4]=as.numeric(testD[,4])
#'DELE_function(data = testD,var=4,mapTitle= "new MAp ",titleColor = "red",mapColor = NULL, DELEcode =   2,showVar1 = 1,showVar2 = 4,showVar3 = 3,addLeg = T,prov= providers$Esri.OceanBasemap,GovBoundaryColor="black")

#'
#'@export
DELE_function <- function(data,DELEcode,var,mapTitle="",mapColor=NULL,titleColor="black",showVar1,showVar2,showVar3,addLeg=T,prov=providers$Esri.WorldImagery,GovBoundaryColor="black")
{
  library(leaflet)
  library(devtools)
  library(htmlwidgets)
  library(htmltools)
  i=match(tnMAPDELE$HASC_2,data[,DELEcode])
  m=NULL
  dataB<- cbind.data.frame(data[i,],tnMAPDELE)
dataB[is.na(dataB[,var]),var]=0

  m=leaflet(dataB)

  m=m%>% addProviderTiles(provider = prov)

  labels<-sprintf("<strong>%s<br/> <strong>%g<br/><strong>%s<br/>",dataB[,showVar1],dataB[,showVar2],dataB[,showVar3])
  labels<-paste(labels)%>%lapply(htmltools::HTML)

  if(is.null(mapColor)){
    m=m%>% addPolylines(data=tnMAPDELE,
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

    pal<-colorNumeric(col(268),domain = dataB[,var],n=268)
    m=m%>% addPolygons(data=tnMAPDELE,
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
  if(!is.null(GovBoundaryColor)){
    m=m%>% addPolylines(data=tnMAP,
                        weight=3,
                        opacity=0.7,color=GovBoundaryColor)}

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
