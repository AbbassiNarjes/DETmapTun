#' DETmap package
#'
#' this package is used to represent data on the tunisian map , it  contains four function , and two  datasets:
#'
#'
#'-Gov_function : Gov_function :this function is used when you want to show your data by governorates(states) on  the tuisian map ,
#'it takes as variables : data:,GOVcode ,var,-mapTitle,titleColor,mapColor,prov ,addLeg
#'
#'
#'-DEL_function :this function is used when you want to show your data by Delegation on  the tuisian map ,
#'it takes as variables : data:,DELEcode ,var,-mapTitle,titleColor,mapColor,prov ,addLeg
#'
#'-GovDotDensityMap :just like its name indicates , this function enable you to have a dot density map with your variable as the number of dots on each state
#'it takes as variables : data:,GOVcode ,var,polyColor,dotColor,prov, DotWeight,OneDFor
#'
#'-DELEDotDensityMap :just like its name indicates , this function enable you to have a dot density map with your variable as the number of dots on each delegation
#'it takes as variables : data:,DELEcode ,var,polyColor,dotColor,prov, DotWeight,OneDFor
#'
#'
#'this package also containes two datasets which are tnMAP and tnMAPDELE the  first contains the shape file along with the states data  names ,HASC_1 ...for the tunisian states and the second have the shape file for the delegations  along with theiir names delegation names ans their HASC_2
#'
#'
#'
#' @docType package
#'
#'
#' @author Data Expert
#'
#'@name DETmap
NULL
