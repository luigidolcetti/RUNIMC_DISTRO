#'Some nice thing
#'
#'
#' @export
extractPolygons<-function(fn_polygonList){

  condensedPoligonList<-
    lapply(names(fn_polygonList),function(sld){

      lapply(names(fn_polygonList[[sld]]),function(mrkr){


        nRowPoly<-length(fn_polygonList[[sld]][[mrkr]]@polygons)

        if (nRowPoly!=0) {
          polygon.id=1:nRowPoly

          geoM<-sf::st_sfc(lapply(fn_polygonList[[sld]][[mrkr]]@polygons,function(x){
            sf::st_polygon(list(x))}))

          FFrame<-data.frame(uid=rep(sld,nRowPoly),
                             polygon.id=polygon.id,
                             label=rep(mrkr,nRowPoly),
                             stringsAsFactors = F)


          TEMP_poly<-sf::st_sf(FFrame,
                               geom=geoM,
                               sf_column_name = "geom",
                               stringsAsFactors = F,
                               sfc_last = T,
                               row.names = polygon.id)

          newIDs<-apply(TEMP_poly,1,digest::digest)

          oldNames<-colnames(TEMP_poly)
          polygonsList<-dplyr::bind_cols(TEMP_poly,pIDs=newIDs)[,c('pIDs',oldNames)]

        }
      })
    })


  condensedPoligonList<-unlist(condensedPoligonList,recursive = F)
  condensedPoligonList<-condensedPoligonList[!unname(unlist(lapply(condensedPoligonList,is.null)))]
  condensedPoligonList<-do.call(rbind.data.frame,condensedPoligonList)

  return(condensedPoligonList)
}
