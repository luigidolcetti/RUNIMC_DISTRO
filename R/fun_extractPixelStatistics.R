#'
#' @export
.extractPixelStatistics<-function(fn_polygons,
                                 fn_raster,
                                 fn_function,
                                 verbose=T,
                                 cl=NA,
                                 ...){

  prgrsbr<-verbose
  if (!any(is.na(cl))){
    parallel::clusterExport(cl=cl,
                            varlist = c('fn_raster',
                                        'fn_polygons',
                                        'fn_function',
                                        'prgrsbr'),
                            envir = environment())

  }

  if (!any(is.na(cl))){

    value_x<-parallel::parLapply(unique(fn_polygons$uid),function(x){

      sub_raster<-fn_raster[[x]]
      sub_condensedPolygon<-fn_polygons[fn_polygons$uid==x,]
      sub_condensedPolygon<-sf::st_sf(sub_condensedPolygon,sf_column_name = 'geom')
      value<-exactextractr::exact_extract(x = sub_raster,
                                          y = sub_condensedPolygon,
                                          fun = fn_function,
                                          progress = prgrsbr,
                                          force_df = T)

      colnames(value)<-names(sub_raster)
      value<-dplyr::bind_cols(pIDs=sub_condensedPolygon[,'pIDs',drop=T],value)

    },cl = cl)

  } else {

    value_x<-lapply(unique(fn_polygons$uid),function(x){

      sub_raster<-fn_raster[[x]]
      sub_condensedPolygon<-fn_polygons[fn_polygons$uid==x,]
      value<-exactextractr::exact_extract(x = sub_raster,
                                          y = sub_condensedPolygon,
                                          fun = fn_function,
                                          progress = prgrsbr,
                                          force_df = T)

      colnames(value)<-names(sub_raster)
      value<-dplyr::bind_cols(pIDs=sub_condensedPolygon[,'pIDs',drop=T],value)

    })

  }

  value_x<-do.call(rbind.data.frame,value_x)
  return(value_x)

}
