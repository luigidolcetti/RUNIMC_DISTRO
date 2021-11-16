#' lazyCatMap
#'
#' Simple engine function to transform a raster segmentation map (as obtained
#' by other segmentation tools) in a vectorized segmentation map
#' @param fn_srt raster, a rater usually stored in segmentation and imported
#'  via [importTiffMask].
#' @param fn_uid character, uid string referring to a specific ROI.
#' @param fn_indexToExclude numeric scalar, level that will be excluded from
#'  the segmentation, usually the value representing the background.
#' @details Engine function should not be used directly but instead through the
#'   appropriate method (e.g. [segment])
#' @export
lazyCatMap<-function (fn_srt=NULL,
                      fn_uid=NULL,
                      fn_indexToExclude=NULL){


  if (is.null(fn_uid)) fn_uid<-NA
  if (!is.null(fn_indexToExclude)) {
    for (i in fn_indexToExclude){
      fn_srt[fn_srt==i]<-NA
    }
  }

  newStars<-stars::st_as_stars(fn_srt)

  newPoly<-sf::st_as_sf(newStars,merge=T)
  newPoly<-sf::st_buffer(newPoly,0)
  colnames(newPoly)[1]<-'ID'
  #### new part ####

  newCondensedPoly<-lapply(unique(newPoly$ID),function(x){
    out<-sf::st_union(newPoly[newPoly$ID==x,])
    out<-sf::st_sf(sf_column_name = 'geom',
                   geom = out,
                   uid = fn_uid,
                   ID = x,
                   stringsAsFactors = F)
  })

  newCondensedPoly<-do.call(dplyr::bind_rows,newCondensedPoly)
  return(newCondensedPoly)

}



