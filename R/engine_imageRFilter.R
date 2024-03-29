#' Filters
#'
#' [imageRFilter()] is the wrapping function that apply [imager::vanvliet()],
#'   [imager::deriche()] or [imager::blur_anisotropic()] to a rater.
#'
#' @param fn_rasterStack IMC_RasterStack to be processed.
#' @param fn_filter character, filter to be applied. Available filters are
#'   vanvliet, deriche or blur_anisotropic.
#' @param fn_markerList character, vector name of layers to be processed.
#' @param fn_filterParameterList list, parameter to be passed to the filter
#'   function.
#' @param fn_pathToFile character, path to a folder that will contain
#'   the subfolder rasters and rasterStacks
#' @return a new rasterstack with processed rasters
#' @return what.
imageRFilter<-function(fn_rasterStack,
                       fn_filter,
                       fn_markerList,
                       fn_filterParameterList,
                       fn_pathToFile){


  defaults<-formals(eval(parse(text=paste0('imager::',fn_filter))))

  new_rasterStack<-sapply(names(fn_rasterStack),function(x)list(),USE.NAMES = T)


  for (st in names(fn_rasterStack)){

    xmn<-fn_rasterStack[[st]]@extent[1]
    xmx<-fn_rasterStack[[st]]@extent[2]
    ymn<-fn_rasterStack[[st]]@extent[3]
    ymx<-fn_rasterStack[[st]]@extent[4]

    pathDir<-checkDir(file.path(fn_pathToFile,'rasters'),fn_rasterStack[[st]]@IMC_text_file)
    new_rstStk<-list()

    for (i in fn_markerList){
      for (p in fn_filterParameterList){
        parametersList<-append(list(im=imager::as.cimg(fn_rasterStack[[st]][[i]])),p)
        parametersList<-append(parametersList,defaults[!(names(defaults) %in% names(parametersList))])
        newName<-paste(fn_filter,i,paste0(unlist(lapply(names(p),function(x){paste0(x,'_',p[x])}),recursive = F),collapse = '.'),sep='_')
        df<-do.call(eval(parse(text=paste0('imager::',fn_filter))),parametersList)
        df<-raster::raster(t(as.matrix(df)),xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,crs=sp::CRS(as.character(NA)))
        names(df)<-newName

        pathComlete<-file.path(pathDir,paste0(newName,'.nc'))
        raster::writeRaster(x = df,
                            filename = pathComlete,
                            overwrite=T,
                            format='CDF')
        df<-raster::raster(pathComlete)

        new_rstStk[[newName]]<-df
      }
      new_rasterStack[[st]]<-IMC_stack(new_rstStk,
                                       uid=st,
                                       IMC_text_file=fn_rasterStack[[st]]@IMC_text_file,
                                       study=fn_rasterStack[[st]]@study,
                                       sample=fn_rasterStack[[st]]@sample,
                                       replicate=fn_rasterStack[[st]]@replicate,
                                       ROI=fn_rasterStack[[st]]@ROI,
                                       bioGroup=fn_rasterStack[[st]]@bioGroup,
                                       type='calc')
    }
  }
  return(new_rasterStack)
}
