#'Some nice thing
#'
#'
#' @export
monkeyForest<-function(fn_rst = NULL,
                       fn_layers = NULL,
                       fn_newLayerName='label',
                       fn_undeterminedLabel='undetermined',
                       fn_Ptreshold=0,
                       fn_forest = NULL,
                       fn_filePath=NULL,
                       fn_uid = NULL,
                       fn_TempPath = NULL){

  newFilePath<-checkDir(file.path(fn_filePath,'rasters'),fn_rst@IMC_text_file)

  rfClasses<-fn_forest$classes
  rfClassNumber<-length(rfClasses)

  rstFilePath<-lapply(setNames(rfClasses,rfClasses),function(nms){
    file.path(newFilePath,paste0(nms,'.grd'))
  })

  newStack <-raster::predict(object = fn_rst,
                             model = fn_forest,
                             filename = file.path(fn_TempPath,paste0('TEMP_',fn_uid,'.nc')),
                             overwrite=T,
                             type='prob',
                             progress='text',
                             index=seq_along(rfClasses))

  names(newStack)<-rfClasses
  newRstList<-raster::as.list(newStack)
  names(newRstList)<-rfClasses

  newRstList<-lapply(setNames(rfClasses,rfClasses), function(nms){
    rstfilePath<-file.path(newFilePath,paste0(nms,'.grd'))
    newRaster<-raster::writeRaster(x = newRstList[[nms]],
                                   filename = rstfilePath,
                                   overwrite=T,
                                   format='raster')
    return(newRaster)
  })

  newRaster<-raster::stackApply(x = newStack,
                                indices = rep(1,rfClassNumber),
                                fun=function(x,
                                             trsh=fn_Ptreshold,
                                             na.rm){
                                  winnerClass<-which(x==max(x))
                                  if (length(winnerClass)==0) return(length(x)+1)
                                  if (length(winnerClass)>1) return(length(x)+1)
                                  if (x[winnerClass[1]]>trsh) return(winnerClass[1]) else return(length(x)+1)},
                                filename = file.path(fn_TempPath,paste0('TEMP_winner_',fn_uid,'.nc')),
                                overwrite = T)

  names(newRaster)<-fn_newLayerName
  trainingLabels<-c(rfClasses,fn_undeterminedLabel)

  rat<-data.frame(ID=seq_along(trainingLabels),label=trainingLabels)

  levels(newRaster)[[1]]<-rat

  newRaster<-raster::writeRaster(newRaster,
                                 file.path(newFilePath,paste0(fn_newLayerName,'.grd')),
                                 overwrite = T)

  unlink(file.path(fn_TempPath,paste0('TEMP_',fn_uid,'.nc')))
  unlink(file.path(fn_TempPath,paste0('TEMP_winner_',fn_uid,'.nc')))


  newRstList[[fn_newLayerName]]<-newRaster

  rstrStk<-IMC_stack(x = newRstList,
                     uid = fn_rst@uid,
                     IMC_text_file = fn_rst@IMC_text_file,
                     study = fn_rst@study,
                     sample = fn_rst@sample,
                     replicate = fn_rst@replicate,
                     ROI = fn_rst@ROI,
                     bioGroup = fn_rst@bioGroup,
                     channels = fn_rst@channels,
                     type = 'class')

  return(rstrStk)
}
