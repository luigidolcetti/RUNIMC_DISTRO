#' @export
randomOnions_parallel<-function(fn_rstStack=NULL,
                                fn_layerLabel=NULL,
                                fn_raster = NULL,
                                fn_derivedRaster = NULL,
                                fn_label=NULL,
                                fn_classifiers=NULL,
                                fn_prefix='topoMap_',
                                fn_analysisFolder=NULL,
                                cl = NA,
                                ...){

  if (is.null(fn_layerLabel)) stop(mError('no classification layer specified'),call. = F)
  if (is.null(fn_label)) stop(mError('no classification label specified'),call. = F)
  if (is.null(cl)) stop (mError('use the non-parallel randomOnions()'),call. = F)
  if (is.null(fn_analysisFolder)) stop (mError('analysis Folder cannot be NULL'),call. = F)



  smpCl<-names(fn_rstStack)

  cl<-parallel::makeCluster(cl)
  on.exit(parallel::stopCluster(cl))

  parallel::clusterExport(cl,
                          varlist = c(
                            'fn_rstStack',
                            'fn_derivedRaster',
                            'fn_raster',
                            'fn_label',
                            'fn_layerLabel',
                            'fn_analysisFolder',
                            'fn_classifiers',
                            'fn_prefix'),
                          envir = environment())

  classOut<-pbapply::pblapply(setNames(smpCl,smpCl),function(smp){

    if (!is.null(fn_derivedRaster)){

      superRaster<-raster::stack(fn_raster[[smp]],fn_derivedRaster[[smp]])

    } else {

      superRaster<-fn_raster[[smp]]
    }

    labelOut<-lapply(setNames(fn_label,paste0(fn_prefix,fn_label)),function(lbl){

      lvls<-raster::levels(fn_rstStack[[smp]][[fn_layerLabel]])[[1]]
      rowLabels<-which(grepl(lbl,lvls$label))
      if (length(rowLabels)==0) {stop(mError('unrecognised label'))} else{
        if (length(rowLabels)>1) {message(mWarning(paste0(paste0(lvls$label[rowLabels],collapse = ', '), 'will be collapsed to ',lbl)))}
      }
      IDlabels<-lvls$ID[rowLabels]
      labelMask<-lapply(IDlabels,function(x){
        fn_rstStack[[smp]][[fn_layerLabel]]==x
      })
      labelMask<-raster::stack(labelMask)

      TempFile_classMask<-file.path(fn_analysisFolder,
                                    'Temp',
                                    paste0('TEMP_CLASS_MASK_',smp,'_',lbl,'.nc'))

      labelMask<-raster::calc(labelMask,
                              sum,
                              filename = TempFile_classMask,
                              overwrite=T)

      filePath<-raster::filename(fn_rstStack[[smp]][[fn_layerLabel]])
      filePath<-DescTools::SplitPath(filePath)
      fileObjective<-file.path(filePath$drive,filePath$dirname,paste0(fn_prefix,lbl,'.',filePath$extension))
      filePathTEMP<-file.path(fn_analysisFolder,'Temp')

      if (is.null(fn_classifiers[[lbl]])){

        outRst<-labelMask
        outRst[outRst==0]<-NA
        names(outRst)<-paste0(fn_prefix,lbl)
        outRst<-raster::writeRaster(outRst,
                                    filename = fileObjective,
                                    overwrite=T,
                                    format='raster')
      } else {

        TempFile_maskedRaster<-file.path(fn_analysisFolder,
                                         'Temp',
                                         paste0('TEMP_PREDICT_CLASS_MASK_',smp,'_',lbl,'.nc'))

        maskedRaster<-raster::mask(x=superRaster,
                                   mask = labelMask,
                                   maskvalue=1,
                                   inverse=T,
                                   filename=TempFile_maskedRaster,
                                   overwrite=T)

        names(maskedRaster)<-names(superRaster)

        outRst<-raster::predict(maskedRaster,
                                fn_classifiers[[lbl]],
                                na.rm=T,
                                filename = file.path(filePathTEMP,paste0('TEMP_',smp,'_',fn_prefix,lbl,'.',filePath$extension)),
                                overwrite=T,
                                format='raster')

        names(outRst)<-paste0(fn_prefix,lbl)

        outRst<-raster::writeRaster(x =outRst,
                                    filename = fileObjective,
                                    overwrite=T,
                                    format='raster')

        unlink(file.path(filePathTEMP,paste0('TEMP_',smp,'_',fn_prefix,lbl,'.grd')))
        unlink(file.path(filePathTEMP,paste0('TEMP_',smp,'_',fn_prefix,lbl,'.gri')))

        unlink(TempFile_classMask)
        unlink(TempFile_maskedRaster)
      }

      return(outRst)
    })

    labelOut<-IMC_stack(x = labelOut,
                        uid = fn_raster[[smp]]@uid,
                        IMC_text_file = fn_raster[[smp]]@IMC_text_file,
                        study = fn_raster[[smp]]@study,
                        sample = fn_raster[[smp]]@sample,
                        replicate = fn_raster[[smp]]@replicate,
                        ROI = fn_raster[[smp]]@ROI,
                        bioGroup = fn_raster[[smp]]@bioGroup,
                        channels = fn_raster[[smp]]@channels,
                        type = 'class')

    return(labelOut)
  },cl = cl)

  classOut<-new('IMC_Classification',classOut)

  return(classOut)
}
