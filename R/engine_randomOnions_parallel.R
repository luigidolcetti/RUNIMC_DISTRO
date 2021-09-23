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
  smpRs<-names(fn_raster)

  cl<-parallel::makeCluster(cl)
  parallel::clusterExport(cl,
                          varlist = c(
                            'fn_rstStack',
                            'fn_derivedRaster',
                            'fn_raster',
                            'fn_label',
                            'fn_layerLabel',
                            'fn_analysisFolder'
                          ),
                          envir = environment())

  superRaster<-parallel::parLapply(setNames(smpCl,smpCl),function(smp){
    if (!is.null(fn_derivedRaster)){
      superRaster<-raster::stack(fn_raster[[smp]],fn_derivedRaster[[smp]])
    } else {
      superRaster<-fn_raster[[smp]]
    }
    return(superRaster)
  },cl=cl)

  classMask<-parallel::parLapply(setNames(smpCl,smpCl),function(smp){
    out<-lapply(setNames(fn_label,fn_label),function(lbl){

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
      labelMask<-raster::calc(labelMask,
                              sum,
                              filename = file.path(fn_analysisFolder,
                                                   'Temp',
                                                   paste0('TEMP_CLASS_MASK_',smp,'_',lbl,'.nc')),
                              overwrite=T)

      return(labelMask)
    })
    return(out)
  },cl=cl)

  ##############################################

  parallel::clusterExport(cl,
                          c('superRaster',
                            'classMask',
                            'fn_classifiers',
                            'fn_prefix'),
                          envir = environment())

  classOut<-parallel::parLapply(setNames(smpCl,smpCl),function(smp){

    labelOut<-lapply(setNames(fn_label,fn_label),function(lbl){

      if (is.null(fn_classifiers[[lbl]])){
        outRst<-classMask[[smp]][[lbl]]
        outRst[outRst==0]<-NA
        names(outRst)<-paste0(fn_prefix,lbl)
        outRst<-raster::writeRaster(outRst,
                                    filename = fileObjective,
                                    overwrite=T,
                                    format='raster')
      } else {
        maskedRaster<-raster::mask(x=superRaster[[smp]],
                                   mask = classMask[[smp]][[lbl]],
                                   maskvalue=1,
                                   inverse=T,
                                   filename=file.path(fn_analysisFolder,
                                                      'Temp',
                                                      paste0('TEMP_PREDICT_CLASS_MASK_',smp,'_',lbl,'.nc')),
                                   overwrite=T)

        names(maskedRaster)<-names(superRaster[[smp]])

        filePath<-raster::filename(fn_rstStack[[smp]][[fn_layerLabel]])
        filePath<-DescTools::SplitPath(filePath)
        fileObjective<-file.path(filePath$drive,filePath$dirname,paste0(fn_prefix,lbl,'.',filePath$extension))

        outRst<-raster::predict(maskedRaster,
                                fn_classifiers[[lbl]],
                                na.rm=T,
                                filename = fileObjective,
                                overwrite=T,
                                format='raster')

        names(outRst)<-paste0(fn_prefix,lbl)
        unlink(raster::filename(maskedRaster))
      }

      return(outRst)
    })

    names(labelOut)<-unlist(lapply(labelOut,names))

    for (ii in names(labelOut)){
      fn_rstStack[[smp]][[ii]]<-labelOut[[ii]]

    }

    fpt<-raster::filename(fn_rstStack[[smp]])

    outStk<-IMCstackSave(fn_rstStack[[smp]],fpt)

    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

    attr(outStk,'mdtnTimeStmp')<-newTimeStmp
    attr(outStk,'artnTimeStmp')<-newTimeStmp
    attr(outStk,'fileArchive')<-fpt

    return(outStk)
  },cl = cl)

  parallel::stopCluster(cl)

  lapply(setNames(smpCl,smpCl),function(smp){
    out<-lapply(setNames(fn_label,fn_label),function(lbl){
      unlink(raster::filename(classMask[[smp]][[lbl]]))
    })
  })

  return(classOut)
}
