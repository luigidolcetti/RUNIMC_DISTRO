#' @export
randomOnions<-function(fn_rstStack=NULL,
                       fn_layerLabel=NULL,
                       fn_raster = NULL,
                       fn_derivedRaster = NULL,
                       fn_label=NULL,
                       fn_classifiers=NULL,
                       fn_prefix='topoMap_',
                       fn_analysisFolder=NULL,
                       ...){

  if (is.null(fn_layerLabel)) stop(mError('no classification layer specified'))
  if (is.null(fn_label)) stop(mError('no classification label specified'))


  smpCl<-names(fn_rstStack)
  smpRs<-names(fn_raster)
  superRaster<-sapply(smpCl,function(smp){
    if (!is.null(fn_derivedRaster)){
      superRaster<-raster::stack(fn_raster[[smp]],fn_derivedRaster[[smp]])
    } else {
      superRaster<-fn_raster[[smp]]
    }
    return(superRaster)
  },USE.NAMES = T,simplify = F)

  classMask<-lapply(setNames(smpCl,smpCl),function(smp){
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
  })


  ##############################################
  classOut<-lapply(setNames(smpCl,smpCl),function(smp){

    labelOut<-lapply(setNames(fn_label,fn_label),function(lbl){

      cat(paste0('predict raster for :::',lbl,'::: in ',smp,'\n'))


      if (is.null(fn_classifiers[[lbl]])){
        message(mWarning('Impossible to produce map for ',lbl,' returning a flat map'))
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
        filePathTEMP<-file.path(fn_analysisFolder,'Temp')

        outRst<-raster::predict(maskedRaster,
                                fn_classifiers[[lbl]],
                                na.rm=T,
                                filename = file.path(filePathTEMP,paste0('TEMP_',fn_prefix,lbl,'.',filePath$extension)),
                                overwrite=T,
                                format='raster')

        names(outRst)<-paste0(fn_prefix,lbl)

        outRst<-raster::writeRaster(x =outRst,
                                    filename = fileObjective,
                                    overwrite=T,
                                    format='raster')

        unlink(file.path(filePathTEMP,paste0('TEMP_',fn_prefix,lbl,'.grd')))
        unlink(file.path(filePathTEMP,paste0('TEMP_',fn_prefix,lbl,'.gri')))

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
  })

  lapply(setNames(smpCl,smpCl),function(smp){
    out<-lapply(setNames(fn_label,fn_label),function(lbl){
      unlink(raster::filename(classMask[[smp]][[lbl]]))
    })
  })

  return(classOut)
}
