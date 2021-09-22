#' @export
randomOnions_parallel<-function(fn_rstStack=NULL,
                                fn_layerLabel=NULL,
                                fn_raster = NULL,
                                fn_derivedRaster = NULL,
                                fn_label=NULL,
                                fn_classifiers=NULL,
                                fn_prefix='topoMap_',
                                cl = NA){

  if (is.null(fn_layerLabel)) stop(mError('no classification layer specified'))
  if (is.null(fn_label)) stop(mError('no classification label specified'))
  if (is.null(cl)) stop (mError('use the non-parallel randomOnions()'))


  smpCl<-names(fn_rstStack)
  smpRs<-names(fn_raster)

  cl<-parallel::makeCluster(cl)
  parallel::clusterExport(cl,
                          varlist = c(
                            'fn_rstStack',
                            'fn_derivedRaster',
                            'fn_raster',
                            'fn_label',
                            'fn_layerLabel'

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
      labelMask<-raster::calc(labelMask,sum)

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
      } else {
        maskedRaster<-raster::mask(x=superRaster[[smp]],
                                   mask = classMask[[smp]][[lbl]],
                                   maskvalue=1,
                                   inverse=T)
        outRst<-raster::predict(maskedRaster,
                                fn_classifiers[[lbl]],
                                na.rm=T,
                                progress='text')
      }

      names(outRst)<-paste0(fn_prefix,lbl)
      # if (!is.null(fn_filePath)){
      filePath<-raster::filename(fn_rstStack[[smp]][[fn_layerLabel]])
      filePath<-DescTools::SplitPath(filePath)
      fileObjective<-file.path(filePath$drive,filePath$dirname,paste0(fn_prefix,lbl,'.',filePath$extension))
      raster::writeRaster(x = outRst,
                          filename = fileObjective,
                          overwrite=T,
                          format='raster')

      outRst<-raster::raster(fileObjective)
      # }
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

  return(classOut)
}
