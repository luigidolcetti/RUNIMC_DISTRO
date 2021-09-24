mdm_pandaMap<-function(x,labelLayer,mthd,mthdPrmtrs){


  rstNames<-names(x$currentAnalysis$classification)
  rstNames<-setNames(rstNames,rstNames)

  rstToSegment<-lapply(rstNames,function(nms){
    if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) {
      stop(mError('Check classification layer name provided'))}
    return(x$currentAnalysis$classification[[nms]][[labelLayer]])
  })

  rstNames<-names(rstToSegment)
  rstNames<-setNames(rstNames,rstNames)

  polygonsList<-lapply(rstNames, function(rst){
    out<-lapply(setNames(labelLayer,labelLayer),function(i){

      mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
      mrkr<-setNames(mrkr,mrkr)
      mrkrIndex<-which(vapply(mrkr,function(x)grepl(x,i),logical(1),USE.NAMES = T))

      cat(paste(rst,mrkr[mrkrIndex],'\n',sep=":::"))

      out<-pandaMap(fn_srt = rstToSegment[[rst]][[i]],
                    fn_uid = rst,
                    fn_primaryIndex=mrkr[mrkrIndex],
                    fn_clpDir = mthdPrmtrs$ClampDetectionDirection,
                    fn_brake = mthdPrmtrs$nOfCutBrakes,
                    fn_lowerQuantile = mthdPrmtrs$lowerQuantile,
                    fn_upperQuantile = mthdPrmtrs$upperQuantile,
                    fn_lowerAreaLimit= mthdPrmtrs$lowerAreaLimit,
                    fn_movingWindow_dim = mthdPrmtrs$movingWindowDimension,
                    fn_movingWindow_overlap = mthdPrmtrs$overlapExtent,
                    fn_cores = mthdPrmtrs$numberOfCores,
                    fn_verbose = mthdPrmtrs$verbose,
                    fn_TempFile = file.path(x$currentAnalysis$folder,'Temp'))

    })
    out<-do.call(rbind.data.frame,append(out,
                                         list(make.row.names = F,
                                              stringsAsFactors = F,
                                              deparse.level=0)))
  })

  polygonsList<-do.call(rbind.data.frame,append(polygonsList,
                                                list(make.row.names = F,
                                                     stringsAsFactors = F,
                                                     deparse.level=0)))

  newIDs<-apply(polygonsList,1,digest::digest)

  oldNames<-colnames(polygonsList)
  polygonsList<-dplyr::bind_cols(polygonsList,pIDs=newIDs)[,c('pIDs',oldNames)]

  return(polygonsList)

}
