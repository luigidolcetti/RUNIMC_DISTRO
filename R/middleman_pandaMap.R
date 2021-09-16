mdm_pandaMap<-function(x,labelLayer,mthd,mthdPrmtrs){

  rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
    if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(mError('Check classification layer name provided'))
    return(x$currentAnalysis$classification[[nms]][[labelLayer]])
  },USE.NAMES = T,simplify = F)

  polygonsList<-sapply(names(rstToSegment),function(x){
    sapply(labelLayer,function(x){},
           simplify = F,USE.NAMES = T)},
    simplify = F,USE.NAMES = T)

  for (rst in names(rstToSegment)){
    for (i in labelLayer){

      mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
      mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,i),USE.NAMES = F,simplify = T))


      cat(paste(rst,mrkr[mrkrIndex],'\n',sep=":::"))

      polygonsList[[rst]][[i]]<-pandaMap(fn_srt = rstToSegment[[rst]][[i]],
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
                                         fn_verbose = mthdPrmtrs$verbose)

    }
  }


  polygonsList<-lapply(polygonsList,function(xuid){
    do.call(rbind.data.frame,append(xuid,
                                    list(make.row.names = F,
                                         stringsAsFactors = F,
                                         deparse.level=0)
    )
    )
  })

  polygonsList<-do.call(rbind.data.frame,append(polygonsList,
                                                list(make.row.names = F,
                                                     stringsAsFactors = F,
                                                     deparse.level=0)))

  newIDs<-ids::random_id(n = nrow(polygonsList),
                         bytes = 16,
                         use_openssl = TRUE)

  oldNames<-colnames(polygonsList)
  polygonsList<-dplyr::bind_cols(polygonsList,pIDs=newIDs)[,c('pIDs',oldNames)]

  return(polygonsList)

}
