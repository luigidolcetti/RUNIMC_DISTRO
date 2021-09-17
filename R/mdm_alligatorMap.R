mdm_alligatorMap<-function(x,labelLayer,mthd,mthdPrmtrs){

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

      group_area<-x$currentAnalysis$trainingFeatures@geometry$area[x$currentAnalysis$trainingFeatures@geometry$label==mrkr[mrkrIndex]]
      group_roundness<-x$currentAnalysis$trainingFeatures@geometry$roundness[x$currentAnalysis$trainingFeatures@geometry$label==mrkr[mrkrIndex]]
      groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)
      groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)

      if (is.character(mthdPrmtrs$targetArea)){

        switch(mthdPrmtrs$targetArea,
               training_mean = {
                 targetArea<-mean(unlist(group_area))
               },
               training_median = {
                 targetArea<-median(unlist(group_area))
               },
               training_mode = {
                 brks<-0:ceiling(unlist(group_area))
                 frqT<-hist(x = unlist(group_area),breaks = brks)
                 targetArea<-median(frqT$breaks[which(frqT$counts==max(frqT$counts))])
               },
               training_max = {
                 targetArea<-max(unlist(group_area))
               },
               training_middle = {
                 targetArea<-(max(unlist(group_area))-min(unlist(group_area)))/2
               },
               {targetArea<-mthdPrmtrs$targetArea})
      } else {targetArea<-mthdPrmtrs$targetArea}


      out<-alligatorMap(fn_srt = rstToSegment[[rst]][[i]],
                              fn_Nspikes=mthdPrmtrs$spikes,
                              fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                              fn_coverage = mthdPrmtrs$coverage,
                              fn_minArea = groupAreaRange[[1]],
                              fn_maxArea = groupAreaRange[[2]],
                              fn_minRoundness = groupRoundnessRange[[1]],
                              fn_maxRoundness = groupRoundnessRange[[2]],
                              fn_seedOutScore = mthdPrmtrs$seedOutScore,
                              fn_cycleWindow = mthdPrmtrs$cycleWindow,
                              fn_adaptative = mthdPrmtrs$adaptative,
                              fn_areaAdaptRate = mthdPrmtrs$areaAdaptRate,
                              fn_roundnessAdaptRate = mthdPrmtrs$roundnessAdaptRate,
                              fn_segmentAlg = mthdPrmtrs$segmentAlg,
                              fn_fusion = mthdPrmtrs$fusion,
                              fn_maxNetworkSize = mthdPrmtrs$maxNetworkSize,
                              fn_targetArea = targetArea,
                              fn_inflateDeflate = mthdPrmtrs$inflateDeflate,
                              fn_favourForeing = mthdPrmtrs$favourForeing,
                              fn_returnKinetic = mthdPrmtrs$returnKinetic,
                              fn_returnRasters = mthdPrmtrs$returnRasters)

      return(out)

    })
    return(out)
  })

  polygonsList<-new('IMC_SegmentationList',polygonsList)

  return(polygonsList)
}
