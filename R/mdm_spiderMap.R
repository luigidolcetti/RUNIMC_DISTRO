mdm_spiderMap<-function(x,labelLayer,mthd,mthdPrmtrs){

  rstNames<-names(x$currentAnalysis$classification)
  rstNames<-setNames(rstNames,rstNames)

  rstToSegment<-lapply(rstNames,function(nms){
    if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) {
      stop(mError('Check classification layer name provided'))}
    return(x$currentAnalysis$classification[[nms]][[labelLayer]])
  })

  iMat<-x$currentAnalysis$interpretationMatrix

  rstNames<-names(rstToSegment)
  rstNames<-setNames(rstNames,rstNames)

  imtNames<-names(iMat)
  imtNames<-setNames(imtNames,imtNames)

  polygonsList<-lapply(rstNames, function(rst){
    out<-lapply(imtNames,function(i){


      cat(paste(rst,i,'\n',sep=":::"))
      interpretationMatrixInstance<-iMat[[i]]
      group_area<-x$currentAnalysis$trainingFeatures@geometry$area[x$currentAnalysis$trainingFeatures@geometry$label==i]
      group_roundness<-x$currentAnalysis$trainingFeatures@geometry$roundness[x$currentAnalysis$trainingFeatures@geometry$label==i]
      groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)*mthdPrmtrs$areaExpansion
      groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)*mthdPrmtrs$roundnessExpansion

      out<-spiderMap(fn_srt = rstToSegment[[rst]],
                     fn_interpret = interpretationMatrixInstance,
                     fn_Nspikes=mthdPrmtrs$spikes,
                     fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                     fn_densityMultiplier=mthdPrmtrs$densityMultiplier,
                     fn_coverage = mthdPrmtrs$coverage,
                     fn_minArea = groupAreaRange[[1]],
                     fn_maxArea = groupAreaRange[[2]],
                     fn_minRoundness = groupRoundnessRange[[1]],
                     fn_maxRoundness = groupRoundnessRange[[2]],
                     fn_seedOutScore = mthdPrmtrs$seedOutScore,
                     fn_cutSeedList = mthdPrmtrs$cutSeedList,
                     fn_cycleWindow = mthdPrmtrs$cycleWindow,
                     fn_discoverTreshold = mthdPrmtrs$discoverTreshold,
                     fn_adaptative = mthdPrmtrs$adaptative,
                     fn_drastic = groupAreaRange[[1]]*mthdPrmtrs$drastic,
                     fn_direction = mthdPrmtrs$direction,
                     fn_seed = mthdPrmtrs$seed)


    })
  })

  polygonsList<-new('IMC_SegmentationList',polygonsList)

  return(polygonsList)
}
