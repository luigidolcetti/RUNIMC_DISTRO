#' Segment
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
setGeneric("segment", function(x,labelLayer='label',...)
  standardGeneric("segment"))

setMethod('segment',signature = ('IMC_Study'),
          function(x,labelLayer='label',...){

            if (is.null(x$currentAnalysis$segmentationDirectives)) stop(mError('Before segmenting directives must be specified'))

            mthd<-x$currentAnalysis$segmentationDirectives@method
            mthdPrmtrs<-x$currentAnalysis$segmentationDirectives@methodParameters

            switch(mthd,

                   spiderMap = {

                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)
                     iMat<-x$currentAnalysis$interpretationMatrix

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in names(iMat)){

                         cat(paste(rst,i,'\n',sep=":::"))
                         interpretationMatrixInstance<-iMat[[i]]
                         group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==i]
                         group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==i]
                         groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)*mthdPrmtrs$areaExpansion
                         groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)*mthdPrmtrs$roundnessExpansion

                         TEMP<-list(spiderMap(fn_srt = rstToSegment[[rst]],
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
                                              fn_seed = mthdPrmtrs$seed))
                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]


                       }
                     }
                   },

                   ratMap = {

                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in labelLayer){

                         mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                         mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,i),USE.NAMES = F,simplify = T))


                         cat(paste(rst,mrkr[mrkrIndex],'\n',sep=":::"))
                         group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)*mthdPrmtrs$areaExpansion
                         groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)*mthdPrmtrs$roundnessExpansion

                         TEMP<-list(ratMap(fn_srt = rstToSegment[[rst]][[i]],
                                           fn_Nspikes=mthdPrmtrs$spikes,
                                           fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                           fn_coverage = mthdPrmtrs$coverage,
                                           fn_minArea = groupAreaRange[[1]],
                                           fn_maxArea = groupAreaRange[[2]],
                                           fn_minRoundness = groupRoundnessRange[[1]],
                                           fn_maxRoundness = groupRoundnessRange[[2]],
                                           fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                           fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                           fn_discoverTreshold = mthdPrmtrs$discoverTreshold,
                                           fn_adaptative = mthdPrmtrs$adaptative,
                                           fn_drastic = groupAreaRange[[1]]*mthdPrmtrs$drastic,
                                           fn_lowPenalty = mthdPrmtrs$lowPenalty,
                                           fn_highPenalty = mthdPrmtrs$highPenalty,
                                           fn_roundnessPenalty = mthdPrmtrs$roundnessPenalty,
                                           fn_seed = mthdPrmtrs$seed))
                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]

                       }
                     }
                   },

                   slothMap = {


                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in labelLayer){

                         mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                         mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,i),USE.NAMES = F,simplify = T))


                         cat(paste(rst,mrkr[mrkrIndex],'\n',sep=":::"))
                         group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
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


                         TEMP<-list(slothMap(fn_srt = rstToSegment[[rst]][[i]],
                                             fn_Nspikes=mthdPrmtrs$spikes,
                                             fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                             fn_coverage = mthdPrmtrs$coverage,
                                             fn_minArea = groupAreaRange[[1]],
                                             fn_maxArea = groupAreaRange[[2]],
                                             fn_minRoundness = groupRoundnessRange[[1]],
                                             fn_maxRoundness = groupRoundnessRange[[2]],
                                             fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                             fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                             fn_discoverTreshold = mthdPrmtrs$discoverTreshold,
                                             fn_adaptative = mthdPrmtrs$adaptative,
                                             fn_areaAdaptRate = mthdPrmtrs$areaAdaptRate,
                                             fn_roundnessAdaptRate = mthdPrmtrs$roundnessAdaptRate,
                                             fn_fusion = mthdPrmtrs$fusion,
                                             fn_maxNetworkSize = mthdPrmtrs$maxNetworkSize,
                                             fn_targetArea = targetArea,
                                             fn_inflateDeflate = mthdPrmtrs$inflateDeflate,
                                             fn_favourForeing = mthdPrmtrs$favourForeing,
                                             fn_returnKinetic = mthdPrmtrs$returnKinetic,
                                             fn_returnRasters = mthdPrmtrs$returnRasters))

                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]

                       }
                     }
                   },

                   alligatorMap = {


                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in labelLayer){

                         mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                         mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,i),USE.NAMES = F,simplify = T))


                         cat(paste(rst,mrkr[mrkrIndex],'\n',sep=":::"))
                         group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
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


                         TEMP<-list(alligatorMap(fn_srt = rstToSegment[[rst]][[i]],
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
                                                 fn_returnRasters = mthdPrmtrs$returnRasters))

                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]

                       }
                     }
                   },

                   lazyCatMap = {

                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in labelLayer){

                         cat(paste(rst,i,'\n',sep=":::"))

                         polygonsList[[rst]][[i]]<-list(lazyCatMap(fn_srt = rstToSegment[[rst]][[i]],
                                                                   fn_uid = rst,
                                                                   fn_indexToExclude = mthdPrmtrs$indexToExclude))
                         # polygonsList[[rst]][[i]]<-list()
                         # polygonsList[[rst]][[i]]<-TEMP[[1]]

                       }
                     }

                     polygonsList<-lapply(polygonsList,function(xuid){
                       do.call(dplyr::bind_rows,xuid)})

                     polygonsList<-do.call(dplyr::bind_rows,polygonsList)

                     if (mthdPrmtrs$distillDirect){

                       condensedPoligonList<-extractMeanPixel(fn_polygons = polygonsList,
                                                              fn_raster = x$raster)
                     }

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                     condensedPoligonList<-initObjectAttr(condensedPoligonList)

                     x$currentAnalysis$exprs<-condensedPoligonList
                     attr(x,'mdtnTimeStmp')<-newTimeStmp
                     attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
                     return()


                   },

                   pandaMap = {

                     out<-mdm_pandaMap(x,labelLayer,mthd,mthdPrmtrs)

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     out<-initObjectAttr(out)

                     x$currentAnalysis$exprs<-ept_add_primary(x = x$currentAnalysis$exprs,
                                                              newTab = out,
                                                              name = 'pandaMap')
                     attr(x,'mdtnTimeStmp')<-newTimeStmp
                     attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
                     return()

                   }
            )


            polygonsList<-new('IMC_SegmentationList',polygonsList)
            x$currentAnalysis$segmentation<-polygonsList

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })
