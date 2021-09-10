#' Test segmentation
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
setGeneric("testSegment", function(x,labelLayer='label',label=NULL,uid=NULL,limits=NULL,...)
  standardGeneric("testSegment"))

setMethod('testSegment',signature = ('IMC_Study'),
          function(x,labelLayer='label',label=NULL,uid=NULL,limits=NULL,...){



            if (is.null(x$currentAnalysis$segmentationDirectives)) stop(mError('Before segmenting directives must be specified'))
            if (is.null(labelLayer)) stop(mError('specify the classification layer to segment'))
            if (is.null(label)) stop(mError('specify the classification label to segment'))
            if (is.null(uid)) stop(mError('specify sample(uid) to segment'))



            avlblUids<-names(x$currentAnalysis$classification)
            if (!any(uid %in% avlblUids)) {stop(paste0("couldn't find ",uid,". Available samples(uid) are: ",paste0(avlblUids,collapse='/n')))}

            avlblLabelLayers<-names(x$currentAnalysis$classification[[uid]])
            if (!any(labelLayer %in% avlblLabelLayers)) {stop(paste0("couldn't find ",labelLayer,". Available labelLayers are: ",paste0(avlblLabelLayers,collapse=', ')))}

            if (is.null(limits)){
              message(mWarning('limits not specified, the entire raster will be used'))
              limits<-raster::extent(x$currentAnalysis$classification[[uid]][[labelLayer]])}



            mthd<-x$currentAnalysis$segmentationDirectives@method
            mthdPrmtrs<-x$currentAnalysis$segmentationDirectives@methodParameters

            switch(mthd,

                   spiderMap = {

                     rstToSegment<-raster::crop(x=x$currentAnalysis$classification[[uid]][[labelLayer]],y=limits)

                     iMat<-x$currentAnalysis$interpretationMatrix


                     cat(paste(uid,label,'\n',sep=":::"))
                     interpretationMatrixInstance<-iMat[[label]]
                     group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==label]
                     group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==label]
                     groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)*mthdPrmtrs$areaExpansion
                     groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)*mthdPrmtrs$roundnessExpansion

                     timerStart<-Sys.time()

                     TEMP<-list(spiderMap(fn_srt = rstToSegment,
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

                     timerStop<-Sys.time()

                     polygonsList<-list()
                     polygonsList[[uid]][[label]]<-list()
                     polygonsList[[uid]][[label]]<-TEMP[[1]]

                     newMarker<-label
                     dumpMarkers<-interpretationMatrixInstance$label[interpretationMatrixInstance$level!=label]
                     rasterMask<-lapply(dumpMarkers, function(dmpM){
                       polygonsList[[uid]][[label]]@raster==dmpM
                     })
                     rasterMask<-raster::stack(rasterMask)
                     rasterMask<-raster::calc(rasterMask,sum)
                     rasterMask<-rasterMask==0
                     polygonsList[[uid]][[label]]@raster<-raster::mask(polygonsList[[uid]][[label]]@raster,rasterMask,maskvalue=0,updateValue=NA)
                   },

                   ratMap = {

                     rstToSegment<-raster::crop(x=x$currentAnalysis$classification[[uid]][[labelLayer]],y=limits)

                     mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                     mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,label),USE.NAMES = F,simplify = T))

                     cat(paste(uid,mrkr[mrkrIndex],'\n',sep=":::"))
                     group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                     group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                     groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)*mthdPrmtrs$areaExpansion
                     groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)*mthdPrmtrs$roundnessExpansion

                     timerStart<-Sys.time()

                     TEMP<-list(ratMap(fn_srt = rstToSegment,
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

                     timerStop<-Sys.time()


                     polygonsList<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-TEMP[[1]]

                     newMarker<-mrkr[mrkrIndex]

                   },

                   slothMap = {

                     rstToSegment<-raster::crop(x=x$currentAnalysis$classification[[uid]][[labelLayer]],y=limits)

                     mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                     mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,label),USE.NAMES = F,simplify = T))

                     cat(paste(uid,mrkr[mrkrIndex],'\n',sep=":::"))
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
                     timerStart<-Sys.time()

                     TEMP<-list(slothMap(fn_srt = rstToSegment,
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
                                         fn_returnKinetic = T,
                                         fn_returnRasters = T))

                     timerStop<-Sys.time()


                     polygonsList<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-TEMP[[1]]

                     newMarker<-mrkr[mrkrIndex]

                   },

                   alligatorMap = {

                     rstToSegment<-raster::crop(x=x$currentAnalysis$classification[[uid]][[labelLayer]],y=limits)

                     mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                     mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,label),USE.NAMES = F,simplify = T))

                     cat(paste(uid,mrkr[mrkrIndex],'\n',sep=":::"))
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
                     timerStart<-Sys.time()

                     TEMP<-list(alligatorMap(fn_srt = rstToSegment,
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
                                             fn_returnKinetic = T,
                                             fn_returnRasters = T))

                     timerStop<-Sys.time()


                     polygonsList<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-TEMP[[1]]

                     newMarker<-mrkr[mrkrIndex]

                   }
            )


            polygonsList<-new('IMC_SegmentationList',polygonsList)

            condensedPoligonList<-extractPolygons(polygonsList)
            condensedPoligonList<-extractMeanPixel(fn_polygons = condensedPoligonList,
                                                   fn_raster = x$raster)


            logicRaster<-polygonsList[[uid]][[newMarker]]@raster<0

            positiveCells<-raster::freq(logicRaster,
                                        digits=3,
                                        progress='text')
            totalCells<-sum(positiveCells[,'count'])
            labelCells<-sum(positiveCells[!is.na(positiveCells[,'value']),'count'])
            ratioTotal<-positiveCells[,'count']/totalCells
            ratioLabel<-positiveCells[,'count']/labelCells
            positiveCells<-cbind(positiveCells,ratioTotal,ratioLabel)
            colnames(positiveCells)<-c('label','count','POT','POL')

            out_PL<-as.data.frame(sf::st_drop_geometry(condensedPoligonList[,!(names(condensedPoligonList) %in% c('uid','polygon.id','label','color','GEOMETRY'))]))
            out_PL<-summary(out_PL)
            out_GM<-sf::st_geometry(condensedPoligonList)
            out_STK<-raster::stack(list(original=rstToSegment,
                                        segmented=polygonsList[[uid]][[newMarker]]@raster,
                                        logical=logicRaster,
                                        data=raster::crop(x=x$raster[[uid]],y=limits)))

            out_timer<-data.frame(start=timerStart,
                                  stop=timerStop,
                                  duration=(timerStop-timerStart),
                                  expected=(x$currentAnalysis$classification[[uid]][[labelLayer]]@ncols*
                                              x$currentAnalysis$classification[[uid]][[labelLayer]]@nrows)/
                                    (rstToSegment@ncols*rstToSegment@nrows)*(timerStop-timerStart))

            out<-list(timer =out_timer,
                      summary = out_PL,
                      coverage = positiveCells,
                      geometry = out_GM,
                      raster =out_STK)



            return(out)
          })
