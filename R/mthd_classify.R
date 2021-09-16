#' Classify
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
setGeneric("classify", function(x,method=NULL,update=T,...)
  standardGeneric("classify"))


setMethod('classify',signature = ('IMC_Study'),
          function(x,method=NULL,update=T,...){

            if (is.null(x$currentAnalysis$classifier)) stop(mError('coud not find a classification model to apply'))
            if (is.null(method)) stop(mError('specify what method to use'))

            switch(method,
                   randomForest = {

                     pFtr<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$predictiveFeatures
                     rfCls<-x$currentAnalysis$classifier[[method]]
                     uids<-x$studyTable$uid

                     TEST_monkey<-sapply(uids,function(uid){
                       rst<-list(x$raster[[uid]],x$currentAnalysis$derivedRasters[[uid]])
                       rstrStk<-IMC_stack(x = rst,
                                          uid = x$raster[[uid]]@uid,
                                          IMC_text_file = x$raster[[uid]]@IMC_text_file,
                                          study = x$raster[[uid]]@study,
                                          sample = x$raster[[uid]]@sample,
                                          replicate = x$raster[[uid]]@replicate,
                                          ROI = x$raster[[uid]]@ROI,
                                          bioGroup = x$raster[[uid]]@bioGroup,
                                          channels = x$raster[[uid]]@channels,
                                          type = 'ANY')


                       fn_filePath<-file.path(x$currentAnalysis$folder,
                                              'test/classification')
                       checkDir(fn_filePath,'rasters')
                       checkDir(fn_filePath,'rasterStacks')

                       mf<-monkeyForest(fn_rst =rstrStk,
                                        fn_layers = pFtr,
                                        fn_newLayerName = 'label',
                                        fn_undeterminedLabel='undetermined',
                                        fn_Ptreshold=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$PvalueTreshold,
                                        fn_forest =rfCls,
                                        fn_filePath = fn_filePath)},USE.NAMES = T)

                     if (is.null(x$currentAnalysis$classification)){
                       newClassification<-TEST_monkey

                     } else {
                       oldClassification<-x$currentAnalysis$classification
                       newClassification<-lapply(setNames(uids,uids),function(uid){
                         mergeIMC_stacks(oldClassification[[uid]],
                                         TEST_monkey[[uid]],
                                         update = update)})
                     }
                     newClassification<-new('IMC_Classification',newClassification)

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     attr(newClassification,'crtnTimeStmp')<-attr(x$currentAnalysis$classification,'crtnTimeStmp')
                     attr(newClassification,'mdtnTimeStmp')<-newTimeStmp
                     attr(newClassification,'artnTimeStmp')<-newTimeStmp
                     attr(newClassification,'fileArchive')<-file.path(x$currentAnalysis$folder,'test','classification','rasterStacks')

                     x$currentAnalysis$classification<-newClassification

                   },
                   randomOnions={

                     if (is.null(x$currentAnalysis$classification)) stop(mError('coud not find any classification'))


                     newClassification<-randomOnions(fn_rstStack=x$currentAnalysis$classification,
                                                     fn_layerLabel=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$classificationLyr,
                                                     fn_label=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$labels,
                                                     fn_prefix=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$prefix,
                                                     fn_raster=x$raster,
                                                     fn_derivedRaster=x$currentAnalysis$derivedRasters,
                                                     fn_classifiers = x$currentAnalysis$classifier[[method]])

                     newClassification<-new('IMC_Classification',newClassification)

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     attr(newClassification,'crtnTimeStmp')<-attr(x$currentAnalysis$classification,'crtnTimeStmp')
                     attr(newClassification,'mdtnTimeStmp')<-newTimeStmp
                     attr(newClassification,'artnTimeStmp')<-newTimeStmp
                     attr(newClassification,'fileArchive')<-file.path(x$currentAnalysis$folder,'test','classification','rasterStacks')


                     x$currentAnalysis$classification<-newClassification

                   },
                   stop(mError('unknown method'),call. = F))



            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

            # attr(x$currentAnalysis$classification,'artnTimeStmp')<-newTimeStmp
            # attr(x$currentAnalysis$classification,'fileArchive')<-file.path(x$currentAnalysis$folder,'test/classification/rasterStacks')

          })
