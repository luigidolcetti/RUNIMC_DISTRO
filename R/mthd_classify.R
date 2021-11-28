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
setGeneric("classify", function(x,method=NULL,update=T,cl=NA,...)
  standardGeneric("classify"))


setMethod('classify',signature = ('IMC_Study'),
          function(x,method=NULL,update=T,cl=NA,...){

            if (is.null(x$currentAnalysis$classifier)) stop(mError('coud not find a classification model to apply'))
            if (is.null(method)) stop(mError('specify what method to use'))

            uids<-x$studyTable$uid
            tempPath<-file.path(x$currentAnalysis$folder,'Temp')
            fn_filePath<-file.path(x$currentAnalysis$folder,
                                   'test/classification')
            checkDir(fn_filePath,'rasters',verbose=F)
            checkDir(fn_filePath,'rasterStacks',verbose=F)

            switch(method,
                   randomForest = {

                     pFtr<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$predictiveFeatures
                     pval<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$PvalueTreshold
                     rfCls<-x$currentAnalysis$classifier[[method]]

                     if (!is.na(cl)){
                       cl<-parallel::makeCluster(cl)
                       on.exit(parallel::stopCluster(cl = cl))

                       parallel::clusterExport(cl = cl,
                                               varlist = c(
                                                 'pFtr',
                                                 'rfCls',
                                                 'uids',
                                                 'tempPath',
                                                 'pval'),
                                               envir = environment())
                       parallel::clusterExport(cl=cl,
                                               varlist = c(
                                                 'raster'),
                                               envir = x)
                       parallel::clusterExport(cl=cl,
                                               varlist = c(
                                                 'derivedRasters',
                                                 'folder'),
                                               envir = x$currentAnalysis)


                       newClassification<-pbapply::pblapply(setNames(uids,uids),function(uid){

                         if (is.null(derivedRasters)) {

                           rstrStk<-raster[[uid]]

                         } else {

                           rstrStk<-mergeIMC_stacks(raster[[uid]],derivedRasters[[uid]])

                         }

                         mf<-monkeyForest(fn_rst =rstrStk,
                                          fn_layers = pFtr,
                                          fn_newLayerName = 'label',
                                          fn_undeterminedLabel='undetermined',
                                          fn_Ptreshold=pval,
                                          fn_forest =rfCls,
                                          fn_filePath = fn_filePath,
                                          fn_uid = uid,
                                          fn_TempPath = tempPath)

                         return(mf)

                       },cl = cl)


                     } else {

                       newClassification<-lapply(setNames(uids,uids),function(uid){

                         if (is.null(x$currentAnalysis$derivedRasters[[uid]])) {

                           rstrStk<-x$raster[[uid]]

                         } else {

                           rstrStk<-mergeIMC_stacks(x$raster[[uid]],
                                                    x$currentAnalysis$derivedRasters[[uid]])

                         }

                         mf<-monkeyForest(fn_rst =rstrStk,
                                          fn_layers = pFtr,
                                          fn_newLayerName = 'label',
                                          fn_undeterminedLabel='undetermined',
                                          fn_Ptreshold=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$PvalueTreshold,
                                          fn_forest =rfCls,
                                          fn_filePath = fn_filePath,
                                          fn_uid = uid,
                                          fn_TempPath = tempPath)
                         return(mf)
                       })

                     }

                   },
                   randomOnions={

                     if (is.null(x$currentAnalysis$classification)) stop(mError('coud not find any classification'))

                     if (!is.na(cl)){

                       newClassification<-randomOnions_parallel(fn_rstStack=x$currentAnalysis$classification,
                                                                fn_layerLabel=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$classificationLyr,
                                                                fn_label=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$labels,
                                                                fn_prefix=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$prefix,
                                                                fn_raster=x$raster,
                                                                fn_derivedRaster=x$currentAnalysis$derivedRasters,
                                                                fn_classifiers = x$currentAnalysis$classifier[[method]],
                                                                fn_analysisFolder = x$currentAnalysis$folder,
                                                                cl=cl)

                     } else {

                       newClassification<-randomOnions(fn_rstStack=x$currentAnalysis$classification,
                                                       fn_layerLabel=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$classificationLyr,
                                                       fn_label=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$labels,
                                                       fn_prefix=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$prefix,
                                                       fn_raster=x$raster,
                                                       fn_derivedRaster=x$currentAnalysis$derivedRasters,
                                                       fn_classifiers = x$currentAnalysis$classifier[[method]],
                                                       fn_analysisFolder = x$currentAnalysis$folder)

                     }
                   },
                   stop(mError('unknown method'),call. = F)
            )


            ##### update study ####

            if (!is.null(x$currentAnalysis$classification)){

              oldClassification<-x$currentAnalysis$classification
              newClassification<-lapply(setNames(uids,uids),function(uid){
                mergeIMC_stacks(oldClassification[[uid]],
                                newClassification[[uid]],
                                update = update)})

            }

            newClassification<-lapply(newClassification,function(ncl){
              fpt <- file.path(x$currentAnalysis$folder,
                               'test',
                               'classification',
                               'rasterStacks',
                               paste0(ncl@IMC_text_file,'.stk'))

              out<-IMCstackSave(ncl,fpt)
              newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
              attr(out,'mdtnTimeStmp')<-newTimeStmp
              attr(out,'artnTimeStmp')<-newTimeStmp
              attr(out,'fileArchive')<-fpt
              return(out)
            })

            newClassification<-new('IMC_Classification',newClassification)

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

            attr(newClassification,'mdtnTimeStmp')<-newTimeStmp
            attr(newClassification,'artnTimeStmp')<-newTimeStmp
            attr(newClassification,'fileArchive')<-file.path(x$currentAnalysis$folder,'test','classification','rasterStacks')

            x$currentAnalysis$classification<-newClassification

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

          })
