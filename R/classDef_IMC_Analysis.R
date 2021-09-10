#'
IMC_StudyTable<-setClass('IMC_Analysis',
                         contains = 'environment')

setMethod('initialize','IMC_Analysis',
          function(.Object,
                   name=NULL,
                   filters=NULL,
                   derivedRasters=NULL,
                   extractionDirectives = NULL,
                   trainingFeatures=NULL,
                   classificationDirectives=NULL,
                   classifier=NULL,
                   classification=NULL,
                   interpretationMatrix=NULL,
                   segmentationDirectives=NULL,
                   segmentation=NULL,
                   exprs=NULL,
                   ...) {
            Object <- callNextMethod(.Object, ...)

            Object$name<-name
            Object$filters<-filters
            Object$derivedRasters<-derivedRasters
            Object$extractionDirectives<-extractionDirectives
            Object$trainingFeatures <-trainingFeatures
            Object$classificationDirectives <-classificationDirectives
            Object$classifier <-classifier
            Object$classification <-classification
            Object$interpretationMatrix<-interpretationMatrix
            Object$segmentationDirectives <-segmentationDirectives
            Object$segmentation <-segmentation
            Object$exprs <-exprs

            Object<-initObjectAttr(Object)

            return(Object)})

setMethod('show','IMC_Analysis',
          function(object){
            stt<-object$name
            if (is.null(stt)){
              cat('Empty IMC analysis')
            } else {

              obj<-ls(envir = object)
              objTest<-unlist(lapply(obj,function(x)is.null(object[[x]])))
              objSet<-vector('character',length(objTest))
              objSet[objTest]<-'empty'
              objSet[!objTest]<-'set'
              out<-''
              for (i in 1:length(objSet)){
              out<-paste0(out,
                       obj[i],
                       ':\t\t',
                       objSet[i],
                       '\n')
              }
              att<-strgfObjectAttr(object)
              cat(paste0('IMC Analysis: ',stt,'\n'))
              cat(out)
              cat(att)
            }
          })
