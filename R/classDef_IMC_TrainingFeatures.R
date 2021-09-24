# IMC_TrainingFeatures<-setClass('IMC_TrainingFeatures',
#                                contains = 'list')
#
# setMethod('initialize','IMC_TrainingFeatures',
#           function(.Object, ...) {
#             Object <- callNextMethod(.Object, ...)
#             Object<-initObjectAttr(Object)
#             return(Object)})


IMC_TrainingFeatures<-setClass('IMC_TrainingFeatures',
                               slots = c(value='data.frame',
                                         geometry = 'data.frame',
                                         labels = 'data.frame'))

setMethod('initialize','IMC_TrainingFeatures',
          function(.Object,value=NULL,geometry=NULL, labels=NULL, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@value <- value
            Object@geometry <- geometry
            Object@labels <- labels
            Object<-initObjectAttr(Object)
            return(Object)})

setMethod('show','IMC_TrainingFeatures',
          function(object){
            cat('Traning features:')
            cat(paste0('Column names:',paste(colnames(object@value),collapse = ', '),'\n'))
            cat(paste0('Annotation labels: ',paste0(object@labels$parLabel,collapse = ', '),'\n'))
            cat(paste0('Extraction labels: ',paste0(object@labels$label,collapse = ', '),'\n'))
            cat('Summary: \n')
            print(object@labels)
})
