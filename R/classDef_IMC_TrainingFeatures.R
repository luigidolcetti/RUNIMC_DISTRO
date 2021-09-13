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
