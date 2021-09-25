#' IMC_ClassificationDirectives
#'
#' Object containing the classification parameters to be applied. Each
#' classification method needs to be implemented as an engine function, with
#' a list of default parameter
#' and have a proper section of the method segment() able to process the
#' method parametes and call the appropriate engine.
#' @slot method character, method name, needs to mach the call in the
#'   witch statement in the segment method.
#' @slot methodParameters list, method parameters as specified for the
#'   method
IMC_ClassificationDirectives<-setClass('IMC_ClassificationDirectives',
                                       slots = c(method='character',
                                                 methodParameters='list'))

setMethod('initialize','IMC_ClassificationDirectives',
          function(.Object, method=c(),methodParameters=list(),...) {
            Object <- callNextMethod(.Object, ...)
            Object@method<-method
            Object@methodParameters<-methodParameters
            Object<-initObjectAttr(Object)
            return(Object)})
