#' @importClassesFrom raster RasterStack
#'
IMC_RasterStack<-setClass('IMC_RasterStack',
                          contains = 'RasterStack',
                          slots = c(uid='character',
                                         IMC_text_file='character',
                                         study='character',
                                         sample='character',
                                         replicate='character',
                                         ROI='character',
                                         bioGroup='character',
                                         channels = 'IMC_ChannelTable'))

setMethod('initialize','IMC_RasterStack',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@uid<-''
            Object@IMC_text_file<-''
            Object@study<-''
            Object@sample<-''
            Object@replicate<-''
            Object@ROI<-''
            Object@bioGroup<-''
            Object@channels<-new('IMC_ChannelTable')
            Object<-initObjectAttr(Object)
            return(Object)})
