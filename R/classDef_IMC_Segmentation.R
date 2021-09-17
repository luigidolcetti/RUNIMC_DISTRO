IMC_Segmentation<-setClass('IMC_Segmentation',
                             slots = c (polygons='list',
                                          performance='data.frame',
                                          raster='RasterLayer'))

setMethod('initialize','IMC_Segmentation',
          function(.Object,
                   polygons = list(),
                   performance=data.frame(),
                   raster=raster::raster(),
                   ...) {
            Object <- callNextMethod(.Object, ...)
            Object@polygons<-polygons
            Object@performance<-performance
            Object@raster<-raster
            Object<-initObjectAttr(Object)
            return(Object)})
