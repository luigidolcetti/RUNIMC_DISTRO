IMC_Segmentation<-setClass('IMC_Segmentation',
                           representation(polygons='list',
                                          performance='data.frame',
                                          raster='RasterLayer'))

setMethod('initialize','IMC_Segmentation',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})
