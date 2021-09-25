#' Filters list
#'
#' [addFilter()] either initialize or add a new filter to an object of class
#'   IMC_FilterFrame. Filters are then processed via the command [deployFilters()].
#'
#' @param x IMC_Study, a study
#' @param filter character, type of filter can be 'vanvliet', 'deriche', 'blur_anisotropic', from the package imageR.
#' @param parameters list, list of named values to be passed to the specific function.
#' @param channels character vector, names of the layers to be processed.
#' @param append logical, FALSE delete the previously added filters, TRUE append the specified filters to the stack of filters already defined.
#'
#' @return an object of class IMC_filterFrame
#'
#' @examples
#' \dontrun{
#' availableLayers<-ch_Rnames(MyStudy)
#' addFilter (x = MyStudy,
#'            filter = 'vanvliet',
#'            parameters = list(list(sigma=1,order=0,axis='x'), list(sigma=1,order=0,axis='y')),
#'            channels = availableLayers,
#'            append = F)
#' addFilter (x = MyStudy,
#'            filter = 'blur_anisotropic',
#'            parameters = list(list(amplitude=9),list(amplitude=3)),
#'            channels = availableLayers,
#'            append=T)
#' deployFilters (MyStudy, saveToDisk = T)
#' }
#'
#' @export
setGeneric("addFilter", function(x,filter=NULL,parameters=NULL,channels=NULL,append=T,...)
  standardGeneric("addFilter"))

setMethod('addFilter',signature = ('IMC_Study'),
          function(x,filter=NULL,parameters=NULL,channels=NULL,append=T,...){

            if (is.null(filter)) stop(mError('provide Filter'),call. = F)
            if (is.null(parameters)) stop(mError(paste0('provide parameters for ',filter,' filter')),call. = F)
            if (is.null(channels)) stop(mError(paste0('provide channels to elaborate with ',filter,' filter')),call. = F)
            if (is.null(x$currentAnalysis)) stop(mError('initialize an analysis with newAnalysis()'),call. = F)
            if (append) x$currentAnalysis$filters<-x$currentAnalysis$filters else x$currentAnalysis$filters<-NULL
            if (is.null(x$currentAnalysis$filters)) {
              newFilter<-new('IMC_FilterFrame')
              x$currentAnalysis$filters<-newFilter
            }

            newFilter<-data.frame(filter=filter,
                                  parameters=I(list(parameters)),
                                  channels=I(list(channels)))


            oldFilters<-x$currentAnalysis$filters
            newFilter<-rbind.data.frame(oldFilters,newFilter)
            newFilter<-new('IMC_FilterFrame',newFilter)

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
            x$currentAnalysis$filters<-newFilter

          })

#' Apply filters
#'
#' [deployFilters()] utilizes the filter definitions stored in the IMC_FilterFrame
#'   object, created via the function [addFilter()].
#'   This function is a wrapper for the internal [imageRFilter()].
#'
#' @param x IMC_Study, a study.
#' @param cl cluster object created with [parallel] or [snow]
#'
#' @return a new IMC_RsCollection that will be stored in the object **derivedRasters**.
#'
#' @examples
#' \dontrun{
#' availableLayers<-ch_Rnames(MyStudy)
#' addFilter (x = MyStudy,
#'            filter = 'vanvliet',
#'            parameters = list(list(sigma=1,order=0,axis='x'), list(sigma=1,order=0,axis='y')),
#'            channels = availableLayers,
#'            append = F)
#' addFilter (x = MyStudy,
#'            filter = 'blur_anisotropic',
#'            parameters = list(list(amplitude=9),list(amplitude=3)),
#'            channels = availableLayers,
#'            append=T)
#' deployFilters (MyStudy, saveToDisk = T)
#' }
#'
#' @export
setGeneric("deployFilters", function(x,cl=NA, ...)
  standardGeneric("deployFilters"))

setMethod('deployFilters',signature = ('IMC_Study'),
          function(x,cl=NA,...){


            oldRstk<-list.files(file.path(x$currentAnalysis$folder,'rasterStacks'),full.names = T,recursive = F)
            unlink(oldRstk,recursive = T)
            oldRst<-list.dirs(file.path(x$currentAnalysis$folder,'rasters'),full.names = T,recursive = F)
            unlink(oldRst,recursive = T)
            message(mMessage('Clean up raster and rasterStack folders'))


            ff<-x$currentAnalysis$filters
            if (is.na(cl)){
              derivedRasters<-apply(X = ff,
                                    MARGIN = 1,
                                    FUN = function(fltDf){
                                      imageRFilter(fn_rasterStack = x$raster,
                                                   fn_filter = fltDf$filter,
                                                   fn_filterParameterList = fltDf$parameters,
                                                   fn_markerList = fltDf$channels,
                                                   fn_pathToFile = x$currentAnalysis$folder)
                                    })
            } else {

              cl<-parallel::makeCluster(cl)

              parallel::clusterExport(cl=cl,
                                      varlist = c('raster'),
                                      envir=x)

              derivedRasters<-pbapply::pbapply(X = ff,
                                                 MARGIN = 1,
                                                 FUN = function(fltDf){
                                                   imageRFilter(fn_rasterStack = raster,
                                                                fn_filter = fltDf$filter,
                                                                fn_filterParameterList = fltDf$parameters,
                                                                fn_markerList = fltDf$channels,
                                                                fn_pathToFile = x$currentAnalysis$folder)
                                                 },
                                                 cl = cl)
              on.exit(parallel::stopCluster(cl=cl))
            }



            rstNms<-Reduce('=',lapply(derivedRasters,names))
            derivedRasters<-unlist(derivedRasters,recursive=F)
            derivedRasters<-lapply(setNames(rstNms,rstNms),function(x){
              rasterStackList<-derivedRasters[rstNms==x]


              uid<-Reduce('=',lapply(rasterStackList,slot,'uid'))
              IMC_text_file<-Reduce('=',lapply(rasterStackList,slot,'IMC_text_file'))
              study<-Reduce('=',lapply(rasterStackList,slot,'study'))
              sample<-Reduce('=',lapply(rasterStackList,slot,'sample'))
              replicate<-Reduce('=',lapply(rasterStackList,slot,'replicate'))
              ROI<-Reduce('=',lapply(rasterStackList,slot,'ROI'))
              bioGroup<-Reduce('=',lapply(rasterStackList,slot,'bioGroup'))
              channels<-Reduce('=',lapply(rasterStackList,slot,'channels'))
              out<-IMC_stack(rasterStackList,
                             uid,
                             IMC_text_file,
                             study,
                             sample,
                             replicate,
                             ROI,
                             bioGroup,
                             channels,
                             type = 'calc')

            })

            newNames<-names(derivedRasters)

            derivedRasters<-lapply(setNames(newNames,newNames),function(nms){

              fpt<-file.path(x$currentAnalysis$folder,
                             'rasterStacks',
                             paste0(derivedRasters[[nms]]@IMC_text_file,'.stk'))

              out<-IMCstackSave(derivedRasters[[nms]],fpt)

              newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

              attr(out,'mdtnTimeStmp')<-newTimeStmp
              attr(out,'artnTimeStmp')<-newTimeStmp
              attr(out,'fileArchive')<-fpt

              return(out)

            })


            derivedRasters<-new('IMC_RsCollection',derivedRasters)

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

            x$currentAnalysis$derivedRasters<-derivedRasters

            attr(x$currentAnalysis$derivedRasters,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis$derivedRasters,'artnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis$derivedRasters,'fileArchive')<-file.path(x$currentAnalysis$folder,'rasterStacks')


          })

