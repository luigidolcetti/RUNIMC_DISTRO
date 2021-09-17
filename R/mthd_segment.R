#' Segment
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
setGeneric("segment", function(x,labelLayer='label',...)
  standardGeneric("segment"))

setMethod('segment',signature = ('IMC_Study'),
          function(x,labelLayer='label',...){

            if (is.null(x$currentAnalysis$segmentationDirectives)) stop(mError('Before segmenting directives must be specified'))

            mthd<-x$currentAnalysis$segmentationDirectives@method
            mthdPrmtrs<-x$currentAnalysis$segmentationDirectives@methodParameters

            switch(mthd,

                   spiderMap = {

                     out<-mdm_spiderMap(x,labelLayer,mthd,mthdPrmtrs)

                     x$currentAnalysis$segmentation<-out

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     attr(x,'mdtnTimeStmp')<-newTimeStmp
                     attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

                     out<-extractPolygons(out)

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     x$currentAnalysis$exprs<-ept_add_primary(x = x$currentAnalysis$exprs,
                                                              newTab = out,
                                                              name = 'spiderMap')
                     attr(x,'mdtnTimeStmp')<-newTimeStmp
                     attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

                   },

                   alligatorMap = {

                     out<-mdm_alligatorMap(x,labelLayer,mthd,mthdPrmtrs)

                     x$currentAnalysis$segmentation<-out

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     attr(x,'mdtnTimeStmp')<-newTimeStmp
                     attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

                     out<-extractPolygons(out)

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     x$currentAnalysis$exprs<-ept_add_primary(x = x$currentAnalysis$exprs,
                                                              newTab = out,
                                                              name = 'alligatorMap')
                     attr(x,'mdtnTimeStmp')<-newTimeStmp
                     attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

                   },

                   lazyCatMap = {

                     out<-mdm_lazyCatMap(x,labelLayer,mthd,mthdPrmtrs)

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     out<-initObjectAttr(out)

                     x$currentAnalysis$exprs<-ept_add_primary(x = x$currentAnalysis$exprs,
                                                              newTab = out,
                                                              name = 'lazyCatMap')
                     attr(x,'mdtnTimeStmp')<-newTimeStmp
                     attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

                   },

                   pandaMap = {

                     out<-mdm_pandaMap(x,labelLayer,mthd,mthdPrmtrs)

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     out<-initObjectAttr(out)

                     x$currentAnalysis$exprs<-ept_add_primary(x = x$currentAnalysis$exprs,
                                                              newTab = out,
                                                              name = 'pandaMap')
                     attr(x,'mdtnTimeStmp')<-newTimeStmp
                     attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

                   }
            )


          })
