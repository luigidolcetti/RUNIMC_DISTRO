#' Extraction Directives
#'
#' Training feature directives determine how training features (pixels) are extracted
#'   from training data. [addExtractionDirectives()] either initialize or add a new
#'   row to the table of directives.
#'
#' @param x IMC_Study, a study
#' @param coverage numeric, a pair of values between 0 and 1 that specify the range
#'   of coverage to be included in the category
#' @param prefix character, a prefix that will be added to each label in the form
#'   prefix_label. It must not contain any underscore
#' @param append logic, initialize (FALSE) or append a new definition (TRUE)
#' @param ... not implemented
#' @examples
#' \dontrun{
#' addExtractionDirectives(MyStudy,c(0.75,100),'core', append = F)
#' addExtractionDirectives(MyStudy,c(0,0.75),'border', append = T)
#' }
#' @export
setGeneric("addExtractionDirectives", function(x,
                                               coverage=NULL,
                                               prefix=NULL,
                                               append=T,
                                               ...)
  standardGeneric("addExtractionDirectives"))


setMethod('addExtractionDirectives',signature = ('IMC_Study'),
          function(x,
                   coverage=NULL,
                   prefix=NULL,
                   append=T,
                   ...){


            if (is.null(coverage)) stop(mError('provide coverage bounderies'))
            if (is.null(prefix)) stop(mError(paste0('provide prefix to use in label')))

            if (append) x$currentAnalysis$Directives<-x$currentAnalysis$extractionDirectives else x$currentAnalysis$extractionDirectives<-NULL

            if (is.null(x$currentAnalysis$extractionDirectives)) {
              newDirectives<-new('IMC_ExtractionDirectives')

              x$currentAnalysis$extractionDirectives<-newDirectives
            }

            newDirectives<-data.frame(coverage=I(list(coverage)),prefix=I(list(prefix)))

            oldDirectives<-x$currentAnalysis$extractionDirectives

            newDirectives<-rbind.data.frame(oldDirectives,newDirectives)

            newDirectives<-new('IMC_ExtractionDirectives',newDirectives)
            newDirectives<-initObjectAttr(newDirectives)
            x$currentAnalysis$extractionDirectives<-newDirectives

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

          })
