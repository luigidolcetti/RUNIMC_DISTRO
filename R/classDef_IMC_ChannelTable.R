#' IMC_ChannelTable
#'
#' Data frame containing information regarding channel and markers names
#' @export
IMC_ChannelTable<-setClass('IMC_ChannelTable',
                           contains = 'data.frame')

setMethod('initialize','IMC_ChannelTable',
          function(.Object,val = NULL, ...) {
            Object <- callNextMethod(.Object, ...)

            prtp<-data.frame(
              columnNames=character(0),
              RcolumnNames=character(0),
              channel=character(0),
              marker=character(0),
              loaded=logical(0),
              stringsAsFactors = F)

            if (is.null(val)) {
              Object@.Data<-prtp
              Object@names <- names(prtp)
              Object@row.names <- row.names(prtp)
              Object<-initObjectAttr(Object)
            } else {
              val<-val[,colnames(val) %in% colnames(prtp)]
              missingColumns<-colnames(prtp)
              missingColumns<-missingColumns[colnames(missingColumns) %in% colnames(val)]
              missingColumns<-data.frame(matrix(NA,
                                                nrow=nrow(val),
                                                ncol=length(missingColumns),
                                                dimnames = list(rownames(val),missingColumns)))
              val<-cbind.data.frame(val,missingColumns)
              val<-val[colnames(prtp)]
              val<-rbind.data.frame(prtp,val)
              Object@.Data <- val
              Object@names <- names(val)
              Object@row.names <- row.names(val)
              Object<-initObjectAttr(Object)
            }

            return(Object)})
