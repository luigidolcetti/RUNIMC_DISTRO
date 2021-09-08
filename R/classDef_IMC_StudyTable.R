
#'
IMC_StudyTable<-setClass('IMC_StudyTable',
                         contains = 'data.frame')

setMethod('initialize','IMC_StudyTable',
          function(.Object,val=NULL, ...) {
            Object <- callNextMethod(.Object, ...)

            prtp<-data.frame(
              uid=character(0),
              IMC_folder=character(0),
              IMC_text_file=character(0),
              study=character(0),
              sample=character(0),
              replicate=character(0),
              ROI=character(0),
              bioGroup=character(0),
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
