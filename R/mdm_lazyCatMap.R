mdm_lazyCatMap<-function(x,labelLayer,mthd,mthdPrmtrs){

  rstNames<-names(x$currentAnalysis$classification)
  rstNames<-setNames(rstNames,rstNames)

  rstToSegment<-lapply(rstNames,function(nms){
    if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) {
      stop(mError('Check classification layer name provided'))}
    return(x$currentAnalysis$classification[[nms]][[labelLayer]])
  })

  rstNames<-names(rstToSegment)
  rstNames<-setNames(rstNames,rstNames)

  polygonsList<-lapply(rstNames, function(rst){
    out<-lapply(setNames(labelLayer,labelLayer),function(i){

      cat(paste(rst,i,'\n',sep=":::"))

      out<-lazyCatMap(fn_srt = rstToSegment[[rst]][[i]],
                      fn_uid = rst,
                      fn_indexToExclude = mthdPrmtrs$indexToExclude)
    return(out)
    })
    out<-do.call(dplyr::bind_rows,out)
    return(out)
  })

polygonsList<-do.call(dplyr::bind_rows,polygonsList)

newIDs<-apply(polygonsList,1,digest::digest)

oldNames<-colnames(polygonsList)
polygonsList<-dplyr::bind_cols(polygonsList,pIDs=newIDs)[,c('pIDs',oldNames)]

}
