prettyPrintList<-function(lst,maxL = 20){
  allFirst<-names(lst)
  cF<-crayon::combine_styles('bold','underline','magenta')
  cS<-crayon::combine_styles('bold','yellow')
  allLength<-lapply(seq_along(lst),function(x){
    lapply(seq_along(lst[[x]]),function(y){
      ynms<-names(lst[[x]])[y]
      if (is.null(lst[[x]][[y]])) {
        yval<-'(empty)'} else {
          oo<-paste(unname(as.character(lst[[x]][[y]])),collapse = ', ')
          if (nchar(oo)<=maxL){
            yval<-substr(paste(unname(as.character(lst[[x]][[y]])),collapse = ', '),1,maxL)
          } else {
            yval<-paste0(substr(paste(unname(as.character(lst[[x]][[y]])),collapse = ', '),1,maxL-3),'...')
          }
        }
      ynmsL<-nchar(ynms)
      yvalL<-nchar(yval)
      return(c(ynmsL,yvalL))
    })
  })
  allLengthMax<-max(unlist(allLength,recursive = T))

  alllabel<-lapply(seq_along(lst),function(x){
    lapply(seq_along(lst[[x]]),function(y){
      ynms<-names(lst[[x]])[y]
      if (is.null(lst[[x]][[y]])) {
        yval<-'(empty)'} else {
          oo<-paste(unname(as.character(lst[[x]][[y]])),collapse = ', ')
          if (nchar(oo)<=maxL){
            yval<-substr(paste(unname(as.character(lst[[x]][[y]])),collapse = ', '),1,maxL)
          } else {
            yval<-paste0(substr(paste(unname(as.character(lst[[x]][[y]])),collapse = ', '),1,maxL-3),'...')
          }
        }
      ynmsL<-nchar(ynms)
      yvalL<-nchar(yval)
      ynms<-paste0(paste0(rep(' ',allLengthMax-ynmsL),collapse = ''),ynms)
      yval<-paste0(yval,paste0(rep(' ',allLengthMax-yvalL),collapse = ''))
      out<-paste(ynms,yval,sep=':')
      return(c(out))
    })
  })
  for (i in seq_along(allFirst)){
    cat(cF(paste0(allFirst[i],':','\n')))
    maxchr<-length(alllabel[[i]])
    for (ii in seq_along(alllabel[[i]])){
      ind<-formatC(ii,width = ceiling(log(maxchr+1,10))+1,flag='0')
      cat(cS(paste0(ind,': ',alllabel[[i]][[ii]],'\n')))
    }
  }
}
