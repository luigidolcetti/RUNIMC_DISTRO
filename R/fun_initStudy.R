#' Study initialization
#'
#' [initStudy()] provide functionality to initialize and create the infrastructure to accommodate a new study.
#'
#' @param fn_studyName character, name of the study.
#' @param fn_rootFolder character, directory where to store all the files inherent to this study.
#' @param fn_rawDataFolder character, directory containing the raw data txt files.
#' @param fn_whichFiles numeric vector, which files should be loaded (alphabetical order as produced by list.files).
#'   If NULL (default) all files will be loaded.
#' @param fn_whichColumns character, can be either 'named' (default) or 'all'.
#'   Header of the txt table will be parsed in search of names of possible markers,
#'   providing 'named', only those columns that seem to contain a name are loaded.
#' @param fn_x_column character, name of the column containing X coordinates.
#' @param fn_y_column character, name of the column containing Y coordinates.
#' @param fn_nonChannelColumns character vector, names of non-data columns.
#' @param fn_emptyChannelPattern character, regex expression of empty columns.
#' @param fn_usedChannelPattern character, regex expression of used columns.
#' @param fn_channelPattern character, regex expression to extract the channel name.
#' @param fn_markerPattern character, regex expression to extract the marker name.
#' @param fn_transpose logical, should the raster be transposed?
#' @param fn_overwrite logical, if TRUE and a study with the same name is present
#'   on the disk, files will be delete first. FALSE, produce a milder effect
#'   producing a progressive overwrite.
#' @param fn_nThread numeric scalar, number of thread to use in [data.table::fread],
#'   if data.table version is <1.14.3 fn_nThread is coerced to 1
#' @param fn_verbose logical, describe what is going on?
#' @return An environment containing a new study and a hierarchy of files where specified.
#' @examples
#' \dontrun{
#' study<-initStudy('TEST_study',
#' 'c:/Data/whereToStoreStudy,
#' 'c:/Data/whereToGetRawData,
#' NULL,
#' 'named',
#' F,
#' F,
#' T)
#' }
#' @export
initStudy<-function(fn_studyName='IMCstudy',
                    fn_rootFolder=NULL,
                    fn_rawDataFolder=NULL,
                    fn_whichFiles=NULL,
                    fn_whichColumns='named',
                    fn_x_column = 'X',
                    fn_y_column = 'Y',
                    fn_nonChannelColumns = c('X','Y','Z','Start_push','End_push','Pushes_duration'),
                    fn_emptyChannelPattern = '[0-9]+[A-z]+\\([A-z]+[0-9]+[A-z]+\\)',
                    fn_usedChannelPattern = '[0-9]+[A-z]+-.+\\([A-z]+[0-9]+[A-z]+\\)',
                    fn_channelPattern = '[0-9]+[A-z]+.*\\(([A-z]+[0-9]+[A-z]+)\\)',
                    fn_markerPattern = '[0-9]+[A-z]+-(.+)\\([A-z]+[0-9]+[A-z]+\\)',
                    fn_transpose=F,
                    fn_overWrite=F,
                    fn_nThread = 1,
                    fn_verbose=T,
                    ...){

  if (is.null(fn_studyName)) stop(mError('provide a name for the study'),call. = F)
  if (is.null(fn_rootFolder)) stop(mError('provide a folder where to store the study'),call. = F)
  if (is.null(fn_rawDataFolder)) stop(mError('provide a folder containing raw data'),call. = F)
  if (is.null(fn_whichColumns)){
    fn_whichColumns<-T
  } else {
    if (!any(fn_whichColumns %in% c('named','all'))) {
      stop(mError('fn_which column must be either "named" or "all"'),call. = F)
    }
  }
  if (fn_overWrite){
    targetFolder<-file.path(fn_rootFolder,fn_studyName)
    if (dir.exists(targetFolder)) unlink(targetFolder,recursive = T,force = T)
  }

  studyFolder<-checkDir(fn_rootFolder,fn_studyName,verbose=fn_verbose)
  rasterFolder<-checkDir(studyFolder,'rasters',verbose=fn_verbose)
  stackFolder<-checkDir(studyFolder,'rasterStacks',verbose=fn_verbose)
  analysisFolder<-checkDir(studyFolder,'analysis',verbose=fn_verbose)
  archiveFolder<-checkDir(studyFolder,'archive',verbose=fn_verbose)
  TempFolder<-checkDir(studyFolder,'Temp',verbose=fn_verbose)

  rawDataFiles<-list.files(fn_rawDataFolder,full.names = T,pattern = '*.txt',recursive = F)
  if (length(rawDataFiles)==0) stop(mError("Could not find any raw data"),call. = F)
  if (!is.null(fn_whichFiles)) rawDataFiles<-rawDataFiles[fn_whichFiles]

  headersLine<-lapply(rawDataFiles,function(flnm){
    con <- file(flnm,"r")
    headerLine <- readLines(con,n=1)
    close(con)
    return(headerLine)
  })

  masterHeader<-unique(headersLine)[[1]]

  if (length(unique(headersLine))>1) {stop(mError("Headers from different files do not match"),call. = F)}

  masterHeader<-strsplit(masterHeader,'\t',fixed=T)[[1]]
  if (fn_whichColumns=='all') ldld<-T

  Channels<-lapply(masterHeader,function(mstrhdr){

    if (length(grep(mstrhdr,fn_nonChannelColumns,ignore.case = T,value = F))==0){

      ECC<-grepl(fn_emptyChannelPattern,mstrhdr)
      UCC<-grepl(fn_usedChannelPattern,mstrhdr)
      OCC<-!ECC & !UCC

      if (ECC){
        chnl<-regexpr(fn_channelPattern,mstrhdr,perl = T)
        chnl<-substr(mstrhdr,attr(chnl,'capture.start'),attr(chnl,'capture.start')+attr(chnl,'capture.length')-1)
        mrkr<-'(empty)'
        if (fn_whichColumns=='named') ldld<-F
      }

      if (UCC){
        chnl<-regexpr(fn_channelPattern,mstrhdr,perl = T)
        chnl<-substr(mstrhdr,attr(chnl,'capture.start'),attr(chnl,'capture.start')+attr(chnl,'capture.length')-1)
        mrkr<-regexpr(fn_markerPattern,mstrhdr,perl = T)
        mrkr<-substr(mstrhdr,attr(mrkr,'capture.start'),attr(mrkr,'capture.start')+attr(mrkr,'capture.length')-1)
        if (fn_whichColumns=='named') ldld<-T
      }

      if (OCC){
        chnl<-'(..??..)'
        mrkr<-'(..??..)'
        if (fn_whichColumns=='named') ldld<-NA
      }

      chnls<-data.frame(columnNames=mstrhdr,
                        RcolumnNames=tolower(make.names(mstrhdr,unique = F,allow_ = F)),
                        channel=chnl,
                        marker=mrkr,
                        loaded=ldld,
                        stringsAsFactors = F)
    }
  })

  Channels<-do.call(rbind.data.frame,Channels)
  Channels<-new('IMC_ChannelTable',Channels)

  ### load rasters

  rawDataFiles<-list.files(fn_rawDataFolder, pattern = "*.txt")
  if (!is.null(fn_whichFiles)) rawDataFiles<-rawDataFiles[fn_whichFiles]

  rst<-lapply(rawDataFiles,
              function(tblFile){
                oneRasterFolder<-checkDir(rasterFolder,tblFile,verbose=fn_verbose)
                rst<-loadTxtToStack(fn_path = fn_rawDataFolder,
                                    fn_file = tblFile,
                                    fn_rasterDBPath = oneRasterFolder,
                                    fn_rasterStackPath = stackFolder,
                                    fn_cols = Channels$columnNames[Channels$loaded],
                                    fn_newNames =  Channels$RcolumnNames[Channels$loaded],
                                    fn_details =  list(study=fn_studyName),
                                    fn_channel = Channels,
                                    fn_transpose = fn_transpose,
                                    fn_norm = F,
                                    fn_x_column = fn_x_column,
                                    fn_y_column = fn_y_column,
                                    fn_trsh = NULL,
                                    fn_zeroOff = NULL,
                                    fn_nThread = fn_nThread,
                                    fn_verbose = fn_verbose,
                                    ...)
                return(rst)})

  names(rst)<-lapply(rst,function(x){x@uid})

  rst<-new('IMC_RsCollection',rst)
  attr(rst,'artnTimeStmp')<-attr(rst,'mdtnTimeStmp')
  attr(rst,'fileArchive')<-stackFolder

  studyTable<-lapply(rst,function(x){
    data.frame(
      uid=x@uid,
      IMC_folder=fn_rawDataFolder,
      IMC_text_file=x@IMC_text_file,
      study=x@study,
      sample=x@sample,
      replicate=x@replicate,
      ROI=x@ROI,
      bioGroup=x@bioGroup,
      stringsAsFactors = F)})

  studyTable<-do.call(rbind.data.frame,studyTable)
  studyTable<-new('IMC_StudyTable',studyTable)

   newStudy<-new('IMC_Study',
                name=fn_studyName,
                rootFolder=fn_rootFolder,
                rawDataFolder=fn_rawDataFolder,
                studyTable = studyTable,
                raster=rst,
                whichColumns=fn_whichColumns,
                channels=Channels,
                analysis=NULL,
                currentAnalysis=NULL)

  return(newStudy)
}
