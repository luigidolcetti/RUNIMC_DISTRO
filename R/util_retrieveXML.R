retrieveXML<-function(fn_file=NULL,
                       fn_timeStamp=T){

  fileContent<-try(XML::xmlParse(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }

  rootNode<-XML::xmlRoot(fileContent)
  childrenNode<-XML::xmlChildren(rootNode)
  chldNdNames<-names(childrenNode)
  if (XML::xmlName(rootNode)=='study'){
    newStudy<-new('IMC_Study')
    attr(newStudy,'crtnTimeStmp')<-XML::xmlGetAttr(rootNode,'crtnTimeStmp')
    attr(newStudy,'mdtnTimeStmp')<-XML::xmlGetAttr(rootNode,'mdtnTimeStmp')
    attr(newStudy,'artnTimeStmp')<-XML::xmlGetAttr(rootNode,'artnTimeStmp')
    attr(newStudy,'fileArchive')<-XML::xmlGetAttr(rootNode,'fileArchive')
    newStudy$name<-as.character(XML::xmlValue(childrenNode$name))
    newStudy$rootFolder<-as.character(XML::xmlValue(childrenNode$rootFolder))
    newStudy$rawDataFolder<-as.character(XML::xmlValue(childrenNode$rawDataFolder))
    newStudy$whichColumns<-as.character(XML::xmlValue(childrenNode$whichColumns))
    newStudy$analysis<-unlist(strsplit(x = XML::xmlValue(childrenNode$analysis),split = ',',fixed = T),recursive = T)
    #studyTable
    targetFile<-XML::xmlValue(childrenNode$studyTable)
    if (file.exists(targetFile)){
      newStudy$studyTable<-retrieve(targetFile)
      attr(newStudy$studyTable,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$studyTable,'crtnTimeStmp')
      attr(newStudy$studyTable,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$studyTable,'mdtnTimeStmp')
      attr(newStudy$studyTable,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$studyTable,'artnTimeStmp')
      attr(newStudy$studyTable,'fileArchive')<-targetFile} else {newStudy$studyTable<-NULL}
    #channelTable
    targetFile<-XML::xmlValue(childrenNode$channels)
    if (file.exists(targetFile)){
      newStudy$channels<-retrieve(targetFile)
      attr(newStudy$channels,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$channels,'crtnTimeStmp')
      attr(newStudy$channels,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$channels,'mdtnTimeStmp')
      attr(newStudy$channels,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$channels,'artnTimeStmp')
      attr(newStudy$channels,'fileArchive')<-targetFile} else {newStudy$channels<-NULL}
    #rasters
    targetFile<-XML::xmlValue(childrenNode$raster)
    if (dir.exists(targetFile)){
      newStudy$raster<-retrieve(targetFile)
      attr(newStudy$raster,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$raster,'crtnTimeStmp')
      attr(newStudy$raster,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$raster,'mdtnTimeStmp')
      attr(newStudy$raster,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$raster,'artnTimeStmp')
      attr(newStudy$raster,'fileArchive')<-targetFile} else {newStudy$raster<-NULL}
    #currentAnalysis

    targetFile<-XML::xmlValue(childrenNode$currentAnalysis)
    if (file.exists(targetFile)){
      newStudy$currentAnalysis<-retrieve(targetFile)
      attr(newStudy$currentAnalysis,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$currentAnalysis,'crtnTimeStmp')
      attr(newStudy$currentAnalysis,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$currentAnalysis,'mdtnTimeStmp')
      attr(newStudy$currentAnalysis,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$currentAnalysis,'artnTimeStmp')
      attr(newStudy$currentAnalysis,'fileArchive')<-targetFile} else {newStudy$currentAnalysis<-NULL}


    # if (!is.null(newStudy$currentAnalysis$classification)){
    #   namesList<-names(newStudy$currentAnalysis$classification)
    #   namesList<-sapply(namesList,function(x) {
    #     x<-sub('.grd','',x)},USE.NAMES = F,simplify = T)
    #   namesList<-sapply(namesList,function(x) {newStudy$studyTable$uid[newStudy$studyTable$IMC_text_file==x]})
    #   names(newStudy$currentAnalysis$classification)<-namesList
    # }

    return(newStudy)

  }
  #parse Analysis
  if (XML::xmlName(rootNode)=='analysis'){

    newAnal<-new('IMC_Analysis')
    attr(newAnal,'crtnTimeStmp')<-XML::xmlGetAttr(rootNode,'crtnTimeStmp')
    attr(newAnal,'mdtnTimeStmp')<-XML::xmlGetAttr(rootNode,'mdtnTimeStmp')
    attr(newAnal,'artnTimeStmp')<-XML::xmlGetAttr(rootNode,'artnTimeStmp')
    attr(newAnal,'fileArchive')<-XML::xmlGetAttr(rootNode,'fileArchive')
    newAnal$folder<-as.character(XML::xmlValue(childrenNode$folder))
    newAnal$name<-as.character(XML::xmlValue(childrenNode$name))

    #classification
    targetFile<-XML::xmlValue(childrenNode$classification)
    if (file.exists(targetFile)){
      newAnal$classification<-retrieve(targetFile)

      attr(newAnal$classification,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classification,'crtnTimeStmp')
      attr(newAnal$classification,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classification,'mdtnTimeStmp')
      attr(newAnal$classification,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$classification,'artnTimeStmp')
      attr(newAnal$classification,'fileArchive')<-targetFile} else {newAnal$classification<-NULL}
    #classificationDirectives
    targetFile<-XML::xmlValue(childrenNode$classificationDirectives)
    if (file.exists(targetFile)){
      newAnal$classificationDirectives <-retrieve(targetFile)
      attr(newAnal$classificationDirectives,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classificationDirectives,'crtnTimeStmp')
      attr(newAnal$classificationDirectives,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classificationDirectives,'mdtnTimeStmp')
      attr(newAnal$classificationDirectives,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$classificationDirectives,'artnTimeStmp')
      attr(newAnal$classificationDirectives,'fileArchive')<-targetFile} else {newAnal$classificationDirectives<-NULL}
    #classifier
    targetFile<-XML::xmlValue(childrenNode$classifier)
    if (file.exists(targetFile)){
      newAnal$classifier<-retrieve(targetFile)
      attr(newAnal$classifier,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classifier,'crtnTimeStmp')
      attr(newAnal$classifier,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classifier,'mdtnTimeStmp')
      attr(newAnal$classifier,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$classifier,'artnTimeStmp')
      attr(newAnal$classifier,'fileArchive')<-targetFile} else {newAnal$classifier<-NULL}
    #exprs
    targetFile<-XML::xmlValue(childrenNode$exprs)
    if (file.exists(targetFile)){

      newAnal$exprs<-retrieve(targetFile)
      attr(newAnal$exprs,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$exprs,'crtnTimeStmp')
      attr(newAnal$exprs,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$exprs,'mdtnTimeStmp')
      attr(newAnal$exprs,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$exprs,'artnTimeStmp')
      attr(newAnal$exprs,'fileArchive')<-targetFile} else {newAnal$exprs<-NULL}
    #extractionDirectives
    targetFile<-XML::xmlValue(childrenNode$extractionDirectives)
    if (file.exists(targetFile)){
      newAnal$extractionDirectives<-retrieve(targetFile)
      attr(newAnal$extractionDirectives,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$extractionDirectives,'crtnTimeStmp')
      attr(newAnal$extractionDirectives,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$extractionDirectives,'mdtnTimeStmp')
      attr(newAnal$extractionDirectives,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$extractionDirectives,'artnTimeStmp')
      attr(newAnal$extractionDirectives,'fileArchive')<-targetFile} else {newAnal$extractionDirectives<-NULL}
    #filters
    targetFile<-XML::xmlValue(childrenNode$filters)
    if (file.exists(targetFile)){
      newAnal$filters<-retrieve(targetFile)
      attr(newAnal$filters,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$filters,'crtnTimeStmp')
      attr(newAnal$filters,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$filters,'mdtnTimeStmp')
      attr(newAnal$filters,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$filters,'artnTimeStmp')
      attr(newAnal$filters,'fileArchive')<-targetFile} else {newAnal$filters<-NULL}
    #interpretationMatrix
    targetFile<-XML::xmlValue(childrenNode$interpretationMatrix)
    if (file.exists(targetFile)){
      newAnal$interpretationMatrix<-retrieve(targetFile)
      attr(newAnal$interpretationMatrix,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$interpretationMatrix,'crtnTimeStmp')
      attr(newAnal$interpretationMatrix,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$interpretationMatrix,'mdtnTimeStmp')
      attr(newAnal$interpretationMatrix,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$interpretationMatrix,'artnTimeStmp')
      attr(newAnal$interpretationMatrix,'fileArchive')<-targetFile} else {newAnal$interpretationMatrix<-NULL}
    #segmentation
    targetFile<-XML::xmlValue(childrenNode$segmentation)
    if (file.exists(targetFile)){
      newAnal$segmentation<-retrieve(targetFile)
      attr(newAnal$segmentation,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentation,'crtnTimeStmp')
      attr(newAnal$segmentation,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentation,'mdtnTimeStmp')
      attr(newAnal$segmentation,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentation,'artnTimeStmp')
      attr(newAnal$segmentation,'fileArchive')<-targetFile} else {newAnal$segmentation<-NULL}
    #segmentationDirectives
    targetFile<-XML::xmlValue(childrenNode$segmentationDirectives)
    if (file.exists(targetFile)){
      newAnal$segmentationDirectives<-retrieve(targetFile)
      attr(newAnal$segmentationDirectives,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentationDirectives,'crtnTimeStmp')
      attr(newAnal$segmentationDirectives,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentationDirectives,'mdtnTimeStmp')
      attr(newAnal$segmentationDirectives,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentationDirectives,'artnTimeStmp')
      attr(newAnal$segmentationDirectives,'fileArchive')<-targetFile} else {newAnal$segmentationDirectives<-NULL}
    #trainingFeatures
    targetFile<-XML::xmlValue(childrenNode$trainingFeatures)
    if (file.exists(targetFile)){
      newAnal$trainingFeatures<-retrieve(targetFile)
      attr(newAnal$trainingFeatures,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$trainingFeatures,'crtnTimeStmp')
      attr(newAnal$trainingFeatures,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$trainingFeatures,'mdtnTimeStmp')
      attr(newAnal$trainingFeatures,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$trainingFeatures,'artnTimeStmp')
      attr(newAnal$trainingFeatures,'fileArchive')<-targetFile} else {newAnal$trainingFeatures<-NULL}
    #derivedRasters
    targetFile<-XML::xmlValue(childrenNode$derivedRasters)
    if (dir.exists(targetFile)){
      newAnal$derivedRasters<-retrieve(targetFile)
      attr(newAnal$derivedRasters,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$derivedRasters,'crtnTimeStmp')
      attr(newAnal$derivedRasters,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$derivedRasters,'mdtnTimeStmp')
      attr(newAnal$derivedRasters,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$derivedRasters,'artnTimeStmp')
      attr(newAnal$derivedRasters,'fileArchive')<-targetFile} else {newAnal$derivedRasters<-NULL}


    return(newAnal)


  }

  # return(fileContent)
}
