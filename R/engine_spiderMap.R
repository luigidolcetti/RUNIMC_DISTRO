#'Segmentation engines
#'
#'A segmentation engine is a function used by the wrapper [segment()].
#'  A segmentation engine can be deployed outside the mechanism of the [segment()]
#'  method for example if it becomes convenient to apply specific values for
#'  each different cell subtype.
#'  [spiderMap()] offers a basic segmentation algorithm that is highly heuristic
#'  because it lacks a deterministic assignment of seeds, as opposed to
#'  [ratMap()] and [slothMap()] where the seeds are determined at the classification
#'  level.
#'
#'@param fn_srt rasterLayer, a classification discrete raster layer, as produced
#'  by the **randomForest** classification method, that describes in a single
#'  raster all the classes.
#'@param fn_interpret interpretation matrix, as produced by [addInterpretationMatrix()]
#'  and possibly manually modified.
#'@param fn_radius numeric, length (pixels) of the radius of the scanning mask
#'@param fn_densityMultiplier ....
#'@param fn_Nspikes numeric, number of vectors to use in the scanning matrix
#'@param fn_minArea numeric, minimum area extent of a polygon to be accepted
#'@param fn_maxArea numeric, the same as minArea
#'@param fn_minRoundness numeric, minimum roundness of a polygon to be accepted
#'@param fn_maxRoundness numeric, the same a s minRoundness
#'@param fn_coverage numeric, minimum coverage (between 0 and 1) of a pixel,
#'  to be included in a newly defined polygon and therfore masked out.
#'@param fn_seedOutScore numeric, maximum nuber of times a seed should be
#'  used to try before being discarded.
#'@param fn_cutSeedList numeric, proportion (between 0 and 1) of the seed list
#'  to discard after a number of unsuccesfull try. used only if **fn_direction**
#'  is **insideout**.
#'@param fn_cyclewindow numeric, how many cycles to run before modifying the
#'  search parameters
#'@param fn_discoveryTreshold numeric, ratio of new polygons/ n cycles to be
#'  discovered within a **fn_cycleWindow** to prevent the modification
#'  of the search parameters.
#'@param fn_adaptative logic, whether to apply (TRUE) or not (FALSE) the mechanisms
#'  that alter search parameters
#'@param fn_drastic numeric, proportion of fn_minArea that determine the drastic
#'  dismissal of all the pixels included in a polygon that is considered too small.
#'@param fn_direction character, can be one among: **random**, **insideout** and
#'  **outsidein**. It determines the order in which seeds will be interrogated
#'  and depends on the pixel density as determined by the function [MASS::kde2d],
#'  in which the bandwidth will be **fn_radius x fn_densityMultiplier**.
#'@param fn_seed numeric, number used in [set.seed()] if **fn_direction** is set
#'  to **random**
#'@param ... not implemented
#'@return IMC_segmentation object
#' @export
spiderMap<-function (fn_srt,
                     fn_interpret,
                     fn_radius=10,
                     fn_densityMultiplier=1,
                     fn_Nspikes=4,
                     fn_minArea=10,
                     fn_maxArea=100,
                     fn_minRoundness=0.6,
                     fn_maxRoundness=0.8,
                     fn_coverage=0.3,
                     fn_seedOutScore=10,
                     fn_cutSeedList=0.1,
                     fn_cycleWindow=1000,
                     fn_discoverTreshold=10e-3,
                     fn_adaptative=T,
                     fn_drastic=0,
                     fn_direction='random',
                     fn_returnRasters = T,
                     fn_seed=1234,
                     ...){

  TimingFunction<-data.frame(Nseeds=0,Npoly=0,Ntime=Sys.time())

  xmn<-fn_srt@extent[1]
  xmx<-fn_srt@extent[2]
  ymn<-fn_srt@extent[3]
  ymx<-fn_srt@extent[4]

  labelSeed<-raster::levels(fn_srt)[[1]]$ID[raster::levels(fn_srt)[[1]]$label==rownames(fn_interpret)[fn_interpret$seed==1]]
  bunchOfSeeds<-which(raster::getValues(fn_srt)%in%labelSeed)

  if (length(bunchOfSeeds)==0){
    if (!fn_returnRasters) fn_srt<-raster::raster(matrix(0))
    fakePolygon<-matrix(data = c(xmn,ymn,xmn+1,ymn,xmn+1,ymn+1,xmn,ymn+1,xmn,ymn),
                        ncol = 2,
                        byrow = T)
    colnames(fakePolygon)<-c('x','y')
    segmentationOut<-new('IMC_Segmentation',
                         polygons=list(fakePolygon),
                         performance=data.frame(Nseeds=numeric(0),
                                                Npoly=numeric(0),
                                                Ntime=numeric(0))
                         ,raster=fn_srt)
    return(segmentationOut)
  }


  if (fn_direction!='random'){
    bunchCoords<-xyFromCell(fn_srt,bunchOfSeeds)
    densityEstimate<-MASS::kde2d(x=bunchCoords[,'x'],
                           y=bunchCoords[,'y'],
                           lims = c(xmn,xmx,ymn,ymx),
                           h = fn_radius*fn_densityMultiplier,

                           n=c(xmx-xmn,ymx-ymn))


    densityMatrix<-apply(t(densityEstimate$z),2,rev)
    rasterDensity<-raster::raster(densityMatrix,
                          xmn=xmn,
                          xmx=xmx,
                          ymn=ymn,
                          ymx=ymx,
                          crs=sp::CRS(as.character(NA)))
    densityValue<-getValues(rasterDensity)[bunchOfSeeds]} else {
      densityValue<-rep(NA,length(bunchOfSeeds))}

  bunchOfSeeds<-data.frame(seed= bunchOfSeeds,
                           score=rep(0,length(bunchOfSeeds)),
                           active=rep(1,length(bunchOfSeeds)),
                           density=densityValue)

  bunchOfSeeds<-switch(fn_direction,
                       insideout={bunchOfSeeds[order(bunchOfSeeds$density,decreasing = T),]},
                       outsidein={bunchOfSeeds[order(bunchOfSeeds$density,decreasing = F),]},
                       random={set.seed(fn_seed)
                         bunchOfSeeds[sample(nrow(bunchOfSeeds)),]})

  polyList<-vector(mode = 'list',length = nrow(bunchOfSeeds))

  rMsk<-radialMask(fn_radius,fn_Nspikes)
  sinMatrix<-rMsk$sinMatrix
  cosMatrix<-rMsk$cosMatrix
  intersectionMatrix<-rMsk$intersectionMatrix

  polyIndex=0
  cycleIndex=1
  oldCycleIndex=1
  oldPolyIndex=1
  oldSeedIndex=0
  seedPointer=1
  activeSeed<-c()
  #
  while(sum(bunchOfSeeds$active)>0){
    #
    if (length(activeSeed)==0){
      repeat{
        if (bunchOfSeeds$active[seedPointer]==1){

          activeSeed<-bunchOfSeeds$seed[seedPointer]
          bunchOfSeeds$score[seedPointer]<-bunchOfSeeds$score[seedPointer]+1
          if (seedPointer==nrow(bunchOfSeeds)) {seedPointer<-1} else {seedPointer<-seedPointer+1}
          break
        } else {

          if (seedPointer==nrow(bunchOfSeeds)) {seedPointer<-1} else {seedPointer<-seedPointer+1}
        }
      }
    }

    TEMP_windowFraction<-round((cycleIndex-oldCycleIndex)/fn_cycleWindow*10)
    TEMP_remaining<-10-TEMP_windowFraction

    cat(paste0(rownames(fn_interpret)[fn_interpret$seed==1],' / ',
               'N of seeds: ',formatC(sum(bunchOfSeeds$active),flag='0',digits = 9),
               ' / N of polygons: ',formatC(polyIndex,flag='0',digits = 9),
               # paste0(arrowVector,collapse=''),
               ' / cycleIndex: ', formatC(cycleIndex,flag='0',digits=9),
               ' / discovery window |',paste0(rep("@",TEMP_windowFraction),collapse = ""),
               paste0(rep("_",TEMP_remaining),collapse = ""),
               '| / throwout after: ',
               formatC(fn_seedOutScore,flag='0',digits=3),
               '\r'))

    #paste0('/ ',mill[millIndex],' ',arrowarrowchr[arrowIndex]),
    xCoords<-raster::xyFromCell(fn_srt,activeSeed)[1]
    yCoords<-raster::xyFromCell(fn_srt,activeSeed)[2]

    xScan<-sinMatrix+xCoords
    yScan<-cosMatrix+yCoords

    xScan[xScan<xmn]<-NA
    yScan[yScan<ymn]<-NA
    xScan[xScan>xmx]<-NA
    yScan[yScan>ymx]<-NA
    xyScan<-matrix(1:length(xScan),ncol=ncol(xScan),nrow=nrow(xScan))

    xyVal<-apply(xyScan, c(1,2),function(x){
      ifelse((is.na(xScan[[x]]) | is.na(yScan[[x]])),
             NA,
             fn_srt[raster::cellFromXY(fn_srt,c(xScan[[x]],yScan[[x]]))])
    })

    spikeEnds<-apply(xyVal,1,
                     function(profileVector){

                       profileAction<-as.vector(unlist(sapply(profileVector,function(x){
                         if (any(x %in% fn_interpret$label)) {fn_interpret$action[fn_interpret$label==x]} else {return(2)}})))
                       profileInclude<-as.vector(unlist(sapply(profileVector,function(x){
                         if (any(x %in% fn_interpret$label)) {fn_interpret$include[fn_interpret$label==x]} else {return(0)}})))

                       profileIndex<-1:fn_radius
                       externalVal<-Inf
                       for ( i in profileIndex){
                         #

                         swtc<-switch(as.character(profileAction[i]),
                                      `2` = {(list=c(i,profileInclude[i]))},
                                      `1` = {
                                        if (externalVal==Inf) {externalVal=profileVector[i]}
                                        if (is.na(profileVector[i])) {(list=c(i,0))} else {
                                          if (externalVal!=profileVector[i]) {(list=c(i,profileInclude[i]))}}},
                                      `0` = {if (externalVal!=Inf){(list=c(i,1))}})
                         if(!is.null(swtc)){return(swtc)}

                       }
                       return(list=c(i,1))
                     })

    polyG<-t(sapply(1:fn_Nspikes,function(x){
      intersectionMatrix[x,spikeEnds[1,x]][[1]][spikeEnds[2,x]+1,]+c(xCoords,yCoords)}))
    colnames(polyG)<-c('x','y')

    polyG<-na.omit(polyG)
    notDup<-!duplicated(polyG)
    polyG<-polyG[notDup,,drop=F]
    polyGXmax<-max(polyG[,'x'])
    polyGXmin<-min(polyG[,'x'])
    polyGYmax<-max(polyG[,'y'])
    polyGYmin<-min(polyG[,'y'])
    roughPolyArea<-(polyGXmax-polyGXmin)*(polyGYmax-polyGYmin)

    if (roughPolyArea<fn_drastic) {
      polyGX<-seq(floor(polyGXmin),floor(polyGXmax))
      polyGY<-seq(floor(polyGYmin),floor(polyGYmax))
      polyGXY<-expand.grid(x=polyGX,y=polyGY)
      polyGCell<-apply(polyGXY,1,function(x) raster::cellFromXY(fn_srt,x))
      # arrowIndex<-5
      # arrowVector<-c(arrowVector[-1],arrowarrowchr[arrowIndex])
      bunchOfSeeds$active[(bunchOfSeeds$seed %in% polyGCell)]<-0
      activeSeed<-c()
    } else {

      if (!(nrow(polyG)<3)){

        # if (millIndex==4) {millIndex<-1} else {millIndex<-millIndex+1}

        polyG<-rbind(polyG,polyG[1,])
        sfPolygon<-sf::st_polygon(list(as.matrix(polyG)),dim="XY")
        polyArea<-sf::st_area(sf::st_sfc(sfPolygon))
        polyPerimeter<-lwgeom::st_perimeter(sf::st_sfc(sfPolygon))
        polyRoundness<-4*pi*polyArea/(polyPerimeter^2)
        coverageDF <- (exactextractr::exact_extract(fn_srt,sf::st_sfc(sfPolygon),include_xy=T))[[1]]
        coverageDF<-coverageDF[coverageDF$coverage_fraction>=fn_coverage,]
        coordDF<-raster::cellFromXY(fn_srt,as.matrix(coverageDF[,c(2,3)]))

        TimingFunction<-rbind(TimingFunction,data.frame(Nseeds=nrow(bunchOfSeeds),Npoly=polyIndex,Ntime=Sys.time()))

        if (polyArea<fn_minArea) {
          bunchOfSeeds$active[(bunchOfSeeds$seed %in% coordDF)]<-0
          activeSeed<-c()}
        else {
          if (polyArea>fn_maxArea){
            activeSeed<-bunchOfSeeds[bunchOfSeeds$seed %in% coordDF,]
            activeSeed<-activeSeed[sample(length(activeSeed),1),]
            activeSeed<-activeSeed$seed[1]
            bunchOfSeeds$score[bunchOfSeeds$seed==activeSeed]<-bunchOfSeeds$score[bunchOfSeeds$seed==activeSeed]+1
          } else {
            if (polyRoundness>=fn_minRoundness & polyRoundness<=fn_maxRoundness){
              bunchOfSeeds$active[bunchOfSeeds$seed %in% coordDF]<-0
              polyIndex=polyIndex+1
              fn_srt[coordDF]<-(-polyIndex)
              polyList[[polyIndex]]<-polyG
              activeSeed<-c()
            } else {
              activeSeed<-c()}
          }
        }
      } else {
        activeSeed<-c()}
    }

    if ((cycleIndex-oldCycleIndex)==fn_cycleWindow){
      DPI<-(polyIndex-oldPolyIndex)/(cycleIndex-oldCycleIndex)
      if (DPI<fn_discoverTreshold & fn_adaptative==F) {break()}
      if (DPI<fn_discoverTreshold &
          fn_adaptative==T &
          fn_seedOutScore>1 &
          (fn_direction=='insideout' |
           fn_direction=='random')) {fn_seedOutScore=fn_seedOutScore-1}
      if (DPI<fn_discoverTreshold &
          fn_adaptative==T &
          (nrow(bunchOfSeeds)-round(fn_cutSeedList*nrow(bunchOfSeeds)))>0 &
          fn_direction=='outsidein') {bunchOfSeeds<-bunchOfSeeds[-(1:round(fn_cutSeedList*nrow(bunchOfSeeds))),]}
      oldCycleIndex=cycleIndex
      oldPolyIndex=polyIndex
    }
    bunchOfSeeds$active[bunchOfSeeds$score>fn_seedOutScore]<-0
    cycleIndex=cycleIndex+1
  }
  cat(paste0(rownames(fn_interpret)[fn_interpret$seed==1],' / ',
             'N of seeds: ',formatC(sum(bunchOfSeeds$active),flag='0',digits = 9),
             ' / N of polygons: ',formatC(polyIndex,flag='0',digits = 9),
             ' / cycleIndex: ', formatC(cycleIndex,flag='0',digits=9),
             ' / discovery window |',paste0(rep("@",TEMP_windowFraction),collapse = ""),
             paste0(rep("_",TEMP_remaining),collapse = ""),
             '| / throwout after: ',
             formatC(fn_seedOutScore,flag='0',digits=3),
             '\n'))

  polyList<-polyList[1:polyIndex]

  TimingFunction<-rbind(TimingFunction,data.frame(Nseeds=nrow(bunchOfSeeds),Npoly=polyIndex,Ntime=Sys.time()))

  segmentationOut<-new('IMC_Segmentation',polygons=polyList,performance=TimingFunction,raster=fn_srt)
  return(segmentationOut)
}
