#' pandaMap - engine function
#'
#' Segmentation engine based on local segmentation similar to a watershed protocol
#'
#' @param fn_srt raster, map to be segmented, should represent some meaningful parameter
#'   such as the position of each single pixel as predicted by the function.
#'   [randomOnions] or [randomOnions_parallel].
#' @param fn_uid character, uid string referring to a specific ROI.
#' @param fn_primaryIndex character, used as primary index should correspond
#'   to the annotation label produced via the function [runIMC].
#' @param fn_clpDir character, shold connected pixel been searched in the 4 or 8
#'   surrounding pixels?
#' @param fn_brake numeric scalar, number of quantile brakes to use in subdividing the
#'   range of values in the raster.
#' @param fn_lowerQuantile numeric scalar, the first brake will include all the
#'   values between 0 and the lower quantile values.
#' @param fn_upperQuantile numeric scalar, the last brake will include all the
#'   values between the upper quantile value and 1.
#' @param fn_lowerAreaLimit numeric scalar, minimum area to consider a ragion as
#'   a separate polygon.
#' @param fn_movingWindow_dim numeric vector, acceptable dimension in pixel of
#'   the moving window (e.g. 100:200 or c(100,120,140,180)...). Values need to be
#'   choose accurately since a narrow window can produce artifacts whilst a window
#'   that is too large (e.g. >400 pixels) can extended the calculation time.
#' @param fn_movingWindow_overlap numeric vector, acceptable overlap values for
#'   the scanning window.
#' @param fn_cores numeric scalar, number of cores to be used in parallel calculation.
#'   Setting cl = NULL makes the process not parallel and extra information
#'   are displayed if verbose is TRUE.
#' @param fn_verbose logic scalar, show information regarding the segmentation
#'   process.
#' @param fn_TempFile character scalar, save resulting geometry to a temporary
#'   file.
#' @details Engine function should not be used directly but instead through the
#'   appropriate method (e.g. [segment])
#' @export
pandaMap<-function (fn_srt=NULL,
                    fn_uid = NULL,
                    fn_primaryIndex = 'PI',
                    fn_clpDir = c('8','4'),
                    fn_brake = 10,
                    fn_lowerQuantile=0.01,
                    fn_upperQuantile=0.50,
                    fn_lowerAreaLimit=5,
                    fn_movingWindow_dim = NULL,
                    fn_movingWindow_overlap = 25:50,
                    fn_cores=parallel::detectCores(logical = T)-1,
                    fn_verbose = F,
                    fn_TempFile = NULL){


  drct<-as.numeric(match.arg(fn_clpDir))
  if (is.null(fn_uid)) stop(mError("no uid"),call. = F)
  if (is.null(fn_srt)) stop(mError('no rater to process'))
  if (is.null(fn_primaryIndex)) stop(mError('no primary index'))
  if (is.null(fn_brake)) stop(mError('no cuts'))
  if (!is.null(fn_lowerQuantile)){
    if (fn_lowerQuantile<0 | fn_lowerQuantile>1) stop(mError('quntile outside range 0-1'))
  } else {stop(mError("no quantiles"),call. = F)}
  if (!is.null(fn_upperQuantile)){
    if (fn_upperQuantile<0 | fn_lowerQuantile>1) stop(mError('quntile outside range 0-1'))
  } else {stop(mError("no quantiles"),call. = F)}
  if (is.null(fn_lowerAreaLimit)) stop(mError("no area limit"),call. = F)
  if (!is.null(fn_lowerAreaLimit) & is.null(fn_movingWindow_overlap)) stop(mError("no overlap"),call. = F)
  if (!is.null(fn_verbose)){
    if (fn_verbose) {
      pbapply::pboptions(type = 'text')
    } else {
      pbapply::pboptions(type = 'none')
    }} else {
      fn_verbose<-F
      pbapply::pboptions(type = 'none')
    }

  if (fn_verbose) cat('Raster pre-processing.... \n')

  pixelList<-raster::values(fn_srt)
  pixelMax<-quantile(pixelList,fn_upperQuantile,na.rm=T)
  pixelMin<-quantile(pixelList,fn_lowerQuantile,na.rm=T)
  pixelBrake<-seq(fn_lowerQuantile,fn_upperQuantile,length.out=fn_brake)
  pixelBrake<-unname(quantile(pixelList,pixelBrake,na.rm=T))
  maxii<-length(pixelBrake)

  if (!is.null(fn_movingWindow_dim)){
    extRaster<-raster::extent(fn_srt)
    resRaster<-raster::res(fn_srt)
    dimXRaster<-(extRaster[2]-extRaster[1])/resRaster[1]
    dimYRaster<-(extRaster[4]-extRaster[3])/resRaster[2]

    windowSelect_X<-.bestWindow(fn_windowW = fn_movingWindow_dim,
                                fn_overlapW = fn_movingWindow_overlap,
                                fn_totalW = dimXRaster)

    windowSelect_Y<-.bestWindow(fn_windowW = fn_movingWindow_dim,
                                fn_overlapW = fn_movingWindow_overlap,
                                fn_totalW = dimYRaster)

    if (fn_verbose){
      print(windowSelect_X)
      print(windowSelect_Y)
    }

    xOrigin<-seq(extRaster[1],
                 windowSelect_X['n']*(windowSelect_X['pair.window']-windowSelect_X['pair.overlap'])+extRaster[1],
                 windowSelect_X['pair.window']-windowSelect_X['pair.overlap'])
    yOrigin<-seq(extRaster[3],
                 windowSelect_Y['n']*(windowSelect_Y['pair.window']-windowSelect_Y['pair.overlap'])+extRaster[3],
                 windowSelect_Y['pair.window']-windowSelect_Y['pair.overlap'])

    xyDF<-expand.grid(xOrigin,yOrigin)

    xyDF<-lapply(1:nrow(xyDF),function(i){
      x<-xyDF[i,]
      out<-(c(x,
              c(x[1],x[2]+windowSelect_Y['pair.window']),
              c(x[1]+windowSelect_X['pair.window'],x[2]+windowSelect_Y['pair.window']),
              c(x[1]+windowSelect_X['pair.window'],x[2]),
              x))
      out<-matrix(as.numeric(out),ncol=2,byrow=T)
      return(out)
    })
    #
    windowGrid<-lapply(xyDF,function(x){sf::st_polygon(list(x))})
    windowGrid<-sf::st_sfc(windowGrid)
    windowOverlap<-sf::st_intersection(sf::st_sf(windowGrid))
    ww<-sapply(windowOverlap$origins,function(x)ifelse(length(x)>1,T,F))
    ww1<-sapply(windowOverlap$windowGrid,function(x)ifelse(sf::st_geometry_type(x)=='POLYGON',T,F))
    windowOverlap<-windowOverlap[ww & ww1,]



    xOverlap<-c(rbind(xOrigin[-1],xOrigin[-length(xOrigin)]+windowSelect_X['pair.window']))
    yOverlap<-c(rbind(yOrigin[-1],yOrigin[-length(yOrigin)]+windowSelect_Y['pair.window']))

    xTrelis<-lapply(xOverlap,function(x){
      sf::st_linestring(rbind(c(x,extRaster[3]),c(x,extRaster[4])))})
    yTrelis<-lapply(yOverlap,function(y){
      sf::st_linestring(rbind(c(extRaster[1],y),c(extRaster[2],y)))})

    overlapTrelis<-sf::st_sfc(append(xTrelis,yTrelis))

    extentDF<-lapply(xyDF,function(x){
      raster::extent(c(min(x[,1]),max(x[,1]),min(x[,2]),max(x[,2])))
    })

    newSrt<-lapply(extentDF,function(ext){
      raster::crop(fn_srt,ext)
    })
  } else {
    newSrt<-list(fn_srt)
  }

  if (!is.null(fn_cores)) {
    cl <- parallel::makeCluster(fn_cores)
    if (fn_verbose) print(cl)
    parallel::clusterExport(cl,c(
      'newSrt',
      'pixelBrake',
      'fn_verbose',
      'maxii',
      'drct',
      'fn_lowerAreaLimit'),envir = environment())
    on.exit(expr = {
      if (exists('cl')) parallel::stopCluster(cl)
    })
  } else {
    cl<-NULL
  }


  if (fn_verbose) {
    if (is.null(cl)){
      pbapply::pboptions(type = 'none')
    } else {
      pbapply::pboptions(type = 'txt')
    }
  } else {
    pbapply::pboptions(type = 'none')
  }

  MULTIOUT<-pbapply::pblapply(cl=cl,seq_along(newSrt),function(iXSRT){

    raster::raster()
    XSRT<-newSrt[[iXSRT]]

    polyLayer<-lapply(seq_along(pixelBrake),function(ii){
      if (fn_verbose) cat('Layer : ',ii,' of ',maxii,'\r')


      clippingMap<-XSRT<=pixelBrake[ii]
      clippingMap<-raster::clump(x = clippingMap,
                                 directions = drct,
                                 gaps = F)
      newStars<-stars::st_as_stars(clippingMap)

      clippingPolygon<-sf::st_as_sf(newStars,merge=T)

      flNms<-raster::filename(clippingMap)

      if (flNms!="") {
        unlink(flNms)
      }

      rm (clippingMap)
      rm (newStars)

      #### try that
      if (any(duplicated(clippingPolygon$clumps))){

        clippingPolygon<-sf::st_buffer(clippingPolygon,0)
        clippingPolygon<-lapply((unique(clippingPolygon$clumps)),function(clmps){

          out<-sf::st_union(clippingPolygon[clippingPolygon$clumps==clmps,])
          out<-sf::st_sf(sf_column_name = 'geometry',
                         geometry = out,
                         clumps = clmps,
                         stringsAsFactors = F)

        })

        clippingPolygon<-do.call(dplyr::bind_rows,clippingPolygon)
      }
      colnames(clippingPolygon)[2]<-'geom'
      sf::st_geometry(clippingPolygon)<-'geom'
      clippingPolygon<-sf::st_make_valid(clippingPolygon)
      clippingPolygon<-sf::st_buffer(clippingPolygon,0)
      clippingPolygonArea<-sf::st_area(clippingPolygon)
      clippingPolygon<-dplyr::bind_cols(clippingPolygon,area=clippingPolygonArea)
      clippingPolygon<-clippingPolygon[clippingPolygon$area>fn_lowerAreaLimit,]
      return(clippingPolygon)
    })


    polyLayer_check<-sapply(polyLayer,function(x)ifelse(nrow(x)==0,F,T))
    polyLayer<-polyLayer[polyLayer_check]
    rm(polyLayer_check)

    if (length(polyLayer)==0){
      out<-sf::st_sf(uid = NA,
                     splitp_id = NA,
                     primary_id = NA,
                     tile_id = NA,
                     iland_id = NA,
                     clade_id = NA,
                     level_id = NA,
                     area=0,
                     geom = sf::st_sfc(sf::st_polygon()))[-1,]
      return(out)
    }

    if (fn_verbose) cat('Layer scan completed\n')
    if (fn_verbose) cat('Constructing network\n')

    maxii<-length(polyLayer)
    pL<-seq_along(polyLayer)[-1]

    if (length(pL)==0) {
      out<-sf::st_sf(uid = fn_uid,
                     splitp_id = NA,
                     primary_id = fn_primaryIndex,
                     tile_id = iXSRT,
                     iland_id = polyLayer[[1]]$clumps,
                     clade_id = polyLayer[[1]]$clumps,
                     level_id = 0,
                     area=0,
                     geom = polyLayer[[1]]$geom)
      return(out)
    }

    polyEdge<-lapply(pL,function(ii){
      if (fn_verbose) cat('Edges : ',ii,' of ',maxii,'\r')
      edgeDef<-sf::st_contains(polyLayer[[ii]],polyLayer[[ii-1]],prepared = F)
      maxiii<-length(edgeDef)
      if (maxiii>0) {
        edgeBrake<-lapply(1:length(edgeDef),function(iii){
          if (fn_verbose) cat('Edges : ',ii,' of ',maxii,'... element: ',iii, ' of ',maxiii,'\r')
          from<-paste0(ii,'.',iii)
          if (length(edgeDef[[iii]])>0){
            to<-paste0(ii-1,'.',edgeDef[[iii]])
          } else {

            to<-NA
          }
          out<-data.frame(from,to,stringsAsFactors = F)
        })

        out<-do.call(rbind,edgeBrake)
      } else {out<-NULL}
      return(out)
    })
    if (fn_verbose) cat('Edges : done...\n')

    polyEdge<-do.call(rbind,polyEdge)

    if (fn_verbose) cat('Making big network\n')

    nodeList<-unique(c(na.omit(polyEdge$from),na.omit(polyEdge$to)))
    nodeIndex<-strsplit(nodeList,'.',fixed=T)
    nodeIndex<-do.call(rbind,nodeIndex)
    nodeIndex<-matrix(as.numeric(nodeIndex),ncol = 2,byrow = F)
    nodeList<-nodeList[order(nodeIndex[,1],nodeIndex[,2])]
    nodeIndex<-nodeIndex[order(nodeIndex[,1],nodeIndex[,2]),]
    polyArea<-lapply(polyLayer,function(x) sf::st_drop_geometry(x))
    areaList<-apply(nodeIndex,1,function(idx){
      out<-polyArea[[idx[1]]][idx[2],'area',drop=T]
      if (!is.numeric(out)) out<-0
      return(out)
    })

    nodeDataFrame<-data.frame(ID=seq_along(nodeList),
                              layerIndex = nodeIndex[,1],
                              polyIndex = nodeIndex[,2],
                              area = areaList,
                              uid = NA,
                              splitp_id = NA,
                              primary_id = NA,
                              tile_id = NA,
                              iland_id = NA,
                              clade_id = NA,
                              level_id = NA,
                              splitP_area = NA,
                              visited = F,
                              stringsAsFactors = F)

    edgeList<-polyEdge[!is.na(polyEdge$to),]
    newEdgeList<-apply(edgeList,1,function(el){

      elFrom<-strsplit(el[1],'.',fixed=T)
      elTo<-strsplit(el[2],'.',fixed=T)
      outFrom<-nodeDataFrame$ID[nodeDataFrame$layerIndex==elFrom[[1]][1] &
                                  nodeDataFrame$polyIndex==elFrom[[1]][2]]
      outTo<-nodeDataFrame$ID[nodeDataFrame$layerIndex==elTo[[1]][1] &
                                nodeDataFrame$polyIndex==elTo[[1]][2]]
      out<-list(from=outFrom,to=outTo)
      return(out)
    })
    #
    newEdgeList<-matrix(unlist(newEdgeList),ncol=2,byrow=T)
    polyNet<-igraph::graph_from_data_frame(newEdgeList, directed=TRUE, vertices=nodeDataFrame)

    tailNodes<-igraph::V(polyNet)[igraph::degree(polyNet,mode = 'in')==0]$name
    headNodes<-igraph::V(polyNet)[igraph::degree(polyNet,mode = 'out')==0]$name

    allPath<-lapply(tailNodes,function(tn){
      allDist<-igraph::all_shortest_paths(polyNet,tn,headNodes)$res
      allDist<-lapply(allDist,length)
    })

    allPath<-sum(do.call(c,do.call(c,allPath)))

    targetSF<-sf::st_sf(uid = NA,
                        splitp_id = NA,
                        primary_id = NA,
                        tile_id = NA,
                        iland_id = NA,
                        clade_id = NA,
                        level_id = NA,
                        area=0,
                        geom = sf::st_sfc(lapply(1:allPath, function(x) sf::st_polygon())))



    if (fn_verbose) cat('Crawling the network\n')

    subgroups<-igraph::decompose.graph(polyNet,min.vertices = 1,mode = 'weak')

    maxii<-length(subgroups)

    targetSF_row<-1

    TEMP_LAYER<-length(polyLayer)+1
    polyLayer[[TEMP_LAYER]]<-sf::st_sfc()
    cycleIndex<-0

    for (ii in 1:maxii){

      if (fn_verbose) cat('scanning iland ',ii,' of ',maxii,'\n')

      sgrp<-subgroups[[ii]]

      sgrp_head<-igraph::V(sgrp)[igraph::degree(sgrp,mode='in')==0]$name

      NofOutEdges<-igraph::degree(sgrp,mode='out')

      cladeID<-1
      endOfGame<-F



      while(!endOfGame){

        cycleIndex<-cycleIndex+1
        TEMP_size<-igraph::gsize(sgrp)

        if (fn_verbose) cat('Cycle: ',formatC(cycleIndex,
                                              digits = 2,
                                              width = 6,
                                              flag = '#'),' / ',
                            'network size: ',formatC(TEMP_size,
                                                     digits = 2,
                                                     width = 6,
                                                     flag = '#'),' / ',
                            'single clades: ',formatC(cladeID-1,
                                                      digits = 2,
                                                      width = 6,
                                                      flag = '#'),'\r')

        if (TEMP_size==0) {

          seeds_newIndex<-1:length(NofOutEdges[NofOutEdges==0])
          seeds_names<-igraph::V(sgrp)[NofOutEdges==0]$name
          nodeID<-list(uid = fn_uid,
                       splitp_id = paste(fn_primaryIndex,
                                         iXSRT,
                                         ii,
                                         cladeID,
                                         '0',
                                         sep='.'),
                       primary_id = fn_primaryIndex,
                       tile_id = iXSRT,
                       iland_id = ii,
                       clade_id = cladeID,
                       level_id = 0)
          targetSF[targetSF_row,c('uid',
                                  'splitp_id',
                                  'primary_id',
                                  'tile_id',
                                  'iland_id',
                                  'clade_id',
                                  'level_id')]<-as.data.frame(nodeID)

          nodeLayerIndex<-igraph::get.vertex.attribute(sgrp,
                                                       'layerIndex',
                                                       seeds_names)
          nodePolyIndex<-igraph::get.vertex.attribute(sgrp,
                                                      'polyIndex',
                                                      seeds_names)

          targetSF$geom[targetSF_row]<-sf::st_geometry(polyLayer[[nodeLayerIndex]][nodePolyIndex,'geom'])
          targetSF_row<-targetSF_row+1
          break
        }
        # if (TEMP_size==47)

        seeds_newIndex<-1:length(NofOutEdges[NofOutEdges==0])
        seeds_names<-igraph::V(sgrp)[NofOutEdges==0]$name
        seeds_pathLength<-igraph::shortest.paths(sgrp,sgrp_head,seeds_names)

        # if (fn_verbose) cat('Iland ',ii,' of ',maxii,'... edges: ',NofEdges,'\r')
        rootNode<-colnames(seeds_pathLength[1,which.max(seeds_pathLength)[1],drop=F])
        parentNode<-igraph::incident(graph = sgrp,v = rootNode,mode = 'in')
        parentNode<-igraph::tail_of(graph = sgrp,es = parentNode)
        parentNode<-parentNode$name
        grandParentNode<-igraph::incident(graph = sgrp,v = parentNode,mode = 'in')
        grandParentNode<-igraph::tail_of(graph = sgrp,es = grandParentNode)
        grandParentNode<-grandParentNode$name
        if (length(grandParentNode)==0) endOfGame<-T
        childrenNodes<-igraph::incident(graph = sgrp,v = parentNode,mode = 'out')
        childrenNodes<-igraph::head_of(graph = sgrp,es = childrenNodes)
        childrenNodes<-childrenNodes$name
        childrenGraph<-igraph::make_ego_graph(graph = sgrp,
                                              order = 1,
                                              nodes = parentNode,
                                              mode = 'out')[[1]]
        newSize<-igraph::gsize(childrenGraph)
        if (newSize==1){
          nodeID<-igraph::get.vertex.attribute(sgrp,
                                               name = 'splitp_id',
                                               index = childrenNodes)
          if (is.na(nodeID)){
            nodeID<-list(uid = fn_uid,
                         splitp_id = paste(fn_primaryIndex,
                                           iXSRT,
                                           ii,
                                           cladeID,
                                           '0',
                                           sep='.'),
                         primary_id = fn_primaryIndex,
                         tile_id = iXSRT,
                         iland_id = ii,
                         clade_id = cladeID,
                         level_id = 0)

            cladeID<-cladeID+1

          } else {
            nodeID<-igraph::vertex_attr(graph = sgrp,
                                        index = childrenNodes)[
                                          c('uid',
                                            'splitp_id',
                                            'primary_id',
                                            'tile_id',
                                            'iland_id',
                                            'clade_id',
                                            'level_id')]

          }

          targetSF[targetSF_row,c('uid',
                                  'splitp_id',
                                  'primary_id',
                                  'tile_id',
                                  'iland_id',
                                  'clade_id',
                                  'level_id')]<-as.data.frame(nodeID)

          nodeLayerIndex<-igraph::get.vertex.attribute(sgrp,
                                                       'layerIndex',
                                                       childrenNodes)
          nodePolyIndex<-igraph::get.vertex.attribute(sgrp,
                                                      'polyIndex',
                                                      childrenNodes)

          targetSF$geom[targetSF_row]<-sf::st_geometry(polyLayer[[nodeLayerIndex]][nodePolyIndex,'geom'])
          targetSF_row<-targetSF_row+1

          parentNodeID<-nodeID
          parentNodeID$level_id<-parentNodeID$level_id+1
          parentNodeID$splitp_id<-paste0(unlist(parentNodeID[-c(1,2)]),collapse='.')

          for (attrI in names(parentNodeID)){
            sgrp<-igraph::set.vertex.attribute(graph = sgrp,
                                               name = attrI,
                                               index = parentNode,
                                               value = parentNodeID[[attrI]])}

          sgrp<-igraph::delete_vertices(sgrp,childrenNodes)
          # NofOutEdges<-igraph::degree(sgrp,mode='out')
          # sgrp_head<-igraph::V(sgrp)[igraph::degree(sgrp,mode='in')==0]
        } else {


          gdf<-igraph::as_long_data_frame(childrenGraph)
          prnt_LI<-gdf$from_layerIndex[1]
          prnt_PI<-gdf$from_polyIndex[1]
          chldrn_LI<-gdf$to_layerIndex
          chldrn_PI<-gdf$to_polyIndex

          prnt_poly<-polyLayer[[prnt_LI]][prnt_PI,]
          chldrn_poly<-lapply(1:length(chldrn_LI),function(xi){
            polyLayer[[chldrn_LI[xi]]][chldrn_PI[xi],]
          })
          chldrn_poly<-do.call(dplyr::bind_rows,chldrn_poly)
          prnt_bBox<-sf::st_bbox(prnt_poly)
          prnt_tassel<-sf::st_make_grid(x = prnt_poly,
                                        cellsize = 1,
                                        what = 'polygons',
                                        square = T,
                                        crs = 'NA')
          prnt_tassel<-prnt_tassel[sf::st_contains(prnt_poly,prnt_tassel)[[1]]]

          chldrn_hit<-sf::st_contains(chldrn_poly,prnt_tassel)
          chldrn_hit<-do.call(c,chldrn_hit)
          prnt_tassel<-prnt_tassel[-chldrn_hit]
          dist_tassel<-sf::st_distance(prnt_tassel,chldrn_poly)
          aggr_tassel<-apply(dist_tassel,1,which.min)
          chlP<-sf::st_geometry(chldrn_poly)
          newPoly<-lapply(1:ncol(dist_tassel),function(xii){

            if (length(prnt_tassel[aggr_tassel==xii])!=0){
              out<-sf::st_union(c(prnt_tassel[aggr_tassel==xii],chlP[xii]))
              out<-sf::st_buffer(out,0)
              out<-sf::st_make_valid(out)
              # if (sf::st_geometry_type(out)=='MULTIPOLYGON'){
              #   out<-sf::st_cast(out,'POLYGON')
              #   area_out<-sf::st_area(out)
              #   out<-out[which.max(area_out)]
              # }
            } else {
              out<-chldrn_poly[xii,'geom'][[1]]

            }
            return(out)
          })

          newPolyDF<-sf::st_sf(geom=do.call(c,newPoly))
          newPolyDF<-dplyr::bind_cols(newPolyDF,
                                      data.frame(clumps=NA,
                                                 area=sf::st_area(newPolyDF)))

          oldRow<-nrow(polyLayer[[TEMP_LAYER]])
          if (is.null(oldRow)) oldRow<-0

          polyLayer[[TEMP_LAYER]]<-rbind.data.frame(polyLayer[[TEMP_LAYER]],
                                                    newPolyDF)

          if (!endOfGame) {
            newRow<-nrow(polyLayer[[TEMP_LAYER]])

            newRowIndex<-(oldRow+1):newRow

            lastVertex<-igraph::V(sgrp)
            lastVertex<-lastVertex[[length(lastVertex)]]$name
            newVertex<-as.character(as.numeric(lastVertex)+(1:length(childrenNodes)))
            sgrp<-sgrp+igraph::vertex(name=newVertex,
                                      layerIndex = rep(TEMP_LAYER,length(childrenNodes)),
                                      polyIndex = newRowIndex,
                                      area = sf::st_drop_geometry(polyLayer[[TEMP_LAYER]][newRowIndex,'area']),
                                      uid = NA,
                                      splitp_id = NA,
                                      primary_id = NA,
                                      tile_id = NA,
                                      iland_id = NA,
                                      clade_id = NA,
                                      level_id = NA,
                                      splitP_area = NA,
                                      visited = F)


            newEdges<-matrix(c(rep(grandParentNode,length(newVertex)),
                               newVertex,

                               newVertex,
                               childrenNodes),ncol=2,byrow = F)
            newEdges<-as.vector(t(newEdges))

            sgrp<-igraph::add.edges(sgrp,newEdges)

            sgrp<-igraph::delete.vertices(sgrp,parentNode)

          } else {

            childern_nodeID<-igraph::vertex_attr(graph = sgrp,
                                                 index = childrenNodes)[
                                                   c('uid',
                                                     'splitp_id',
                                                     'primary_id',
                                                     'tile_id',
                                                     'iland_id',
                                                     'clade_id',
                                                     'level_id')]
            children_polyID<-igraph::vertex_attr(graph = sgrp,
                                                 index = childrenNodes)[
                                                   c('layerIndex',
                                                     'polyIndex')]
            children_polyID<-as.data.frame(children_polyID)
            children_poly<-apply(children_polyID,1,function(nip){
              polyLayer[[nip[1]]][nip[2],'geom']
            })
            children_poly<-do.call(rbind.data.frame,children_poly)
            children_polyFrame<-cbind.data.frame(as.data.frame(childern_nodeID,stringsAsFactors = F),area=0)
            # newNodeID<-dplyr::bind_cols(nodeID_poly,nodeID_DF)
            children_SF<-dplyr::bind_cols(children_poly,children_polyFrame)
            targetSF[targetSF_row:(targetSF_row+nrow(children_SF)-1),]<-children_SF


            targetSF_row<-targetSF_row+nrow(children_SF)

            parent_polyFrame<-children_polyFrame
            # newPolyID_DF<-na.omit(newPolyID_DF)
            parent_polyFrame$level_id<-parent_polyFrame$level_id+1
            parent_polyFrame$splitp_id<-apply(parent_polyFrame,1,
                                              function(x){
                                                paste(x['primary_id'],
                                                      as.numeric(x['tile_id']),
                                                      as.numeric(x['iland_id']),
                                                      as.numeric(x['clade_id']),
                                                      as.numeric(x['level_id']),
                                                      sep = '.')})
            parent_SF<-dplyr::bind_cols(sf::st_sf(geom=do.call(c,newPoly)),
                                        parent_polyFrame)

            targetSF[targetSF_row:(targetSF_row+nrow(parent_SF)-1),]<-parent_SF

            targetSF_row<-targetSF_row+nrow(parent_SF)


            break
          }
        }

        NofOutEdges<-igraph::degree(sgrp,mode='out')
        sgrp_head<-igraph::V(sgrp)[igraph::degree(sgrp,mode='in')==0]
      }

      if (fn_verbose) cat('Cycle: ',formatC(cycleIndex,
                                            digits = 2,
                                            width = 6,
                                            flag = '#'),' / ',
                          'network size: ',formatC(TEMP_size,
                                                   digits = 2,
                                                   width = 6,
                                                   flag = '#'),' / ',
                          'single clades: ',formatC(cladeID-1,
                                                    digits = 2,
                                                    width = 6,
                                                    flag = '#'),'\n')


    }

    targetSF<-na.omit(targetSF)
    return(targetSF)
  })

  flNms<-lapply(newSrt,function(x){
    flNms<-raster::filename(x)
    if (flNms!='') unlink(flNms)
  })

  rm(newSrt)

  if (!is.null(cl)) {
    parallel::stopCluster(cl)
    rm(cl)}

  if (length(MULTIOUT)==1) {
    out<-MULTIOUT[[1]]
    gM<-sf::st_drop_geometry(out[,c('iland_id','clade_id','level_id')])
    gM<-order(gM$iland_id,gM$clade_id,gM$level_id)
    out<-out[gM,]
    return(out)}

  MULTIOUT_TOP<-lapply(MULTIOUT,function(MO){
    if (nrow(MO)==0) {
      out<-sf::st_sf(uid = NA,
                     splitp_id = NA,
                     primary_id = NA,
                     tile_id = NA,
                     iland_id = NA,
                     clade_id = NA,
                     level_id = NA,
                     area=0,
                     geom = sf::st_sfc(sf::st_polygon()))[-1,]
      return(out)
    }
    topLayer<-aggregate(level_id~iland_id+clade_id,sf::st_drop_geometry(MO),max)
    out<-apply(topLayer,1,function(dd){
      MO[MO$level_id == dd[3] &
           MO$iland_id == dd[1] &
           MO$clade_id == dd[2],]
    })
    out<-do.call(dplyr::bind_rows,out)
    return(out)
  })


  MULTIOUT_TOP<-lapply(seq_along(MULTIOUT_TOP),function(MTO){

    if (nrow(MULTIOUT_TOP[[MTO]])==0){
      out<-sf::st_sf(uid=NA,
                     splitp_id = NA,
                     primary_id = NA,
                     tile_id = NA,
                     iland_id = NA,
                     clade_id = NA,
                     level_id = NA,
                     area=0,
                     geom = sf::st_sfc(sf::st_polygon()))[-1,]
      return(out)
    }
    newIntersect<-sf::st_intersects(overlapTrelis,windowGrid[[MTO]])
    newIntersect<-sapply(newIntersect,function(x)if(length(x)!=0) T else F)
    newBorders<-sf::st_relate(overlapTrelis,windowGrid[[MTO]])
    newBorders<-grepl('F11F00212',newBorders,fixed = T) | grepl('F11FF0212',newBorders,fixed = T)
    newBorders<-newBorders[newIntersect]
    newBorders<-sf::st_intersection(overlapTrelis,windowGrid[[MTO]])[newBorders]
    crossers<-sf::st_intersects(newBorders,MULTIOUT_TOP[[MTO]])
    crossers<-do.call(c,crossers)
    if (!is.null(crossers)){
      if (length(crossers)!=0){
        out<-MULTIOUT_TOP[[MTO]][-crossers,]
        return(out)
      }
    }
    out<-MULTIOUT_TOP[[MTO]]
    return(out)
  })


  wo<-sapply(MULTIOUT_TOP,function(x)ifelse(nrow(x)==0,F,T),simplify = T,USE.NAMES = F)

  if (!any(wo)){
    org<-raster::extent(fn_srt)
    out<-sf::st_sf(uid=fn_uid,
              splitp_id = paste0(fn_primaryIndex,'.0.0.0.0'),
              primary_id = fn_primaryIndex,
              tile_id = 0,
              iland_id = 0,
              clade_id = 0,
              level_id = 0,
              area=0,
              geom = sf::st_sfc(sf::st_polygon(list(matrix(c(org[1],org[3],
                                                      org[1]+1,org[3],
                                                      org[1]+1,org[3]+1,
                                                      org[1],org[3]+1,
                                                      org[1],org[3]),
                                               ncol=2,
                                               byrow = T)))))

    sf::write_sf(out,file.path(fn_TempFile,paste0('TEMP_POLY_',fn_uid,'_',fn_primaryIndex,'.sqlite')),append = F)
    return(out)
  }

  MULTIOUT_TOP<-MULTIOUT_TOP[wo]

  MULTIOUT_TOP<-do.call(dplyr::bind_rows,MULTIOUT_TOP)

  MULTIOUT_TOP_Match<-sf::st_contains(MULTIOUT_TOP)

  PM_polyIN<-unique(unlist(lapply(MULTIOUT_TOP_Match,'[',1),recursive = T))
  #
  #   PM_polyIN<-unique(unlist(lapply(MULTIOUT_TOP_Match,function(x){
  #     area<-sf::st_area(MULTIOUT_TOP[x,])
  #     warea<-which.max(area)[1]
  #     x[warea]
  #   }),recursive = T))


  PM_polyOUT<-unique(unlist(lapply(MULTIOUT_TOP_Match,'[',-1),recursive = T))
  PM_polyIN<-PM_polyIN[!(PM_polyIN %in% PM_polyOUT)]

  IndexDF<-sf::st_drop_geometry(MULTIOUT_TOP)

  if (!is.null(PM_polyIN)) {
    if (length(PM_polyIN)!=0){
      IndexDF<-IndexDF[PM_polyIN,]
    }
  }

  wo<-sapply(MULTIOUT,function(x)ifelse(nrow(x)==0,F,T),simplify = T,USE.NAMES = F)

  MULTIOUT<-MULTIOUT[wo]

  MULTIOUT<-do.call(dplyr::bind_rows,MULTIOUT)

  OUT<-dplyr::semi_join(MULTIOUT,
                        IndexDF,
                        by = c('tile_id',
                               'iland_id',
                               'clade_id'),
                        copy=F)

  OUT<-OUT[order(OUT$tile_id,OUT$iland_id,OUT$clade_id,OUT$level_id,decreasing = T),]

  sf::write_sf(OUT,file.path(fn_TempFile,paste0('TEMP_POLY_',fn_uid,'_',fn_primaryIndex,'.sqlite')),append = F)
  return(OUT)
}





.bestWindow<-function(fn_windowW=80:120,
                      fn_overlapW=25:50,
                      fn_totalW=1000){
  windowMatrix<-expand.grid(window=fn_windowW,overlap=fn_overlapW)
  eachCombination<-apply(windowMatrix,1,function(x){
    n<-floor((fn_totalW-x['overlap'])/(x['window']-x['overlap']))
    b<-abs(((n*x['window']-(n-1)*x['overlap'])-fn_totalW))
    list(dev=sum(abs(x[1]-median(fn_windowW)),abs(x[2]-median(fn_overlapW)),abs(x[1]-b)),
         n=unname(n),
         overhead=unname(b),
         pair = x)
  })
  ww<-which.min(unlist(lapply(eachCombination,function(x)x$dev)))
  return(unlist(eachCombination[ww]))
}

.topLayer<-function(x,uid=NULL){
  if (is.null(uid)) stop(mError("no uid specified"),call. = F)
  wx<-sf::st_drop_geometry(x)
  wx<-wx[,'uid']==uid
  x<-x[wx,]
  topLayer<-aggregate(level_id~primary_id+tile_id+iland_id+clade_id,sf::st_drop_geometry(x),max)
  out<-dplyr::semi_join(x,topLayer,
                        by=c(colnames(topLayer)))
  return(out)
}

.bottomLayer<-function(x,uid=NULL){
  if (is.null(uid)) stop(mError("no uid specified"),call. = F)
  wx<-sf::st_drop_geometry(x)
  wx<-wx[,'uid']==uid
  x<-x[wx,]
  bottomLayer<-aggregate(level_id~primary_id+tile_id+iland_id+clade_id,sf::st_drop_geometry(x),min)
  out<-dplyr::semi_join(x,bottomLayer,
                        by=c(colnames(topLayer)))
  return(out)
}
