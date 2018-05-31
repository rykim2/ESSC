# Y label inputs. Graph Heatmap by levels

heatMap2 <- function(matrx, kRow, y = NULL){
  library(ComplexHeatmap)
  library(dendextend)
  library(circlize)
  library(colorRamps)
  library(grid)
  library(stats)
  
  myPalt <- structure(c("purple", "orange"), names = c("0", "1"))

  if(is.null(y) == TRUE){
    
    clust.rows <- hclust(dist(matrx))
    hc.rows <- as.dendrogram(clust.rows)
    
    denRow <- color_branches(hc.rows, heat.colors(299), k = kRow)
    
    HMRow <- Heatmap(matrx, name = "Matrix", col = myPalt, row_dend_reorder = TRUE, 
                     show_column_names = FALSE,
                     show_column_dend = FALSE,
                     show_heatmap_legend = TRUE,
                     rect_gp = gpar(col = "white", lwd = 0.5),
                     cluster_rows = denRow)
    
    draw(HMRow, newpage = TRUE)
    
  }
  
  else{
    try(expr = if(length(y) != nrow(matrx)) stop("length of response vector y must be the same as the number of observations", call. = FALSE))
    
    if(length(y) == nrow(matrx)){
      ## Associating response y and reordering accordingly  ##
      y <- as.factor(y)
      y <- sort(y, levels = y)
      lev <- length(levels(y))
      
      ## HEATMAP ANNOTATION : creating a named- vector
      colrs <- palette(matlab.like(n = lev))
      named.vec <- c(structure(colrs, names=levels(y)))
      
      ha <- rowAnnotation(df = data.frame(labs = y), name = "truth",
                              width = unit(0.5,"cm"), show_legend = TRUE, 
                              col = list(labs = c(named.vec)))
      
      ## starting HeatMap function
      # making a dendextend object to color my dendogram branches
      clust.rows <- hclust(dist(matrx))

      hc.rows <- as.dendrogram(clust.rows)
      denRow <- color_branches(hc.rows, heat.colors(299), k = kRow)
      
      HM <- Heatmap(matrix = matrx, name = "Matrix", col = myPalt, row_dend_reorder = FALSE, show_column_names = FALSE,
                       show_column_dend = FALSE, rect_gp = gpar(col = "white", lwd = 0.5),
                       cluster_rows = denRow, gap = unit(2, "mm"))
      
      draw(ha + HM, newpage = TRUE)
      return(list(Node_Clusters = clust.rows))

    }
  }
}

heatMap2(matrx = matrx2, y = y, kRow = 8)

##TESTING MY FUNCTION
y <- FB_labels$Location
matrx <- testMatrix.FB

# new matrix sorted with 1 or 0
y <- as.factor(y)
y <- sort(y, levels = y)
matrx2 <- matrx[sort(y),]
