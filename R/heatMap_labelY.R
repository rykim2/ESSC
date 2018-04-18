# Y label inputs. Graph Heatmap by levels

heatMap2 <- function(matrx, kRow, y = NULL){
  library(ComplexHeatmap)
  library(dendextend)
  library(circlize)
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
                     cluster_rows = denRow)
    
    draw(HMRow, newpage = TRUE)
    
  }
  
  else{
    try(expr = if(length(y) != nrow(matrx)) stop("length of response vector y must be the same as the number of observations", call. = FALSE))
    
    if(length(y) == nrow(matrx)){
      print(paste("The length of the response:", length(y), "is the same as the length of matrx:", nrow(matrx), sep = ' '))
  
      ## Associating response y and reordering accordingly  ##
      truth <- cbind(matrx, y)
      colnames(truth) <- c(1:ncol(matrx), "labels")
      y <- truth[, "labels"]
      y <- as.factor(y)
      lev <- length(levels(y))
      truth <- truth[order(y),]
      matrx <- truth[,-ncol(truth)] ## reordered new matrx
      
      ## HEATMAP ANNOTATION : creating a named- vector
      colrs <- palette(cm.colors(n = lev))
      named.vec <- c(structure(colrs, names=levels(y)))
      
      ha <- HeatmapAnnotation(df = data.frame(labs = y), name = "truth", which = "row",
                              width = unit(0.5,"cm"), show_legend = TRUE, 
                              col = list(labs = c(named.vec)))
      
      ## starting HeatMap function
      # making a dendextend object to color my dendogram branches
      clust.rows <- hclust(dist(matrx))

      hc.rows <- as.dendrogram(clust.rows)
      denRow <- color_branches(hc.rows, heat.colors(299), k = kRow)

      # HMRow <- Heatmap(matrx, column_title = "Row Heatmap", col = myPalt, row_dend_reorder = TRUE, show_column_names = FALSE,
      #                  show_column_dend = FALSE, cluster_rows = denRow, show_heatmap_legend = FALSE)
      # 
      HMRow <- Heatmap(matrx, name = "Matrix", col = myPalt, show_column_names = FALSE,
                       show_column_dend = FALSE, show_row_dend = TRUE, km = lev, km_title = "%i",
                       show_heatmap_legend = TRUE)
      # HMRow <- Heatmap(matrx, name = "Matrix", col = myPalt, row_dend_reorder = TRUE, show_column_names = FALSE,
      #                  show_column_dend = FALSE,
      #                  cluster_rows = denRow, split = lev, gap = unit(2, "mm"))
    
      
      draw(HMRow + ha, newpage = TRUE)
      return(list(Node_Clusters = clust.rows))

    }
  }
}

heatMap2(matrx = testMatrix.FB, kRow = 4)

##TESTING MY FUNCTION
y <- FB_labels$Location
matrx <- testMatrix.FB

y <- as.factor(y)
y <- sort(y, levels = y)

colrs <- rainbow(lev)
named.vec <- c(structure(colrs, names=levels(y)))


