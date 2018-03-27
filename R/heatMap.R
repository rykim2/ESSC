testMatrix <- matrix(round(c(runif(10000, 0, 1), runif(10000, 0, 1)), 2),nrow = 50, ncol = 25)
colnames(testMatrix) = paste(1:25)


heatMap <- function(matrx, kmeans){
  library(gplots)
  library(dendextend)
  hc.rows <- as.dendrogram(hclust(dist(matrx)))
  hc.cols <- as.dendrogram(hclust(dist(t(matrx))))
  denRow <- color_branches(hc.rows, heat.colors(299), k = kmeans)
  denCol <- color_branches(hc.cols, heat.colors(299), k = kmeans)
  HM <- heatmap.2(matrx, main = "Original Heatmap", col = topo.colors(dim(matrx)[2]), 
                  trace = "none", density.info = "none", dendrogram = "none", margins = c(5,5))
  HMRow <- heatmap.2(matrx[cutree(hc.rows, k = kmeans),], trace = "none", Rowv = denRow, 
                     main = "Ordered by Rows", col = topo.colors(dim(matrx)[2]), dendrogram = "row")
  HMCol <- heatmap.2(matrx[,cutree(hc.cols, k = kmeans)], Colv = denCol, dendrogram = "column",
                    main = "Ordered by Columns",col = topo.colors(dim(matrx)[2]), trace = "none")
  par(mfrow = c(3,1))

  return(c(HM, HMRow, HMCol))

}

heatMap(results.pois$PValues, 3)







# -------------------------------ROUGH DRAFT---------------------------------
testMatrix <- results.pois$PValues
hc.rows <- as.dendrogram(hclust(dist(testMatrix)))
hc.cols <- as.dendrogram(hclust(dist(t(testMatrix))))
denRow <- color_branches(hc.rows, heat.colors(299), k = 10)
denCol <- color_branches(hc.cols, heat.colors(299), k = 3)
HMCol <- heatmap.2(testMatrix[,cutree(hc.cols, k = 3)], Colv = denCol, dendrogram = "column",
                   main = "Ordered by Columns",col = topo.colors(dim(testMatrix)[2]), trace = "none")

hc.rows <- hclust(dist(testMatrix))
hc.cols <- hclust(dist(t(testMatrix)))
heatmap.2(testMatrix, main = "Original Heatmap",  ColSideColors = rainbow(ncol(testMatrix)), RowSideColors = cc, col = c("blue", "green"), trace = "none")
heatmap(scale(testMatrix)[cutree(hc.rows,k=2)== 2,], Colv=as.dendrogram(hc.cols), scale='none', keep.dendro = FALSE)
heatmap(scale(testMatrix)[cutree(hc.rows,k=4),], Colv=as.dendrogram(hc.cols), scale='column')
table(cutree(hc.rows, k=6))
