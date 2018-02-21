testMatrix <- matrix(round(c(runif(10000, 0, 1), runif(10000, 0, 1)), 2),nrow = 50, ncol = 25)
colnames(testMatrix) = paste(1:25)


heatMap <- function(matrx, kmeans){
  hc.rows <- hclust(dist(matrx))
  hc.cols <- hclust(dist(t(matrx)))
  HM <- heatmap(testMatrix, Colv = F, Rowv = F, main = "Original Heatmap")
  HMRow <- heatmap(testMatrix[cutree(hc.rows, k = kmeans),], Colv = as.dendrogram(hc.cols), scale = "row", keep.dendro = FALSE,main = "Ordered by Rows")
  HMCol <- heatmap(testMatrix[,cutree(hc.cols, k = kmeans)], Rowv = as.dendrogram(hc.rows), scale = "column", main = "Ordered by Columns")
  return(c(HM, HMRow, HMCol))

}

heatMap(testMatrix, 2)







---------------------------##ROUGH DRAFT##-------------------------
hc.rows <- hclust(dist(testMatrix))
hc.cols <- hclust(dist(t(testMatrix)))
heatmap(scale(testMatrix)[cutree(hc.rows,k=2)== 2,], Colv=as.dendrogram(hc.cols), scale='none', keep.dendro = FALSE)
heatmap(scale(testMatrix)[cutree(hc.rows,k=4),], Colv=as.dendrogram(hc.cols), scale='column')
table(cutree(hc.rows, k=6))
