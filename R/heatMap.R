#Download ComplexHeatmap by:
# library(devtools)
# install_github("jokergoo/ComplexHeatmap")

#Set up: Load Matrix from ESSC 
library(ESSC)
net2 <- stochastic.block(n = 1000, k = 3, P = cbind(c(0.1, 0.01, 0.01), c(0.01, 0.1, 0.01), c(0.01, 0.01, 0.1)), sizes = c(300, 300, 400), random.community.assignment = TRUE)
results.pois <- essc(net2$Adjacency, alpha = 0.10, Null = "Poisson")
testMatrix <- results.pois$PValues

#-----------------------------------Heatmap function-----------------------------#

heatMap <- function(matrx, kRow, kCol){
  library(ComplexHeatmap)
  library(dendextend)
  library(circlize)
  library(grid)
  
  # making a dendextend object to color my dendogram branches
  hc.rows <- as.dendrogram(hclust(dist(matrx)))
  hc.cols <- as.dendrogram(hclust(dist(t(matrx))))
  denRow <- color_branches(hc.rows, heat.colors(299), k = kRow)
  denCol <- color_branches(hc.cols, heat.colors(299), k = kCol)
  
  # Creating my ColorPalette and Lengend for Matrix Values
  myPalt <- colorRamp2(c(0, 0.5, 1), c("lightyellow", "pink", "orange"))
  lgd = Legend(at = c( 0, 0.5, 1), col_fun = myPalt, title = "Matrix", title_position = "topleft", grid_height = unit(10, "mm"))
  
  
  # uses the Complex Heatmap object to create graphical objects --> GROB
  HM <- Heatmap(matrx, column_title = "Original Heatmap", col = myPalt, 
                show_row_dend = FALSE, show_column_dend = FALSE, show_heatmap_legend = FALSE)
  
  HMRow <- Heatmap(matrx, column_title = "Row Heatmap", col = myPalt, 
                   show_column_dend = FALSE, cluster_rows = denRow, show_heatmap_legend = FALSE)
  
  HMCol <- Heatmap(matrx, column_title = "Column Heatmap", col = myPalt, 
                   show_row_dend = FALSE, cluster_columns = denCol, show_heatmap_legend = FALSE)

#------------------------------drawing Heatmap------------------------#
grid.newpage()
pushViewport(viewport(width = 0.9, height = 0.9, name = "base"))

seekViewport("base")
pushViewport(viewport(width = 0.5, height = 0.5, just = c("right", "bottom"), name = "A"))
draw(HM, newpage = FALSE)

seekViewport("base")
pushViewport(viewport(width = 0.5, height = 0.5, just = c("left", "bottom"), name = "B"))
draw(HMRow, newpage = FALSE)

seekViewport("base")
pushViewport(viewport(width = 0.5, height = 0.5, just = c("right", "top"), name = "C"))
draw(HMCol, newpage = FALSE)

seekViewport("base")
pushViewport(viewport(width = 0.5, height=0.5, just = c("left", "top"), name = "D"))
grid.draw(lgd)

grab <- grid.grab()

return(grid.draw(grab))

}

heatMap(testMatrix, kRow = 500, kCol = 3)

