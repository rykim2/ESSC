#' A personal Facebook edgelist displaying the relationship between two nodes
#'
#' A dataset containing the vertices and edges between two nodes
#'
#' @format A data frame with 16750 rows and 3 variables:
#' \describe{
#'   \item{Vertex.1}{the vertex of one node, undirected}
#'   \item{Vertex.2}{the vertex of another node, undirected}
#'   \item{Number.edges}{The number of edges from one edge to another}
#' }
"FB_edgelist"

#' The labels indicating the type of association
#'
#' A dataset containing the type of relationship between an individual and James Wilson
#'
#' @format A data frame with 561 rows and 2 variables:
#' \describe{
#'   \item{Individual}{the individual from Facebook}
#'   \item{Location}{what association the individual has with James.
#'   1 meaning an associate from work. 2 meaning an associate from college. etc}
#' }
"FB_labels"
