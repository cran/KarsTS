compareVecVec <-
function(VA, VB) {
  comparaEleVec <- function(ele, vec) {
    res <- as.matrix(ele == vec)
    res
  }
  res1 = sapply(VA, comparaEleVec, vec = VB)
  if (is.matrix(res1) == FALSE) {
    res1 <- as.matrix(res1)
  }
  colnames(res1) <- VA
  rownames(res1) <- VB
  res1
}
