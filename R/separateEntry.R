separateEntry <-
function(y, class1 = verifyIntEntry, 
                          class2 = verifyCharEntry, noValid = NA) {
  result <- c(noValid, noValid)
  ySplit <- strsplit(y, split = NULL)[[1]]
  if (any(ySplit == ",")) {
    firstComma <- min(which(ySplit == ","))
    if (is.finite(firstComma)) {
      lYSplit <- nchar(y)
      part1 <- substr(y, 1, firstComma - 1)
      part2 <- substr(y, firstComma + 1, lYSplit)
      part1 <- class1(part1, noValid = noValid)
      part2 <- class2(part2, noValid = noValid)
      result <- c(part1, part2)
    }
  }
  result
}
