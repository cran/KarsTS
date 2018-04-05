aboutKTS <-
function() {
  pathToImage <- paste0(system.file(package = "KarsTS"), 
                        "/extdata", "aboutKTS.txt")
  tcltk::tkpager(file = pathToImage, header = "US", 
                 title = "ABOUT", delete.file = FALSE)
}
