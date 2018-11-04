showHelp <-
function() {
  pathToImage <- paste0(system.file(package = "KarsTS"), 
                        "/extdata/", "KarsTSHelp.txt")
  tcltk::tkpager(file = pathToImage, header = "HELP", 
                 title = "KarsTS", delete.file = FALSE)
}
