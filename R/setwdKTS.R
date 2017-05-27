setwdKTS <-
function() {
  dirPath <- tcltk::tk_choose.dir()
  if (is.na(dirPath) == FALSE) {
    setwd(dirPath)
  }
}
