loadKarsTSFonts <-
function() {
  
    fontmainbt <- tcltk::tkfont.create(family = "Arial", 
                                       size = 14, weight = "bold")
    fontsubbt <- tcltk::tkfont.create(family = "Arial", 
                                      size = 12, weight = "bold")
    fontsubbt1 <- tcltk::tkfont.create(family = "Arial", 
                                      size = 12, weight = "bold",
                                      underline = TRUE)
    fonttitle0pan <- tcltk::tkfont.create(family = "Arial", 
                                          size = 11, weight = "bold", 
                                          underline = TRUE)
    fonttitle1pan <- tcltk::tkfont.create(family = "Arial", 
                                          size = 11, weight = "bold")
    fonttitle2pan <- tcltk::tkfont.create(family = "Arial", 
                                          size = 11, slant = "italic")
    fontnormpan <- tcltk::tkfont.create(family = "Arial", size = 11)
    fontaclara <- tcltk::tkfont.create(family = "Arial", 
                                       size = 11, slant = "italic")
    KTSEnv$KTSFonts <- list(mainBt = fontmainbt, subBt = fontsubbt,
                            subBt1 = fontsubbt1,
                            explain = fontaclara, T0 = fonttitle0pan, 
                            T1 = fonttitle1pan, T2 = fonttitle2pan, 
                            normal = fontnormpan)
 
}
