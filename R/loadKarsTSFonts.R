loadKarsTSFonts <-
function() {
    fontmainbt <- tcltk::tkfont.create(family = "Arial", 
                                       size = 14, weight = "bold")
    fontsubbt <- tcltk::tkfont.create(family = "Arial", 
                                      size = 12, weight = "bold")
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
    KarsTSFonts <- list(mainBt = fontmainbt, subBt = fontsubbt, 
                        explain = fontaclara, T0 = fonttitle0pan, 
                        T1 = fonttitle1pan, T2 = fonttitle2pan, 
                        normal = fontnormpan)
    assign("KTSFonts", KarsTSFonts, envir = KTSEnv)
    
}
