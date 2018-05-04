# Colour palettes below should follow the latest surveillance guidelines; 
# So far greens, blues, reds, grays, and qualitative colours - typos possible, all combinations not tested

# Function defaults to green scale, returns mostly a list (for gray/qualitive colours a simple vector)

survcolors <- function(x="greens"){

rgb2 <- function(r=x, g=NULL, b=NULL, maxColorValue = 255){
  val <- rgb(r, g, b, maxColorValue = maxColorValue)
  return(val)}

if(x=="greens"){
# greens
  green <- rgb2(101,179,46)
  gscale2 <- c(rgb2(32,119,50), 
               rgb2(178,207,110)) 
  gscale3 <- c(rgb2(26,110,49), 
               rgb2(101,179,46),
               rgb2(201,217,113)) 
  gscale4 <- c(rgb2(26,110,49), 
               rgb2(40,147,55),
               rgb2(156,198,90), 
               rgb2(201,217,113))
  gscale5 <- c(rgb2(12,72,40), 
               rgb2(32,129,53),
               rgb2(101,179,46), 
               rgb2(178,207,110), 
               rgb2(231,231,185)) 
  gscale6 <- c(rgb2(12,72,40), 
               rgb2(32,119,50), 
               rgb2(66,158,53),
               rgb2(137,190,71), 
               rgb2(192,212,122), 
               rgb2(231,231,185)) 
  gscale7 <- c(rgb2(12,72,40), 
               rgb2(26,110,49), 
               rgb2(40,147,55),
               rgb2(101,179,46), 
               rgb2(156,198,90), 
               rgb2(201,217,113), 
               rgb2(231,231,185))
  cols <- list(green, gscale2, gscale3, gscale4, gscale5, gscale6, gscale7)
  }else if(x=="blues"){  
# blues
  blue <- rgb2(124,189,196)
  bscale2 <- c(rgb2(60,142,162), 
               rgb2(173,210,221)) 
  bscale3 <- c(rgb2(26,107,133), 
               rgb2(124,189,196),
               rgb2(194,218,232)) 
  bscale4 <- c(rgb2(26,107,133), 
               rgb2(73,153,171),
               rgb2(165,206,215), 
               rgb2(194,218,232))
  bscale5 <- c(rgb2(0,60,80), 
               rgb2(60,142,162),
               rgb2(124,189,196), 
               rgb2(173,210,221),
               rgb2(227,232,240))
  bscale6 <- c(rgb2(0,60,80), 
               rgb2(39,117,142),
               rgb2(95,167,181), 
               rgb2(147,199,207), 
               rgb2(187,216,229), 
               rgb2(227,232,240))
  bscale7 <- c(rgb2(0,60,80), 
               rgb2(26,107,133), 
               rgb2(73,153,171), 
               rgb2(124,189,196),
               rgb2(165,206,215), 
               rgb2(194,218,232),
               rgb2(227,232,240))
  cols <- list(blue, bscale2, bscale3, bscale4, bscale5, bscale6, bscale7)
  }else if(x=="reds"){
# reds
  red <- rgb2(168,45,23)
  rscale2 <- c(rgb2(168,45,23), 
               rgb2(225,167,68)) 
  rscale3 <- c(rgb2(168,45,23), 
               rgb2(204,107,33),
               rgb2(233,184,85)) 
  rscale4 <- c(rgb2(168,45,23),
               rgb2(195,74,23),
               rgb2(220,150,53),
               rgb2(233,184,85))
  rscale5 <- c(rgb2(124,23,15), 
               rgb2(182,61,23),
               rgb2(204,107,33), 
               rgb2(225,167,68),
               rgb2(241,214,118))
  rscale6 <- c(rgb2(124,23,15), 
               rgb2(174,52,23),
               rgb2(199,79,27),
               rgb2(214,133,43), 
               rgb2(230,176,77),
               rgb2(241,214,118))
  rscale7 <- c(rgb2(124,23,15),
               rgb2(168,45,23),
               rgb2(195,74,23),
               rgb2(204,107,33),
               rgb2(220,150,53),
               rgb2(233,184,85),
               rgb2(241,214,118))
  cols <- list(red, rscale2, rscale3, rscale4, rscale5, rscale6, rscale7)
  }else if(x=="grays"){
# grayscale
  cols <- c(rgb2(229,229,229),
                 rgb2(199,199,199), 
                 rgb2(128,128,128),
                 rgb2(113,113,113),
                 rgb2(63,63,63))
}else if(x=="qualitative"){
# qualitative colours
  cols <- c(rgb2(101,179,46),
                rgb2(124,189,196), 
                rgb2(192,210,54),
                rgb2(62,91,132),
                rgb2(0,140,117),
                rgb2(130,66,141),
                rgb2(232,104,63),
                rgb2(184,26,93))
}else{
  stop("x is not among the current defined colour palettes, please select from 'greens', 'blues', 'reds', 'grays' or 'qualitative'")
}
return(cols)
}