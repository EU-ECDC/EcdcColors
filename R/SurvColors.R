#' Colour palettes following the March 2018 ECDC guidelines for presentation of surveillance data
#'
#' Full document: European Centre for Disease Prevention and Control. Guidelines for presentation of surveillance data.
#' Stockholm: ECDC; 2018. Available from: https://ecdc.europa.eu/sites/portal/files/documents/Guidelines%20for%20presentation%20of%20surveillance%20data-final-with-cover-for-we..._0.pdf
#'
#' @param col_scale Selected colour scale, defaults to 'green'. Select from 'green', 'blue', 'red', 'grey', 'qual(itative)' or 'hotcold'
#' @param n Number of colours from each colour scale, apart from grey, in preferred order. Defaults to one colour, max 7-8 colours for each scale. To select
#' grey shades, use the argument grey_shade instead.
#' @param grey_shade Selected shade(s) of grey, use only for greyscale; overrides given number of colours (n).
#' @param hot_cols Selected number of hot (warm) colours in the hotcold colour scale, use only for hotcold; must be smaller than number of colours (n).
#' @author Tommi Karki
#' @keywords colourscales
#' @importFrom "grDevices" "rgb"
#' @export
#' @examples
#' # Select three first green colours
#' SurvColors("green", n=3)
#' 
#' # Select two first qualitative colours
#' SurvColors("qual", n=2)
#' 
#' SurvColors("grey", grey_shade = c("mediumlight", "dark"))
#' 
#' # Use in a graph
#' # Dummy data
#' mydat <- data.frame(ID = c(seq(1,10,1)),
#' Gender = c(rep(c("F", "M"),5)))
#' barplot(table(mydat$Gender), col = SurvColors(col_scale = "qual", n=2))
#' 
#' # Hot-cold colour scale
#' barplot(c(1:4), col = SurvColors(col_scale = "hotcold", n = 4, hot_cols = 2))
SurvColors <- function(col_scale = "green", n = NULL, grey_shade = c("light", 
                                                      "mediumlight",
                                                      "medium",
                                                      "mediumdark",
                                                      "dark"),
                       hot_cols = 1){
  if(is.null(n) & col_scale != "grey"){
  n <- 1
  }else{
  n <- n
  }
  
  if(grepl("qual", col_scale)){
    col_scale <- "qualitative"
  }
  
  if(!is.null(n)){
  if(n>7 & col_scale != "qualitative"){
    stop("Maximum number of colours (n) for selected colour scale is 7!")
  }else if(n>8 & col_scale == "qualitative"){
    stop("Maximum number of colours (n) for selected colour scale is 8!")
  }}
  
  if(col_scale=="green"){
# greens
  gscale1 <- rgb(101,179,46, maxColorValue = 255)
  gscale2 <- c(rgb(32,119,50, maxColorValue = 255), 
               rgb(178,207,110, maxColorValue = 255)) 
  gscale3 <- c(rgb(26,110,49, maxColorValue = 255), 
               rgb(101,179,46, maxColorValue = 255),
               rgb(201,217,113, maxColorValue = 255)) 
  gscale4 <- c(rgb(26,110,49, maxColorValue = 255), 
               rgb(40,147,55, maxColorValue = 255),
               rgb(156,198,90, maxColorValue = 255), 
               rgb(201,217,113, maxColorValue = 255))
  gscale5 <- c(rgb(12,72,40, maxColorValue = 255), 
               rgb(32,129,53, maxColorValue = 255),
               rgb(101,179,46, maxColorValue = 255), 
               rgb(178,207,110, maxColorValue = 255), 
               rgb(231,231,185, maxColorValue = 255)) 
  gscale6 <- c(rgb(12,72,40, maxColorValue = 255), 
               rgb(32,119,50, maxColorValue = 255), 
               rgb(66,158,53, maxColorValue = 255),
               rgb(137,190,71, maxColorValue = 255), 
               rgb(192,212,122, maxColorValue = 255), 
               rgb(231,231,185, maxColorValue = 255)) 
  gscale7 <- c(rgb(12,72,40, maxColorValue = 255), 
               rgb(26,110,49, maxColorValue = 255), 
               rgb(40,147,55, maxColorValue = 255),
               rgb(101,179,46, maxColorValue = 255), 
               rgb(156,198,90, maxColorValue = 255), 
               rgb(201,217,113, maxColorValue = 255), 
               rgb(231,231,185, maxColorValue = 255))
  cols <- get(paste0("gscale", n))
  }else if(col_scale=="blue"){  
# blues
  bscale1 <- rgb(124,189,196, maxColorValue = 255)
  bscale2 <- c(rgb(60,142,162, maxColorValue = 255), 
               rgb(173,210,221, maxColorValue = 255)) 
  bscale3 <- c(rgb(26,107,133, maxColorValue = 255), 
               rgb(124,189,196, maxColorValue = 255),
               rgb(194,218,232, maxColorValue = 255)) 
  bscale4 <- c(rgb(26,107,133, maxColorValue = 255), 
               rgb(73,153,171, maxColorValue = 255),
               rgb(165,206,215, maxColorValue = 255), 
               rgb(194,218,232, maxColorValue = 255))
  bscale5 <- c(rgb(0,60,80, maxColorValue = 255), 
               rgb(60,142,162, maxColorValue = 255),
               rgb(124,189,196, maxColorValue = 255), 
               rgb(173,210,221, maxColorValue = 255),
               rgb(227,232,240, maxColorValue = 255))
  bscale6 <- c(rgb(0,60,80, maxColorValue = 255), 
               rgb(39,117,142, maxColorValue = 255),
               rgb(95,167,181, maxColorValue = 255), 
               rgb(147,199,207, maxColorValue = 255), 
               rgb(187,216,229, maxColorValue = 255), 
               rgb(227,232,240, maxColorValue = 255))
  bscale7 <- c(rgb(0,60,80, maxColorValue = 255), 
               rgb(26,107,133, maxColorValue = 255), 
               rgb(73,153,171, maxColorValue = 255), 
               rgb(124,189,196, maxColorValue = 255),
               rgb(165,206,215, maxColorValue = 255), 
               rgb(194,218,232, maxColorValue = 255),
               rgb(227,232,240, maxColorValue = 255))
  cols <- get(paste0("bscale", n))
  }else if(col_scale=="red"){
# reds
  rscale1 <- rgb(168,45,23, maxColorValue = 255)
  rscale2 <- c(rgb(168,45,23, maxColorValue = 255), 
               rgb(225,167,68, maxColorValue = 255)) 
  rscale3 <- c(rgb(168,45,23, maxColorValue = 255), 
               rgb(204,107,33, maxColorValue = 255),
               rgb(233,184,85, maxColorValue = 255)) 
  rscale4 <- c(rgb(168,45,23, maxColorValue = 255),
               rgb(195,74,23, maxColorValue = 255),
               rgb(220,150,53, maxColorValue = 255),
               rgb(233,184,85, maxColorValue = 255))
  rscale5 <- c(rgb(124,23,15, maxColorValue = 255), 
               rgb(182,61,23, maxColorValue = 255),
               rgb(204,107,33, maxColorValue = 255), 
               rgb(225,167,68, maxColorValue = 255),
               rgb(241,214,118, maxColorValue = 255))
  rscale6 <- c(rgb(124,23,15, maxColorValue = 255), 
               rgb(174,52,23, maxColorValue = 255),
               rgb(199,79,27, maxColorValue = 255),
               rgb(214,133,43, maxColorValue = 255), 
               rgb(230,176,77, maxColorValue = 255),
               rgb(241,214,118, maxColorValue = 255))
  rscale7 <- c(rgb(124,23,15, maxColorValue = 255),
               rgb(168,45,23, maxColorValue = 255),
               rgb(195,74,23, maxColorValue = 255),
               rgb(204,107,33, maxColorValue = 255),
               rgb(220,150,53, maxColorValue = 255),
               rgb(233,184,85, maxColorValue = 255),
               rgb(241,214,118, maxColorValue = 255))
  cols <- get(paste0("rscale", n))
  }else if(col_scale == "grey"){
# greyscale
    shades <- c("light", 
                     "mediumlight",
                     "medium",
                     "mediumdark",
                     "dark")
  cols <- c(rgb(229,229,229, maxColorValue = 255),
                 rgb(199,199,199, maxColorValue = 255), 
                 rgb(128,128,128, maxColorValue = 255),
                 rgb(113,113,113, maxColorValue = 255),
                 rgb(63,63,63, maxColorValue = 255))
  cols <- cols[shades %in% grey_shade]
  
  if(is.null(n) & length(grey_shade) == 5){
    message("Greyzone -  If you want one or more specific grey shades, please insert the grey_shade(s): c('light', 'mediumlight','medium','mediumdark','dark')")
  }else if(!is.null(n)){
    message("Greyzone - number of colours (n) overridden by shades of grey. If you want one or more specific grey shades, please insert the grey_shade(s): c('light', 'mediumlight','medium','mediumdark','dark')")
  }
}else if(col_scale=="qualitative"){
# qualitative colours
  cols <- c(rgb(101,179,46, maxColorValue = 255),
                rgb(124,189,196, maxColorValue = 255), 
                rgb(192,210,54, maxColorValue = 255),
                rgb(62,91,132, maxColorValue = 255),
                rgb(0,140,117, maxColorValue = 255),
                rgb(130,66,141, maxColorValue = 255),
                rgb(232,104,63, maxColorValue = 255),
                rgb(184,26,93, maxColorValue = 255))
  cols <- cols[1:n]
}else if(col_scale=="hotcold"){
  if(n<=hot_cols){
    stop("Total number (n) of colours must be greater than n of hot colours (hot_cols)!")
  }
  # reds
  rscale1 <- rgb(168,45,23, maxColorValue = 255)
  rscale2 <- c(rgb(168,45,23, maxColorValue = 255), 
               rgb(225,167,68, maxColorValue = 255)) 
  rscale3 <- c(rgb(168,45,23, maxColorValue = 255), 
               rgb(204,107,33, maxColorValue = 255),
               rgb(233,184,85, maxColorValue = 255)) 
  rscale4 <- c(rgb(168,45,23, maxColorValue = 255),
               rgb(195,74,23, maxColorValue = 255),
               rgb(220,150,53, maxColorValue = 255),
               rgb(233,184,85, maxColorValue = 255))
  rscale5 <- c(rgb(124,23,15, maxColorValue = 255), 
               rgb(182,61,23, maxColorValue = 255),
               rgb(204,107,33, maxColorValue = 255), 
               rgb(225,167,68, maxColorValue = 255),
               rgb(241,214,118, maxColorValue = 255))
  rscale6 <- c(rgb(124,23,15, maxColorValue = 255), 
               rgb(174,52,23, maxColorValue = 255),
               rgb(199,79,27, maxColorValue = 255),
               rgb(214,133,43, maxColorValue = 255), 
               rgb(230,176,77, maxColorValue = 255),
               rgb(241,214,118, maxColorValue = 255))
  rscale7 <- c(rgb(124,23,15, maxColorValue = 255),
               rgb(168,45,23, maxColorValue = 255),
               rgb(195,74,23, maxColorValue = 255),
               rgb(204,107,33, maxColorValue = 255),
               rgb(220,150,53, maxColorValue = 255),
               rgb(233,184,85, maxColorValue = 255),
               rgb(241,214,118, maxColorValue = 255))
  bscale1 <- rgb(124,189,196, maxColorValue = 255)
  bscale2 <- c(rgb(60,142,162, maxColorValue = 255), 
               rgb(173,210,221, maxColorValue = 255)) 
  bscale3 <- c(rgb(26,107,133, maxColorValue = 255), 
               rgb(124,189,196, maxColorValue = 255),
               rgb(194,218,232, maxColorValue = 255)) 
  bscale4 <- c(rgb(26,107,133, maxColorValue = 255), 
               rgb(73,153,171, maxColorValue = 255),
               rgb(165,206,215, maxColorValue = 255), 
               rgb(194,218,232, maxColorValue = 255))
  bscale5 <- c(rgb(0,60,80, maxColorValue = 255), 
               rgb(60,142,162, maxColorValue = 255),
               rgb(124,189,196, maxColorValue = 255), 
               rgb(173,210,221, maxColorValue = 255),
               rgb(227,232,240, maxColorValue = 255))
  bscale6 <- c(rgb(0,60,80, maxColorValue = 255), 
               rgb(39,117,142, maxColorValue = 255),
               rgb(95,167,181, maxColorValue = 255), 
               rgb(147,199,207, maxColorValue = 255), 
               rgb(187,216,229, maxColorValue = 255), 
               rgb(227,232,240, maxColorValue = 255))
  bscale7 <- c(rgb(0,60,80, maxColorValue = 255), 
               rgb(26,107,133, maxColorValue = 255), 
               rgb(73,153,171, maxColorValue = 255), 
               rgb(124,189,196, maxColorValue = 255),
               rgb(165,206,215, maxColorValue = 255), 
               rgb(194,218,232, maxColorValue = 255),
               rgb(227,232,240, maxColorValue = 255))
  cold_cols <- n-hot_cols
  cols <- get(paste0("rscale", hot_cols))
  cols <- c(cols, rev(get(paste0("bscale", cold_cols))))
  
}else{
  stop("col_scale is not among the currently defined colour palettes, please select from 'green', 'blue', 'red', 'grey' or 'qual(itative)'")
}
  return(cols)
}
