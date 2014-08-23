
Id <- function(x) x

#' Translate data to a number of pixels
#' @param x a \code{numeric} vector
#' @param min Minumum nr of pixels
#' @param max Maximum nr of pixels
#' @param scale A scaling function applied to x (\code{Id} means no scale)
#' @export 
#' 
#' This interface is not general enough. It should be:
#' pixelize <- function(x, domain=range(x), range, transform=Id)
#' 
#' 
pixelize <- function(x, min=3, max=7, scale=Id){
  stopifnot(is.numeric(x))
  dx <- diff(range(x))
  dy <- max - min
  min + (x-min(x)) * dy/dx
}

#' Translate data to colors
#' 
#' @param x a vector
#'
#' @export
colorize <- function(x,...){
  UseMethod('colorize')
}

#' @param palette A color palette. If missing \code{\link{RColorBrewer}}'s \code{Dark2} palette is chosen. 
#' @rdname colorize
colorize.factor <- function(x,palette=NULL,...){
  
  lev <- levels(x)
  n <- length(lev)
  if (is.null(palette)){ # RColorBrewer warns at n<=2
    map <- brewer.pal(max(3,n),name="Dark2")[1:n]
  } else {
    map <- palette
  }
  names(map) <- lev
  map[x]
}

#' @rdname colorize
#' @export
colorize.character <- function(x,palette=NULL,...){
  colorize.factor(as.factor(x),palette=palette,...)
}

#' @rdname colorize
#' @export 
colorize.integer <- function(x,palette=NULL,...){
  lev <- unique(x)
  n <- length(lev)
  i <- match(x,lev)
  if (is.null(palette)){
    map <- brewer.pal(max(3,n),name="Dark2")[1:n]
  } else {
    map <- palette
  }
  map[i]
}
