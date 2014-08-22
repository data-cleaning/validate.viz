#' Visualisation of validation rules and results.
#' 
#' This package extends the validate package.
#' 
#' @docType package
#' @name validate.viz
#' @import methods whisker validate
NULL


#' Coerce to JSON
#' 
#' 
#' @param x an R object \code{\link[base]{data.frame}}
#' @param ... arguments to pass to other methods
#' @export 
as.json <- function(x,...){
  UseMethod('as.json')
}


#' @rdname as.json
#' @return For \code{x} a \code{data.frame}, an array of \code{JSON} objects, one for each row of \code{x}.
as.json.data.frame <- function(x,...){
  for (n in names(x) )  x[,n] <- if (is.character(x[,n])|is.factor(x[,n])) paste0('"',x[,n],'"') else x[,n]
  A <- t(x)
  v <- apply(A,2,function(a) paste0('"',rownames(A),'"'," : ",a))
  if ( is.null(dim(v)) ) v <- array(v,dim=c(1,ncol(A))) 
  paste0("[\n",paste0("{",apply(v,2,paste,collapse=", "),"}",collapse=",\n"),"\n]")
}

#' @rdname as.json
#' @return For \code{x} a \code{list}, a \code{JSON} object with elements corresponding to the list 
#'  entries. List entries are coerced to character using \code{\link[base]{paste}}.
#' @export 
as.json.list <- function(x,...){
  u <- paste0('"',names(x),'" : ')  
  v <- sapply(x,function(v) if (is.character(v)|is.factor(v)) paste0(v) else as.character(v))
  paste0("{\n",paste0(u,v,collapse=',\n'),"\n}\n")
}




