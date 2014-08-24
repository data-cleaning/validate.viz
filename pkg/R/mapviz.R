
Id <- function(x) x

setGeneric('rescale',def=function(x, domain, range, ...) standardGeneric('rescale'))

#  Transform a function to transform x and domain with prior to rescaling.
setMethod('rescale',signature('numeric','numeric','numeric'), function(x,domain=range(x), range, transform=Id){
  x <- transform(x)
  dx <- diff(transform(domain))
  dy <- diff(range)
  range[1] + (x-domain[1]) * dy/dx  
})

setMethod('rescale',signature('numeric','matrix','numeric'), function(x, domain, range, transform=Id){
  x <- transform(x)
  dx <- transform(domain[,2]) - transform(domain[,1])
  dy <- diff(range)
  range[1] + (x - domain[,1]) * dy/dx
})

setMethod('rescale',signature('numeric','matrix','matrix'), function(x, domain, range, transform=Id){
  x <- transform(x)  
  dx <- transform(domain[,2]) - transform(domain[,1])
  dy <- range[,2] - range[,1]
  range[,1] + (x - domain[,1]) * dy/dx
})

setMethod('rescale',signature('numeric','numeric','matrix'),function(x, domain=range(x), range, transform=Id){
  x <- transform(x)
  dx <- diff(domain)
  dy <- range[,2] - range[,1]
  range[,1] + (x - domain[,1]) * dy/dx
})



