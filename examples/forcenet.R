\dontrun{
v <- validator( 2*x + 3*y < z, z > y)
html <- tempfile(paste0(tempfile(),'.html'))
forcenet(v, file=html)
browseURL(html)
}
