
#' Generic d3 plot function.
#'
#' @param x an R object
#' @param ... options to be passed to other methods
#' @export
setGeneric("d3plot",def=function(x,...) standardGeneric("d3plot"))

#' @rdname d3plot
#'
#' @param col named vector with colors for nodes representing variables, rules.
#' @param r Radius of nodes (pixels) for nodes representing variables, rules
#'
#' @export
setMethod('d3plot',signature("validator"), 
  function(x, col=c(variable="#FFFFB3",rule="#80B1D3"), r=c(variable=7,rule=7), ...){
  nodes <- es_nodes(x)
    nodes$fill <- col[nodes$type]
    nodes$r <- r[nodes$type]
    nodes$opacity <- 1
    nodes$fill_opacity <- 1
    nodes$stroke <- 'black'
    nodes$stroke_width <- 1
    nodes$stroke_opacity <- 0.5
  
  
  edges <- es_edges(x)
    edges$source <- match(edges$source,nodes$name) - 1
    edges$target <- match(edges$target,nodes$name) - 1
    edges$stroke = 'grey'
    edges$stroke_opacity = 0.5
    edges$stroke_width = 2

  d3forcenet(nodes,edges,...)
})



#' Rule and/or variable nodes or edges from expressionset
#' 
#' @section Details:
#' The output of \code{es_nodes} and \code{es_edges} can be used to define \code{igraph} objects
#' as follows: \code{igraph.data.frame(es_edges(x),vertices=es_nodes(x))}. 
#' 
#' 
#' @param x an object of class \code{expressionset}
#' @param type Which nodes to include
#' @param dummy Include rules defining dummy variables? 
#' @keywords internal
es_nodes <- function(x, type=c('all','rules','variables'), dummy=FALSE){
  type = match.arg(type)
  R <- V <- NULL
  
  i <- if ( dummy ) TRUE else sapply(x$calls(),function(x) x[[1]] != ':=')
  
  if ( type %in% c('all','rules') )
    R <- data.frame(name = names(x)[i], type  = 'rule',label=as.character(x)[i])
  
  if ( type %in% c('all','variables') )
    V <- data.frame(name=variables(x,dummy=dummy), type='variable',label=variables(x,dummy=dummy))
  
  
  rbind(R,V)
  
}

#' @rdname es_nodes
#' @keywords internal
es_edges <- function(x, type=c('all','rules','variables'),dummy=FALSE){
  type <- match.arg(type)
  vars <- variables(x,as='list',dummy=dummy)
  
  switch(type
   , 'all' = setNames(stack(vars), c("source","target"))
   , 'variables' = {
     L <- lapply(vars,function(x) if(length(x)<2) NULL else combn(sort(x),2) )
     setNames(as.data.frame(unique(t(do.call(cbind,L)))),c('source','target'))
   }
   , 'rules' = {
     i <- if ( dummy ) TRUE else sapply(x$calls(),function(x) x[[1]] != ':=')
     rules <- names(x)[i]
     M <- if ( length(rules) < 2 ) array("",dim=c(2,0)) else combn(names(x),2)
     i <- apply(M, 2, function(x) any(vars[[x[1]]] %in% vars[[x[2]]]) )
     setNames(as.data.frame(t(M[,i,drop=FALSE])),c('source','target'))
   })
}



d3forcenet <- function(nodes, edges, open=TRUE, width=500, height=500, d3lib="http://d3js.org/d3.v3.min.js"){
  e <- new.env()
  e$title <- ""
  e$d3lib <- d3lib
  e$height <- height
  e$width <- width
  
  
  
  e$graph <- as.json(list(nodes=as.json(nodes),edges=as.json(edges)))
  e$distance <- 40
  html <- whisker.render(forcenet(),data=e)
  if ( open ){
    tmp <- tempfile(fileext = ".html")
    write(html,file=tmp)
    viewer <- getOption('viewer')
    view <- if( is.null(viewer) ) function(x) utils::browseURL(x) else viewer
    
    view(tmp)
    invisible(html)
  } else {
    html
  }
}


forcenet <- function(){
'<!DOCTYPE HTML>
<html>
  <head>
  <meta charset="utf-8">
  <title>{{{title}}}</title>
  <script type="text/javascript" src="{{{d3lib}}}"></script>
</head>
<body>

<script type="text/javascript">
  var graph = {{{graph}}};
  var width = {{{width}}};
  var height = {{{height}}};


  var force = d3.layout.force()
    .charge(-220)
    .linkDistance({{{distance}}})
    .size([width, height]);

  var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height",height);

  svg.append("marker")
    .attr("id","triangle")
    .attr("viewBox","0 0 10 10")
    .attr("refX","20")
    .attr("refY","5")
    .attr("markerUnits","strokeWidth")
    .attr("markerWidth","3")
    .attr("markerHeight","4")
    .attr("orient","auto")
    .attr("fill","black")
    .append("path")
      .attr("d","M 0 0 L 10 5 L 0 10 z");

  force
    .nodes(graph.nodes)
    .links(graph.edges)
    .start();

  var link = svg.selectAll(".link")
    .data(graph.edges)
    .enter().append("line")
    .attr("class","link")
//    .attr("marker-end","url(#triangle)")
    .attr("stroke", function(d){ return d.stroke; })
    .attr("stroke-opacity",function(d){ return d.stroke_opacity; })
    .attr("stroke-width",function(d){ return d.stroke_width; });


  var node = svg.selectAll(".node")
    .data(graph.nodes)
    .enter().append("circle")
    .attr("class","node")
    .attr("fill",function(d){ return d.fill; })
    .attr("r", function(d){ return d.r; })
    .attr("opacity", function(d){ return d.opacity; })
    .attr("fill-opacity", function(d){ return d.fill_opacity; })
    .attr("stroke", function(d){ return d.stroke; })
    .attr("stroke-width", function(d){ return d.stroke_width; })
    .attr("stroke-opacity", function(d){ return d.stroke_opacity; })
    .call(force.drag);

  node.append("title")
      .text(function(d){ return d.label; });


  force.on("tick", function() {
    link.attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

    node.attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; });
  });
</script>

</body>
</html>
'
}
