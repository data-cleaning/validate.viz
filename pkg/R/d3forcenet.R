
#' Generic d3 plot function.
#'
#' @param x an R object inheriging from \code{\link[validate]{expressionset}}
#' @param y [optional] an R object inheriting fron \code{\link[validate]{confrontation}}
#' @param ... options to be passed to d3forcegraph
#' @export
setGeneric("d3graph",def=function(x,y,...) standardGeneric("d3graph"))

#' @rdname d3plot
#'
#' @param col named vector with colors for nodes representing variables, rules.
#' @param r Radius of nodes (pixels) for nodes representing variables, rules
#' 
#'
#' @export
setMethod('d3graph',signature("validator"), 
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

setMethod('d3graph',signature('validator','validation'),
  function(x,y, col=c(rule="#FFFFB3",variable="#80B1D3"), r=c(variable=7,rule=7), ...){
            
  a <- aggregate(y)
  nodes <- es_nodes(x)

  irule <- nodes$type == 'rule'
  
  nodes$fill <- col[nodes$type]
    nodes$fill <- col[nodes$type]
    nodes$r[ irule] <- pixelize(1-a$rel.NA,min=5,max=15)
    nodes$r[!irule] <- r['variable']
    nodes$opacity <- 1
    nodes$fill_opacity <- 1
    nodes$stroke <- 'black'
    nodes$stroke_width <- 1
    nodes$stroke_opacity <- 0.5

  
    pal <- brewer.pal(10,"RdYlGn")[c(10,1)]
    V <- colorRamp(pal)(with(a,nfail/(nfail+npass)))

    nodes$fill[irule] <- rgb(V[,1],V[,2],V[,3],maxColorValue=255) 
    lab <- paste(nodes$lab[irule],"| failed",a$nfail,"times,",a$nNA,"NA's")
    nodes$label[irule] <- lab

  
  edges <- es_edges(x)
    edges$source <- match(edges$source,nodes$name) - 1
    edges$target <- match(edges$target,nodes$name) - 1
    edges$stroke = 'grey'
    edges$stroke_opacity = 0.5
    edges$stroke_width = 2
  
  d3forcenet(nodes,edges,outer=TRUE,...)
  
  
})


#' Rule and/or variable nodes or edges from expressionset
#' 
#' @section Details:
#' The output of \code{es_nodes} and \code{es_edges} can be used to define \code{igraph} objects
#' as follows: \code{igraph.data.frame(es_edges(x),vertices=es_nodes(x))}. 
#' 
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
    R <- data.frame(name = names(x)[i], type  = 'rule',label=as.character(x)[i], stringsAsFactors=FALSE)
  
  if ( type %in% c('all','variables') )
    V <- data.frame(name=variables(x,dummy=dummy), type='variable',label=variables(x,dummy=dummy),stringsAsFactors=FALSE)
  
  
  out <- rbind(R,V)
  if (anyDuplicated(out$name))
    warning("Found duplicated node names. Rendering a graph is probably inaccurate. (rules and
            variables must have separate names).")
  out
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



d3forcenet <- function(nodes, edges, open=TRUE
      , width=500, height=500
      , distance=40, d3lib="http://d3js.org/d3.v3.min.js"
      , charge = -220
      , outer = FALSE
      ){
  e <- new.env()
  e$title <- ""
  e$d3lib <- d3lib
  e$height <- height
  e$width <- width
  e$outer <- outer
  
  
  e$graph <- as.json(list(nodes=as.json(nodes),edges=as.json(edges)))
  e$distance <- distance
  e$charge <- charge
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
    .charge({{{charge}}})
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
    .attr("stroke", function(d){ return d.stroke; })
    .attr("stroke-opacity",function(d){ return d.stroke_opacity; })
    .attr("stroke-width",function(d){ return d.stroke_width; });


var node = svg.selectAll(".node")
  .data(graph.nodes)
    .enter().append("g")
    .attr("class","node")
    .call(force.drag);

// outer circles
{{#outer}}
node.append("circle")
    .attr("r",15)
    .style("fill-opacity",function(d) {if (d.type=="rule") {return 0.3;} else {return 0.0;};}  );
{{/outer}}

// inner circles
  node.append("circle")
    .attr("r",10)
    .attr("fill",function(d){ return d.fill; })
    .attr("r", function(d){ return d.r; })
    .attr("opacity", function(d){ return d.opacity; })
    .attr("fill-opacity", function(d){ return d.fill_opacity; })
    .attr("stroke", function(d){ return d.stroke; })
    .attr("stroke-width", function(d){ return d.stroke_width; })
    .attr("stroke-opacity", function(d){ return d.stroke_opacity; })

// hover-over text
  node.append("title")
      .text(function(d){ return d.label; });

// displayed text
  node.append("text")
    .text(function(d){return d.name;});


  force.on("tick", function() {
    link.attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });
    
    node.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
  });
</script>

</body>
</html>
'
}
