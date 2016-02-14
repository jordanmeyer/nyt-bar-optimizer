library(whisker)

toJSONarray <- function(dtf){
  name.value <- function(i){
    quote <- '';
    if(class(dtf[, i])!='numeric' && class(dtf[, i])!='integer'){
      quote <- '"';
    }
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }

  clnms <- colnames(dtf)

  if(nrow(dtf) > 1) {
    objs <- apply(sapply(clnms, name.value), 1,
                  function(x){paste(x, collapse=', ')})
  } else {
    objs <- paste(sapply(clnms, name.value), collapse=", ")
  }

  objs <- paste('{', objs, '}')

  res <- paste('[', paste(objs, collapse=', '), ']')

  return(res)
}

BasicHead <- function(){
  "<!DOCTYPE html>
<meta charset=\"utf-8\">
<body> \n"
}

ForceMainStyleSheet <- function(){
  "<style>
.link {
stroke: {{linkColour}};
opacity: .4;
stroke-width: 1px;
}
.node circle {
opacity: 1;
}
</style>

<script> \n"
}

MainForceJS <- function(){
  "var width = parseInt(d3.select(\"{{parentElement}}\").style(\"width\")),
   height = parseInt(d3.select(\"{{parentElement}}\").style(\"height\")),
   scale = Math.max(Math.min(225/nodes.length,1.3), .45),
   zoomWidth = (width-scale*width)/2,
   zoomHeight = (height-scale*height)/2;

  var zoom = d3.behavior.zoom().translate([zoomWidth,zoomHeight]).scale(scale);

  // var color = d3.scale.category10();

   /* Initialize tooltip */
  var tip = d3.tip()
    .offset([-10, 0])
    .attr('class', 'd3-tip')
    .html(function(d) {
       return \"<span style='display: block; margin: 0px auto; text-align:center;'><span style='color:\" + color(d.group) + \"; font-weight:strong'>\" + d.displayname + \"</span> \" + \"</span>\" +
         \"<hr><span style='font-size:11px; color:lightgrey'>\" + d.ingredients + \"</span></br>\";
    });

  var color = d3.scale.ordinal()
    .domain([1])
    .range([\"#2E79B9\"]);
    //.range([\"#FF9800\", \"#00BFF1\", \"#F2385A\"]);

  var force = d3.layout.force()
  .nodes(d3.values(nodes))
  .links(links)
  .size([1.05*width, height])
  .linkDistance({{linkDistance}})
  .charge({{charge}})
  .on(\"start\", start)
  .start();

  var svg_base = d3.select(\"{{parentElement}}\").append(\"svg\")
  .attr(\"width\", width)
  .attr(\"height\", height)
  .call(tip)
  .on(\"click\", tip.hide)
  .attr(\"pointer-events\", \"all\");

  var svg = svg_base.call(zoom.on(\"zoom\",redraw))
  .append(\"svg:g\")
  .attr(\"transform\",\"translate(\"+zoomWidth+\",\"+zoomHeight+\")scale(\"+scale+\",\"+scale+\")\");

  function redraw() {
    svg.attr(\"transform\",
    \"translate(\" + d3.event.translate + \")\"
  + \" scale(\" + d3.event.scale + \")\");
  };

  var link = svg.selectAll(\".link\")
  .data(links)
  .enter().append(\"line\")
  .attr(\"class\", \"link\")
  .style(\"stroke-width\", {{linkWidth}});

  var node = svg.selectAll(\".node\")
  .data(d3.values(nodes))
  .enter().append(\"g\")
  .attr(\"class\", \"node\")
  .style(\"fill\", function(d) { return color(d.group); })
  .style(\"stroke\", \"#222d32\")
  .style(\"opacity\", 1)
  .on(\"mouseover\", tip.show)
  .on(\"mouseout\", tip.hide);
  //.call(force.drag);

  node.append(\"circle\")
  .attr(\"r\", function(d) { return 1.5*d.size; })

  function start() {
    var ticksPerRender = 500;
    requestAnimationFrame(function render() {
      for (var i = 0; i < ticksPerRender; i++) {
        force.tick();
      }
  link
  .attr(\"x1\", function(d) { return d.source.x; })
  .attr(\"y1\", function(d) { return d.source.y; })
  .attr(\"x2\", function(d) { return d.target.x; })
  .attr(\"y2\", function(d) { return d.target.y; });

  node.attr(\"transform\", function(d) { return \"translate(\" + d.x + \",\" + d.y + \")\"; });

      if (force.alpha() > 0) {
        requestAnimationFrame(render);
      }
    })
  }

  function resize() {

  var width = parseInt(d3.select(\"{{parentElement}}\").style(\"width\")),
   height = parseInt(d3.select(\"{{parentElement}}\").style(\"height\"));

   svg_base.attr(\"width\",width).attr(\"height\", height);

   svg.attr(\"width\",width).attr(\"height\", height)
   .attr(\"transform\",\"translate(\"+zoomWidth+\",\"+zoomHeight+\")scale(\"+scale+\",\"+scale+\")\");

   force.size([1.05*width, height]).resume();
  }

  d3.select(window).on('resize', resize);

  </script>\n"
  }

create_network <-
  function (Links, Nodes, Source, Target, Value = NULL, NodeID,
            Group, Ingredients, DisplayName, Size, height = 600, width = 900, fontsize = 7, linkDistance = 50,
            linkWidth = "function(d) { return Math.sqrt(d.value); }",
            charge = -120, linkColour = "#aaa", zoom = FALSE,
            parentElement = "body", standAlone = TRUE, file = NULL, iframe = FALSE,
            d3Script = "http://d3js.org/d3.v3.min.js")
  {
    if (!isTRUE(standAlone) & isTRUE(iframe)) {
      stop("If iframe = TRUE then standAlone must be TRUE.")
    }
    if (is.null(file) & isTRUE(iframe)) {
      Random <- paste0(sample(c(0:9, letters, LETTERS), 5,
                              replace = TRUE), collapse = "")
      file <- paste0("NetworkGraph", Random, ".html")
    }
    FrameHeight <- height + height * 0.07
    FrameWidth <- width + width * 0.03
    clickTextSize <- fontsize * 2.5
    if (class(Links) != "data.frame") {
      stop("Links must be a data frame class object.")
    }
    if (class(Nodes) != "data.frame") {
      stop("Nodes must be a data frame class object.")
    }
    if (is.null(Value)) {
      LinksDF <- data.frame(Links[, Source], Links[, Target])
      names(LinksDF) <- c("source", "target")
    }
    else if (!is.null(Value)) {
      LinksDF <- data.frame(Links[, Source], Links[, Target],
                            Links[, Value])
      names(LinksDF) <- c("source", "target", "value")
    }
    NodesDF <- data.frame(Nodes[, NodeID], Nodes[, Group], Nodes[, Ingredients], Nodes[, DisplayName], Nodes[, Size])
    names(NodesDF) <- c("name", "group", "ingredients", "displayname", "size")
    LinkData <- toJSONarray(LinksDF)
    LinkData <- paste("var links =", LinkData, "; \n")
    NodesData <- toJSONarray(NodesDF)
    NodesData <- paste("var nodes =", NodesData, "; \n")
    PageHead <- BasicHead()
    NetworkCSS <- whisker.render(ForceMainStyleSheet())
    if (!isTRUE(zoom)) {
      MainScript <- whisker.render(MainForceJS())
    }
    else if (isTRUE(zoom)) {
      MainScript <- whisker.render(ForceZoomJS())
    }
    if (is.null(file) & !isTRUE(standAlone)) {
      cat(NetworkCSS, LinkData, NodesData, MainScript)
    }
    else if (is.null(file) & isTRUE(standAlone)) {
      cat(PageHead, NetworkCSS, LinkData, NodesData, MainScript,
          "</body>")
    }
    else if (!is.null(file) & !isTRUE(standAlone)) {
      cat(NetworkCSS, LinkData, NodesData, MainScript, file = file)
    }
    else if (!is.null(file) & !isTRUE(iframe)) {
      cat(PageHead, NetworkCSS, LinkData, NodesData, MainScript,
          "</body>", file = file)
    }
    else if (!is.null(file) & isTRUE(iframe)) {
      cat(PageHead, NetworkCSS, LinkData, NodesData, MainScript,
          "</body>", file = file)
      cat("<iframe src='", file, "'", " height=", FrameHeight,
          " width=", FrameWidth, "></iframe>", sep = "")
    }
  }
