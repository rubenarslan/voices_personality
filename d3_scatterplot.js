// !preview r2d3 data=data.frame(x=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), y = 1:6, add = 3, size = 2)
//
// r2d3: https://rstudio.github.io/r2d3
//

//Set some initial values
var margin = options.margin,
    width = width-(2*margin), height = height-(2*margin),
    xmax = options.xmax,
    xmin = options.xmin,
    ymax = options.ymax,
    ymin = options.ymin;

//Create the axes
x = d3.scaleLinear()
    .range([0+margin, margin+width])
    .domain([xmin - 0.2, xmax + 0.2]);
y = d3.scaleLinear()
    .range([height+margin, 0+margin])
    .domain([ymin - 0.2, ymax + 0.2]);

//Append axes
var xAxisTranslate = height + 1*margin;

svg.append("g")
  .attr("transform", "translate(10, " + xAxisTranslate  +")")
  .call(d3.axisBottom(x));
svg.append("g")
  .attr("transform", "translate(50,10)")
  .call(d3.axisLeft(y));

//Axes labels
svg.append("text")
  .attr("transform", "translate(" + (width/2) + " ," + (height+1.8*margin) + ")")
  .attr("dx", "1em") .style("text-anchor", "middle")
  .style("font-family", "Helvetica, Arial, sans-serif")
  .style("font-size", "16pt") .text(options.xLabel);

svg.append("text") .attr("transform", "translate(" + 0 + " ," + ((height+2*margin)/2) + ") rotate(-90)")
  .attr("dy", "1em")
  .style("text-anchor", "middle")
  .style("font-family", "Helvetica, Arial, sans-serif")
  .style("font-size", "16pt")
  .text(options.yLabel);

//Create the chart title
svg.append("text")
  .attr("x", (width/2))
  .attr("y", (margin/2))
  .attr("text-anchor", "middle")
  .attr("dx", "1em")
  .style("font-size", "20pt")
  .style("font-family", "Helvetica, Arial, sans-serif")
  .text(options.chartTitle);


  var gdots_participants =  svg.selectAll("g.dot")
            .data(data.filter(function(d) { return d.f0_vcs !== null }))
            .enter()
            .append('g');

  var circles_participants = gdots_participants.append("circle")
            .attr("class", "dot")
            .attr("r", 2)
            .attr("cx", function (d) {
                return x(d.f0_vcs);
            })
            .attr("cy", function (d) {
                return y(d.pf_vcs);
            })
            .style("fill", function(d) {
              return d.sex == 1 ? "black" : "#40A5BF";
              })
            .style("opacity", "0.2");

  var gdots =  svg.selectAll("g.dot")
            .data(data.filter(function(d) { return d.x !== null }))
            .enter()
            .append('g');

  var circles = gdots.append("circle")
            .attr("class", "dot")
            .attr("r", 4)
            .attr("cx", function (d) {
                return x(d.x);
            })
            .attr("cy", function (d) {
                return y(d.y);
            });

  var texts = gdots
            .append("text")
            .text(function(d){
            	return d.author;
            })
            .attr("x", function (d) {
                return x(d.x + 0.05);
            })
            .attr("y", function (d) {
                return y(d.pf_pos - 0.05);
            });

window.player = {};

var click = function(d) {
  if(window.player[d.author] === undefined) {
    window.player[d.author] = new Audio('author_voices/' + d.author + '.mp3');
  }
  var aud = window.player[d.author]

  if(aud.paused) {
    dot =     d3.select(this)
    debugger;
    d3.select(this.firstChild)
      .attr("r", 6)
    aud.play()
  } else {
    d3.select(this.firstChild)
      .attr("r", 4)
    aud.pause()
  }
};

var mouseover = function(d) {
d3.select(this)
  .style('fill', options.hovercolour)
  .style('color', options.hovercolour);
};

var mouseleave = function(d) {
d3.select(this)
  .style('fill', options.colour);
};

gdots
  .on("mouseover", mouseover)
  .on("mouseleave", mouseleave)
  .on("click", click);

