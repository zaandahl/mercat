//
// Global variable definitions
// (despite the var in front)
//
var sizeLabel = "COUNT",
    subNodeIdLabel = "name",
    superNodeIdLabel = "groupID",
    resistLabel = "RES",
    sourceLabel = "from",
    targetLabel = "to",
    initx = "layoutX",
    inity = "layoutY";

var abNames = [];

var readForestLinks = false;
var areaScale;
var showNodeLabels;
var minInnerRwithLabels = 10,
    minInnerRwithoutLabels = 0;

var forestForce = d3.layout.force()
    .size([width, height])
    .charge(-150) //function(d) { return -20*rScale(d[sizeLabel]); })
    //.gravity(0.1)
    .linkDistance( function(link) { return link.source.totalRadius + link.target.totalRadius + defaultLinkLength } )
    // .linkStrength(1)
    .on("tick", forestTick);

forestForce.drag().on("dragstart", function(d) { d.fixed=true; } ); // once dragged, set fixed

//
// End of global variable definitions (now just global functions...)
//

d3.select("#forest-viz").append("svg")
    .attr("width", width)
    .attr("height", height);

// arrowheads
d3.select("#forest-viz svg").append("defs").append("marker")
    .attr("id", "arrowhead")
    .attr("fill",forestLinkColor)
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 8)
    .attr("refY", 0)
    .attr("markerWidth", arrowWidth)
    .attr("markerHeight", arrowHeight)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M0,-5L10,0L0,5"); // shape of arrowhead


function forestTick(e) {
    // Push different nodes in different directions for clustering.
    // var k = 6 * e.alpha;
    // nodes[0].x -= k;
    // nodes[6].x += k;
    // TODO: don't calculate each point twice
    d3.selectAll("#forest-viz svg .link")
        // .each(function(d) { d.source.y -= k, d.target.y += k; })
        .attr("x1", function(d) { return circleLineIntsn(d.target, d.source, d.source.totalRadius+linkGap).x; })
        .attr("y1", function(d) { return circleLineIntsn(d.target, d.source, d.source.totalRadius+linkGap).y; })
        .attr("x2", function(d) { return circleLineIntsn(d.source, d.target, d.target.totalRadius+linkGap+arrowHeight/2-1).x; })
        .attr("y2", function(d) { return circleLineIntsn(d.source, d.target, d.target.totalRadius+linkGap+arrowHeight/2-1).y; });
    d3.selectAll("#forest-viz svg .node")
        .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
}
function forestClick(d) {
    if (!d3.event.defaultPrevented) {  // ignore drag
        d.fixed = false; 
    }
}


function makeTooltipText(subnode, resNum) {
    var txt = "";
    if (abNames.length>0) {
        txt = "<p><span>Antibiotic</span>"+abNames[resNum]+"</p>";
    }
    return tooltipTextFromSubNode(subnode)+txt;
}

function nodeArcData(d, gap, pie) {
    // given a node, return data to bind as arcs for the pie chart
    // and save the total radius to the node
    // see below re pie arg
    var result = [];
    var cumRadius = minDR;
    // sort so the least resistant strain is innermost
    function numRes(v) { if (v[resistLabel]) {
            return v[resistLabel].split("").filter(function(d){return (d=='R')}).length
        } else {
            return 1;
        }
    };
    var sorted = d.value.sort(function(a,b) {return numRes(a)-numRes(b) });
    sorted.forEach(function(subnode, i) {
        var subResult = [];
        var outerRadius = Math.sqrt(areaScale(subnode[sizeLabel])/Math.PI+Math.pow(cumRadius,2));
        if (subnode[resistLabel]) {
            var letters = subnode[resistLabel].split("");
        } else {
            var letters = ["-"];
        }
        letters.forEach(function(letter, j) { 
            subResult.push({i: i, j: j,
                            subnode: subnode, 
                            letter:letter, 
                            tooltip:makeTooltipText(subnode, j),
                            innerR:cumRadius, 
                            outerR:outerRadius });
        });
        cumRadius = outerRadius+gap-1;
        // pie puts data into .data, adds value, startAngle, endAngle
        // append to end so result is a flat list
        result.push.apply(result,pie(subResult)); 
    });
    d.totalRadius = cumRadius;
    return result; 
}

function positionLegendLabel(text) {
    var eps = 5;
    // this only affects the position of the antibiotic names on the legend
    text.attr("transform", function(dd) { 
            var c = d3.svg.arc().centroid(dd);
            dd.data.outerR = maxDR/2 + Math.max(minDR, 15);
            dd.innerRadius = dd.data.outerR; // puts label more to outside
            dd.outerRadius = dd.data.outerR + 30 + ((Math.abs(c[0]) < eps) ? 10 : 0); // position middle-aligned text further out
            return "translate("+d3.svg.arc().centroid(dd)+")"; 
        })
        .attr("text-anchor", function(dd) {
            var c = d3.svg.arc().centroid(dd);
            if (c[0] > eps) {return "start";}
            if (c[0] < -eps) {return "end";}
            return "middle";
        });
}

function addNodeGraphic(g, gap, showTooltips) {
    // force works much faster if pie is calc once here
    // instead of every time inside nodeArcData
    var pie = d3.layout.pie().value(function(d){ return 1 });  

    // hole circle (allows for dragging from center)
    g.append("circle")
        .attr("class","hole")
        .attr("fill","white")
        .attr("r",minDR);
    // pie chart
    var newSegs = g.selectAll(".segment")
        .data(function(d) { return nodeArcData(d, gap, pie) }, function(dd) { return dd.key+","+dd.data.i+","+dd.data.j }) 
        .enter();
    var newPaths = newSegs.append("path")
        .attr("class","segment")
        .attr("stroke","white")
        .attr("stroke-width","1px")
        .attr("d", d3.svg.arc()
            .innerRadius(function(dd){ return dd.data.innerR })
            .outerRadius(function(dd){ return dd.data.outerR })
        )
        .style("fill", segmentFill);
    // three mouse events for tooltips on each arc segment
    if (showTooltips) {
        newPaths.on("mouseover", function() { 
                tooltip.transition().duration(250).style("opacity", 1); 
                d3.select(this).style("fill", "black"); 
            })
            .on("mousemove", function(dd) {
                var loc = tooltipX(d3.event, $('.tooltip').width());
                tooltip.html(dd.data.tooltip)
                    .style("left", loc.x + "px")
                    .style("top", (d3.event.pageY+5) + "px");
            })
            .on("mouseout", function() { 
                tooltip.transition().duration(250).style("opacity", 1e-6);
                d3.select(this).style("fill", segmentFill);
            });
    } else {
        // if no tooltips, then show the antibiotic name for the legend
        // shadow first, then real thing
        newText = newSegs.append("text")
            .attr("dy", ".35em")
            .attr("class","legend-label shadow")
            .attr("font-size","14px")  // include explicitly into svg so downloaded svg has it
            .attr("font-family","sans-serif")
            .attr("stroke","white")
            .attr("stroke-width","2.5px")
            .attr("opacity",".9")
            .text(function(dd) { return abNames[dd.data.j].capitalise() });
        positionLegendLabel(newText);
        newText = newSegs.append("text")
            .attr("dy", ".35em")
            .attr("class","legend-label")
            .attr("fill",labelColor)
            .attr("font-size","14px")
            .attr("font-family","sans-serif")
            .text(function(dd) { return abNames[dd.data.j].capitalise() });
        positionLegendLabel(newText);
    }
    // label & white shadow
    g.append("text")
        .attr("y", 0)
        .attr("dy", ".35em")
        .attr("text-anchor", "middle")
        .attr("class","node-label shadow")
        .attr("font-size","14px")  // include explicitly into svg so downloaded svg has it
        .attr("font-family","sans-serif")
        .attr("stroke","white")
        .attr("stroke-width","2.5px")
        .attr("opacity",function(d){return (showNodeLabels ? 0.9 : 1e-6)})
        .text(function(d) { return d.key; });
    g.append("text")
        .attr("y", 0)
        .attr("dy", ".35em")
        .attr("text-anchor", "middle")
        .attr("fill",labelColor)
        .attr("class","node-label")
        .attr("opacity",function(d){return (showNodeLabels ? 1 : 1e-6)})
        .attr("font-size","14px")
        .attr("font-family","sans-serif")  // include explicitly into svg so downloaded svg has it
        .text(function(d) { return d.key; });
}

function calcAreaScale() {
    areaScale = d3.scale.linear()
        .domain([0,d3.max(forestForce.nodes(), function(d) { 
            return d3.max(d.value, function(dd) { return parseFloat(dd[sizeLabel])} )
        })])
        .range([minDR*minDR*Math.PI,maxDR*maxDR*Math.PI]);
}

function populateAndStartForest() {
    var svg = d3.select("#forest-viz svg")
    calcAreaScale();

    var link = svg.selectAll(".link")
        .data(forestForce.links(), function(d) { return d.source.key+"_"+d.target.key} )

    var newLink = link.enter()
        .append("line")
        .attr("class","link")
        .attr("stroke", forestLinkColor)
        .attr("stroke-width", "2.5px")
        .attr("marker-end","url(#arrowhead)");

    var node = svg.selectAll(".node")
        .data(forestForce.nodes(), function(d) { return d.key; })

    var nodeGroup = node.enter()
        .append("g")
        .attr("class","node")
        .on("click", forestClick)
        .call(forestForce.drag);

    readForestNodeParams();

    addNodeGraphic(nodeGroup, gap, true);

    node.exit().remove();
    link.exit().remove();

    // ugly - waits for data before setting arrowhead color - can't get color too early
    svg.selectAll("#arrowhead").attr("fill",forestLinkColor);

    forestForce.start();
}

function displayNodeNumberingInfo(force) {
    var ignore = [subNodeIdLabel, initx, inity, superNodeIdLabel];
    var lines = [];
    if (force.nodes().length==0) 
        return;
    var thead = "<thead><tr><th>Node</th>";
    // TODO: this assumes every subnode has the exact same fields in the same order - 
    //       should be ok, but could be made more robust
    var value = force.nodes()[0].value[0];
    for (var field in value) {
        if (value.hasOwnProperty(field) && ignore.indexOf(field)===-1) {
            thead += "<th>" + field.capitalise().replace("Id","ID List")
                                    .replace("Mlva","MLVA")
                                    .replace("Spol","Spoligotypes")
                                    .replace("Res","Resistance") + "</th>";
        }
    }
    thead += "</tr></thead>";
    force.nodes().forEach(function(node) {
        var nodeKey = node.key;
        node.value.forEach(function(value) {
            var html = "<th>" + nodeKey + "</th>";
            nodeKey = ""; // don't show the same node number on subsequent lines
            for (var field in value) {
                if (value.hasOwnProperty(field) && ignore.indexOf(field)===-1) {
                    html += "<td>" + value[field] + "</td>";
                }
            }
            html += "</td>";
            lines.push(html);
        });
    });
    if (lines.length>0) {
        $('#node-numbering-info').html("<table>"+thead+"<tbody><tr>"+lines.join("</tr><tr>")+"</tr></tbody></table>");
    }
}


function mapLinkIdsToNodes(linkIds, force) {
    // a separate routine so we can save links to node objects once both link and node data are read in
    function subNodeIdList(node) {
        var ids = [];
        node.value.forEach(function(subnode) {
            ids.push(subnode[subNodeIdLabel]);
        });
        return ids;
    }
    linkIds.forEach(function(linkId) {
        // change parseInt(d.value[subNodeIdLabel]) to d.key to use supernode ids instead
        src = force.nodes().filter(function(d){return (subNodeIdList(d).indexOf(linkId[sourceLabel])>=0) ? d : null})[0]
        tgt = force.nodes().filter(function(d){return (subNodeIdList(d).indexOf(linkId[targetLabel])>=0) ? d : null})[0]
        if (src && tgt) {
            force.links().push({source: src, target: tgt});
        }
    });
}

function fixInOriginalPos(force, svg) {
    // eg. call as:  fixInOriginalPos(forestForce, d3.select("#forest-viz svg"))
    // you may need to then call forestTick() or mstTick()
    var margin = {top: 90, bottom: -5, left: 2, right: 2};
    var minX = d3.min(force.nodes(), function(d) { return +d.value[0][initx]; }),
        maxX = d3.max(force.nodes(), function(d) { return +d.value[0][initx]; }),
        minY = d3.min(force.nodes(), function(d) { return +d.value[0][inity]; }),
        maxY = d3.max(force.nodes(), function(d) { return +d.value[0][inity]; }),
        scaleX = d3.scale.linear().domain([minX, maxX]).range([maxDR+margin.left, parseInt(svg.style("width"))-maxDR-margin.right]),
        scaleY = d3.scale.linear().domain([maxY, minY]).range([maxDR+margin.top, parseInt(svg.style("height"))-maxDR-margin.bottom]);
    $.each(force.nodes(), function() {
        var node = this;
        if ((typeof node.value[0][initx] !== 'undefined') && (typeof node.value[0][inity] !== 'undefined')) {
            node.px = node.x = scaleX(+node.value[0][initx]);
            node.py = node.y = scaleY(+node.value[0][inity]);
            this.fixed = true;
        }
    });
}

function fixForestInOriginalPos() {
    fixInOriginalPos(forestForce, d3.select("#forest-viz svg"));
    forestTick();
}

// Shiny integration below
Shiny.addCustomMessageHandler("forest_reset_force", resetForestForce);
Shiny.addCustomMessageHandler("forest_link_data", updateForestLinksData);
Shiny.addCustomMessageHandler("forest_node_data", updateForestNodesData);

function resetForestForce(message) {
    // nodeCircles = {};
    // node.remove();
    // link.remove();
    // svg.clear();
    var svg = d3.select("#forest-viz svg");
    nodes = [];
    links = [];
    forestForce.nodes(nodes);
    forestForce.links(links);
    // svg.clear();
    svg.selectAll('.legend-node').remove();
}

function updateForestLinksData(linkData) {
    readForestLinks = linkData;
    if (forestForce.nodes()) {
        mapLinkIdsToNodes(linkData, forestForce);
        populateAndStartForest();
        displayNodeNumberingInfo(forestForce);
    }
}

function updateForestNodesData(nodeData) {
    var nodeDict = {};
    nodeData.forEach(function(nodeDatum) {
        // add subnode data to supernode if already found, or else create it 
        if (nodeDict[nodeDatum[superNodeIdLabel]]) {
            nodeDict[nodeDatum[superNodeIdLabel]].push(nodeDatum);
        } else {
            nodeDict[nodeDatum[superNodeIdLabel]] = [nodeDatum];
        }
    });
    var nodeArray = d3.entries(nodeDict); // convert dict to array with key and value keys
    // if there is resistance info, need to read in the antibiotic names
    if (nodeArray[0].value[0][resistLabel]) {
            abNames = antibioticNames;
            forestForce.nodes().push.apply(forestForce.nodes(),nodeArray); // appends to end
            showResColorPickers(); // make sure the colour pickers are showing
            makeLegend(d3.select("#forest-viz svg"), gap, forestForce, calcAreaScale, readForestNodeParams);
            if (readForestLinks) {
                mapLinkIdsToNodes(readForestLinks, forestForce);
                populateAndStartForest();
                displayNodeNumberingInfo(forestForce);
            }
        // });
    } else {
        // if no res info, hide and rename colour pickers
        hideResColorPickers();
        // and make the force graph now
        forestForce.nodes().push.apply(forestForce.nodes(),nodeArray); // appends to end
        if (readForestLinks) {
            mapLinkIdsToNodes(readForestLinks, forestForce);
            populateAndStartForest();
            displayNodeNumberingInfo(forestForce);
        }
    }
}
// End Shiny integration



// Legend

makeLegend = function(svg, gap, whichForce, calcScale, readParams) {
    var legendWidth = 274,
        legendHeight = 134,
        legendX = (parseInt(svg.style("width"))-150),
        legendY = 75; // (parseInt(svg.style("height"))-500)/2+90; // assumes css limits height to 500
    calcScale();
    // TODO: could find the biggest supernode and use it for the legend
    var legSubnode = [{}];
    legSubnode[0][superNodeIdLabel] = '1';
    legSubnode[0][subNodeIdLabel] = '__legend__1';
    legSubnode[0][sizeLabel] = areaScale.domain()[1]; // make it the biggest
    //var outerRadius = Math.sqrt(areaScale(subnode[sizeLabel])/Math.PI+Math.pow(minDR,2));
    if (whichForce.nodes()[0].value[0][resistLabel]) {
        var numRes = whichForce.nodes()[0].value[0][resistLabel].length;
        var resStr = new Array(numRes).join('S');
        resStr+='R';
        legSubnode[0][resistLabel] = resStr;
    }

    var legendData = { key:'1', value:legSubnode };
    var node = svg.selectAll(".legend-node")
        .data([legendData], function(d) { return "__legend__"; })

    legendDrag = d3.behavior.drag()
                    //.origin(Object)
                    .on("drag", function(d) {
                        d3.select(this).attr("transform", function(d) { 
                            return "translate("+(d3.event.x)+","+(d3.event.y)+")"; 
                        });
                    });

    var legendG = node.enter()
        .append("g")
        .attr("class","legend-node")
        .attr("transform", function(d) { return "translate("+(d.x||legendX)+","+(d.y||legendY)+")"; })
        .call(legendDrag);
    readParams();
    legendG.append("rect").attr("x", -(legendWidth/2))
                            .attr("y", -(legendHeight/2)+3)
                            .attr("width", legendWidth)
                            .attr("height", legendHeight)
                            .attr("fill","white")
                            .attr("stroke", "gray");
    addNodeGraphic(legendG, gap, false);
}

//  BUTTONS

shakeUpForest = function() {
    forestForce.nodes().forEach(function(node) {
        delete node.x, node.y;
        node.fixed = false;
    });
    forestForce.start();
}

// DOCUMENT MANAGEMENT

// $("#colors input").spectrum({
//     showPalette: true,
//     palette: [
//         ['black', 'white', 'blanchedalmond'],
//         ['rgb(255, 128, 0);', 'hsv 100 70 50', 'lightyellow']
//     ]
// });
function hideResColorPickers() {
    $("#color-R-div").hide();
    $("#color-S-div").hide();
    $("#color-unknown-name").text('Nodes');
}
function showResColorPickers() {
    $("#color-R-div").show();
    $("#color-S-div").show();
    $("#color-unknown-name").text('Unknown');
}
function segmentFill(dd){ 
    var picker = $("#color-"+dd.data.letter.toUpperCase());
    if (picker.length==0) { picker = $("#color-unknown"); }
    return picker.spectrum("get");
}
function forestLinkColor() {
    return $("#color-link").spectrum("get");
}

function setForestColors(color) {
    var svg = d3.select("#forest-viz svg")
    svg.selectAll(".node, .legend-node").selectAll('.segment')
        .transition()
        .duration(100)
        .style("fill", segmentFill);
    svg.selectAll(".link").transition().duration(100).attr("stroke", forestLinkColor);
    svg.selectAll("#arrowhead").transition().duration(100).attr("fill",forestLinkColor);
}

function readForestNodeParams() {
    gap = parseInt(document.getElementById('slider-gap').value);
    maxDR = parseInt(document.getElementById('slider-outerR').value);
    minDR = parseInt(document.getElementById('slider-innerR').value);
    showNodeLabels = $('#show-node-labels').prop('checked');
}

function redrawForestNodes() {
    svg = d3.select("#forest-viz svg");
    readForestNodeParams();
    areaScale = areaScale.range([minDR*minDR*Math.PI,maxDR*maxDR*Math.PI]);
    var pie = d3.layout.pie().value(function(d){ return 1 });
    svg.selectAll(".node, .legend-node").selectAll(".segment")
        .data(function(d) { return nodeArcData(d, gap, pie) }, function(dd) { return dd.key+","+dd.data.i + "," + dd.data.j })
        .attr("d", d3.svg.arc()
            .innerRadius(function(dd){ return dd.data.innerR })
            .outerRadius(function(dd){ return dd.data.outerR })
        );
    svg.selectAll("text.node-label").attr("opacity",function(d){return (showNodeLabels ? 1 : 1e-6)});
    svg.selectAll("text.node-label.shadow").attr("opacity",function(d){return (showNodeLabels ? .9 : 1e-6)});
    svg.selectAll(".hole").attr("r", minDR);

    positionLegendLabel(svg.selectAll("text.legend-label"));
    forestTick();
}

function changeForestNodeAppearance(sliderId, textId) {
    if (sliderId=='slider-charge') {
        forestForceCharge = parseInt(document.getElementById(sliderId).value);
        forestForce = forestForce.charge(forestForceCharge);
        forestForce.start();
    } else {
        showSliderValue(sliderId, textId);
        redrawForestNodes();
    }   
}

function setMinInnerR(noRedraw) {
    readForestNodeParams();
    if (showNodeLabels) {
        $('#slider-innerR').prop("min",minInnerRwithLabels);
        $('#node-numbering-info').fadeIn();
    } else {
        $('#slider-innerR').prop("min",minInnerRwithoutLabels);
        $('#node-numbering-info').fadeOut();
    }
    if (!noRedraw) {
        changeForestNodeAppearance('slider-innerR','value-innerR');
        redrawForestNodes();
    }
}


$( document ).ready(function() {
    // set up the colour pickers
    $("#forest-display input[type=color]").spectrum({
        clickoutFiresChange: true,
        showInitial: true,
        showInput: true,
        preferredFormat: "hex6",
        showPalette: true,
        showSelectionPalette: true,
        palette: palette,
        //localStorageKey: "bf3H7",
        chooseText: "OK",
        cancelText: "Cancel",
        move: setForestColors,
        change: setForestColors,
        hide: setForestColors,
    });

    // set the min innerR
    setMinInnerR(true);
    // show the slider values
    showSliderValue('slider-gap','value-gap');
    showSliderValue('slider-outerR','value-outerR');
    showSliderValue('slider-innerR','value-innerR');

    // check if the viz has been resized
    setInterval(function(){
        var viz = $("#forest-viz"),
            svg = $("#forest-viz svg"),
            vizWidth = viz.width(),
            vizHeight = viz.height(),
            svgWidth = svg.width(),
            svgHeight = svg.height(),
            scrollBarSize = 24;
        var vizBiggerThanSvg = (vizWidth>svgWidth+scrollBarSize) || (vizHeight>svgHeight+scrollBarSize),
            svgWiderThanMinAndViz = (svgWidth>minWidth) && (svgWidth>vizWidth),
            svgTallerThanMinAndViz = (svgHeight>minHeight) && (svgHeight>vizHeight),
            svgBiggerThanMinAndViz = svgWiderThanMinAndViz || svgTallerThanMinAndViz;
        if (vizBiggerThanSvg) {
            svg.width(Math.max(minWidth, vizWidth-scrollBarSize))
                .height(Math.max(minHeight,vizHeight-scrollBarSize));
            forestForce.size([svg.width(), svg.height()]);
            forestForce.start();
        } else if (svgBiggerThanMinAndViz) {
            svg.width(Math.max(minWidth, vizWidth-scrollBarSize))
                .height(Math.max(minHeight, vizHeight-scrollBarSize));
            forestForce.size([svg.width(), svg.height()]);
            forestForce.start();
        }
    }, 800);
});

