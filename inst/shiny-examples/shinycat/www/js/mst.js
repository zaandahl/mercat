//
// Global variable definitions
// (despite the var in front)
//

var mstSubNodeIdLabel = "name",
    mstSuperNodeIdLabel = "groupID",
    mstSourceLabel = "from",
    mstWeightLabel = "weight",
    mstTargetLabel = "to";
    mstResistLabel = "RES",
    mstSizeLabel   = "COUNT";

var readMSTLinks = false;
var mstAreaScale,
    mstLinkScale;
var mstLinkLength = 100,
    maxMSTLinkThickness = 10,
    mstAbNames = [];
var showMSTNodeLabels;

var minMSTInnerRwithLabels = 10,
    minMSTInnerRwithoutLabels = 0;

var mstForce = d3.layout.force()
    .size([width, height])
    .charge(-150) //function(d) { return -20*rScale(d[sizeLabel]); })
    //.gravity(0.1)
    .linkDistance( function(link) { return mstLinkLength } )
    // .linkStrength(1)
    .on("tick", mstTick);

mstForce.drag().on("dragstart", function(d) { d.fixed=true; } ); // once dragged, set fixed

//
// End of global variable definitions (now just global functions...)
//

d3.select("#mst-viz").append("svg")
    .attr("width", width)
    .attr("height", height);
    // .attr("viewBox", "0 0 " + width + " " + height )
    // .attr("preserveAspectRatio", "xMidYMid meet");


function mstTick(e) {
    // TODO: don't calculate each point twice
    d3.selectAll("#mst-viz svg .link")
        .attr("x1", function(d) { return circleLineIntsn(d.target, d.source, minMSTR+linkGap).x; })
        .attr("y1", function(d) { return circleLineIntsn(d.target, d.source, minMSTR+linkGap).y; })
        .attr("x2", function(d) { return circleLineIntsn(d.source, d.target, minMSTR+linkGap).x; })
        .attr("y2", function(d) { return circleLineIntsn(d.source, d.target, minMSTR+linkGap).y; });
    d3.selectAll("#mst-viz svg .node")
        .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
    d3.selectAll("#mst-viz svg .link-blot")
        .attr("cx", function(d) { return (d.source.x + d.target.x)/2.; })
        .attr("cy", function(d) { return (d.source.y + d.target.y)/2.; });
    d3.selectAll("#mst-viz svg .link-label")
        .attr("x", function(d) { return (d.source.x + d.target.x)/2.; })
        .attr("y", function(d) { return (d.source.y + d.target.y)/2.; });

}
function mstClick(d) {
    if (!d3.event.defaultPrevented) {  // ignore drag
        d.fixed = false; 
    }
}

function makeMSTTooltipText(subnode, resNum) {
    //node = d.value[0]
    var txt = "";
    if (mstAbNames.length>0) {
        txt = "<p><span>Antibiotic</span>"+mstAbNames[resNum]+"</p>";
    }
    return tooltipTextFromSubNode(subnode)+txt;
}


function mstNodeArcData(d, pie) {
    // given a node, return data to bind as arcs for the pie chart
    var result = [];
    var cumRadius = minMSTR;
    // sort so the least resistant strain is innermost
    function numRes(v) { if (v[mstResistLabel]) {
            return v[mstResistLabel].split("").filter(function(d){return (d=='R')}).length
        } else {
            return 1;
        }
    };
    var sorted = d.value.sort(function(a,b) {return numRes(a)-numRes(b) });
    sorted.forEach(function(subnode, i) {
        var subResult = [];
        var outerRadius = Math.sqrt(mstAreaScale(subnode[mstSizeLabel])/Math.PI+Math.pow(cumRadius,2))
        if (subnode[mstResistLabel]) {
            var letters = subnode[mstResistLabel].split("");
        } else {
            var letters = ["-"];
        }
        letters.forEach(function(letter, j) { 
            subResult.push({i: i, j: j,
                            subnode: subnode, 
                            letter:letter, 
                            tooltip:makeMSTTooltipText(subnode, j),
                            innerR:cumRadius, 
                            outerR:outerRadius });
        });
        cumRadius = outerRadius+mstGap-1;
        // pie puts data into .data, adds value, startAngle, endAngle
        // append to end so result is a flat list
        result.push.apply(result,pie(subResult)); 
    });
    d.totalRadius = cumRadius;
    return result;
}

function addMSTNodeGraphic(g) {
    // pie chart
    var pie = d3.layout.pie().value(function(d){ return 1 });

    // hole circle (allows for dragging from center)
    g.append("circle")
        .attr("class","hole")
        .attr("fill","white")
        .attr("r",minMSTR);

    var newSegs = g.selectAll(".segment")
        .data(function(d) { return mstNodeArcData(d, pie) }, function(dd) { return dd.key+","+dd.data.i+","+dd.data.j  }) 
        .enter();
    var newPaths = newSegs.append("path")
        .attr("class","segment")
        .attr("stroke","white")
        .attr("stroke-width","1px")
        .attr("d", d3.svg.arc()
            .innerRadius(function(dd){ return dd.data.innerR })
            .outerRadius(function(dd){ return dd.data.outerR })
        )
        .style("fill", mstSegmentFill);

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
        .attr("opacity",function(d){return (showMSTNodeLabels ? 0.9 : 1e-6)})
        .text(function(d) { return d.key; })
    g.append("text")
        .attr("y", 0)
        .attr("dy", ".35em")
        .attr("text-anchor", "middle")
        .attr("fill",labelColor)
        .attr("class","node-label")
        .attr("font-size","14px")  // include explicitly into svg so downloaded svg has it
        .attr("font-family","sans-serif")
        .attr("opacity",function(d){return (showMSTNodeLabels ? 1 : 1e-6)})
        .text(function(d) { return d.key; })

    // three mouse events for tooltips on each arc segment
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
            d3.select(this).style("fill", mstSegmentFill);
        });

}

function calcMSTScales() {
    mstAreaScale = d3.scale.linear()
        .domain([0,d3.max(mstForce.nodes(), function(d) { 
            return d3.max(d.value, function(dd) { return parseFloat(dd[mstSizeLabel])} )
        })])
        .range([minMSTR*minMSTR*Math.PI,maxMSTR*maxMSTR*Math.PI]);

    mstLinkScale = d3.scale.linear()
        .domain( [ 0,d3.max(mstForce.links(), function(d) { return d[mstWeightLabel] }) ] )
        .range([1,maxMSTLinkThickness]);
}


function populateAndStartMST() {
    var svg = d3.select("#mst-viz svg");

    readMSTNodeParams();
    calcMSTScales();

    var link = svg.selectAll(".link")
        .data(mstForce.links(), function(d) { return d.source.key+"_"+d.target.key} )

    link.enter()
        .append("line")
        .attr("class","link")
        .attr("stroke", mstLinkColor)
        .attr("stroke-width", function(d) { return mstLinkScale(d[mstWeightLabel])+"px" });
    // link.enter()  // Place label with the weight on each link, with white background
    //     .append("circle")
    //     .attr("class","link-blot")
    //     .attr("fill","white")
    //     // .attr("fill-opacity", 0.4)
    //     .attr("r",minMSTR)
    //     .attr("cx", function(d){ return 0 })
    //     .attr("cy", function(d){ return 0 });
    link.enter()
        .append("text")
        .attr("y", 0)
        .attr("dy", ".35em")
        .attr("text-anchor", "middle")
        .attr("class","link-label shadow")
        .attr("font-size","14px")  // include explicitly into svg so downloaded svg has it
        .attr("font-family","sans-serif")
        .attr("stroke","white")
        .attr("stroke-width","2.5px")
        .attr("opacity",".9")
        .text(function(d) { return d[mstWeightLabel]; })
    link.enter()
        .append("text")
        .attr("class","link-label")
        .attr("text-anchor", "middle")
        .attr("fill","#909090")
        .attr("font-size","14px")  // include explicitly into svg so downloaded svg has it
        .attr("font-family","sans-serif")
        .text(function(d) { return d[mstWeightLabel]; })
        .attr("dy", ".35em")
        .attr("x", function(d){ return 0 })
        .attr("y", function(d){ return 0 });

    var node = svg.selectAll(".node")
        .data(mstForce.nodes(), function(d) { return d.key; });

    var nodeGroup = node.enter()
        .append("g")
        .attr("class","node")
        .on("click", mstClick)
        .call(mstForce.drag);

    addMSTNodeGraphic(nodeGroup);

    node.exit().remove();
    link.exit().remove();

    mstForce.start();  // TODO: slows down the forest too much right now
}

function displayMSTNodeNumberingInfo(force) {
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
        $('#MST-node-numbering-info').html("<table>"+thead+"<tbody><tr>"+lines.join("</tr><tr>")+"</tr></tbody></table>");
    }
}


function mapMSTLinkIdsToNodes(linkIds, force) {
    // a separate routine so we can save links to node objects once both link and node data are read in

    function subNodeIdList(node) {
        var ids = [];
        node.value.forEach(function(subnode) {
            ids.push(subnode[mstSubNodeIdLabel]);
        });
        return ids;
    }
    linkIds.forEach(function(linkId) {
        src = force.nodes().filter(function(d){return (subNodeIdList(d).indexOf(linkId[mstSourceLabel])>=0) ? d : null})[0]
        tgt = force.nodes().filter(function(d){return (subNodeIdList(d).indexOf(linkId[mstTargetLabel])>=0) ? d : null})[0]
        if (src && tgt) {
            force.links().push({source: src, target: tgt, weight: linkId[mstWeightLabel] || 1});
        }
    });

    // linkIds.forEach(function(linkId) {
    //     // or use d.key instead of d.value[0][mstNodeIdLabel]
    //     src = force.nodes().filter(function(d){return (d.value[0][mstNodeIdLabel]==linkId[mstSourceLabel]) ? d : null})[0]
    //     tgt = force.nodes().filter(function(d){return (d.value[0][mstNodeIdLabel]==linkId[mstTargetLabel]) ? d : null})[0]
    //     if (src && tgt) {
    //         force.links().push({source: src, target: tgt, weight: linkId[mstWeightLabel] || 1});
    //     }
    // });
}

// Shiny integration below
Shiny.addCustomMessageHandler("mst_reset_force", resetMstForce);
Shiny.addCustomMessageHandler("mst_link_data", updateMstLinksData);
Shiny.addCustomMessageHandler("mst_node_data", updateMstNodesData);

function resetMstForce(message) {
    var svg = d3.select("#mst-viz svg");
    nodes = [];
    links = [];
    mstForce.nodes(nodes);
    mstForce.links(links);
    svg.selectAll('.legend-node').remove();
    svg.selectAll('.link-label').remove();
}


// get the links data;
function updateMstLinksData(linkData) {
    readMSTLinks = linkData;
    if (mstForce.nodes()) {
        mapMSTLinkIdsToNodes(readMSTLinks, mstForce);
        populateAndStartMST();
        displayMSTNodeNumberingInfo(mstForce);
    }
}


function updateMstNodesData(nodeData) {
    var nodeDict = {};
    nodeData.forEach(function(nodeDatum) {
        // TODO: clean this up - just copied from Forest so far
        // add subnode data to supernode if already found, or else create it 
        if (nodeDict[nodeDatum[mstSuperNodeIdLabel]]) {
            nodeDict[nodeDatum[mstSuperNodeIdLabel]].push(nodeDatum);
        } else {
            nodeDict[nodeDatum[mstSuperNodeIdLabel]] = [nodeDatum];
        }
    });
    var nodeArray = d3.entries(nodeDict); // convert dict to array with key and value keys
    
    // if there is resistance info, need to read in the antibiotic names
    if (nodeArray[0].value[0][mstResistLabel]) {
        // $.get(dataPath+fileStem+abSuffix, function(data) {
            // mstAbNames = data.split('\n').filter(function(e) {if (e) return e});
            mstAbNAmes = antibioticNames;
            mstForce.nodes().push.apply(mstForce.nodes(),nodeArray); // appends to end
            showMSTResColorPickers(); // make sure the colour pickers are showing
            makeLegend(d3.select("#mst-viz svg"), mstGap, mstForce, calcMSTScales, readMSTNodeParams);
            if (readMSTLinks) {
                mapMSTLinkIdsToNodes(readMSTLinks, mstForce);
                populateAndStartMST();
                displayMSTNodeNumberingInfo(mstForce);
            }
        // });
    } else {
        // if no res info, hide and rename colour pickers
        hideMSTResColorPickers();
        // and make the force graph now
        mstForce.nodes().push.apply(mstForce.nodes(),nodeArray); // appends to end
        if (readMSTLinks) {
            mapMSTLinkIdsToNodes(readMSTLinks, mstForce);
            populateAndStartMST();
            displayMSTNodeNumberingInfo(mstForce);
        }
    }

}





function mstSegmentFill(dd){ 
    var picker = $("#color-MST-"+dd.data.letter.toUpperCase());
    if (picker.length==0) { picker = $("#color-MST-unknown"); }
    return picker.spectrum("get"); 
};

function mstLinkColor() {
    return $("#color-MST-link").spectrum("get");
}


function setMSTColors(color) {
    var svg = d3.select("#mst-viz svg")
    svg.selectAll(".node").selectAll('.segment')
        .transition()
        .duration(100)
        .style("fill", mstSegmentFill);
    svg.selectAll(".link").transition().duration(100).attr("stroke", mstLinkColor);
}

function readMSTNodeParams() {
    maxMSTR = parseInt(document.getElementById('slider-MST-size').value);
    minMSTR = parseInt(document.getElementById('slider-MST-innerR').value);
    maxMSTLinkThickness = 1; //parseInt(document.getElementById('slider-MST-thickness').value);
    mstGap = parseInt(document.getElementById('slider-MST-gap').value);
    showMSTNodeLabels = $('#show-MST-node-labels').prop('checked');
}

function redrawMST() {
    svg = d3.select("#mst-viz svg")
    readMSTNodeParams();
    calcMSTScales();
    var pie = d3.layout.pie().value(function(d){ return 1 });
    svg.selectAll(".node").selectAll(".segment")
        .data(function(d) { return mstNodeArcData(d, pie) }, function(dd) { return dd.key+","+dd.data.i+","+dd.data.j  })
        .attr("d", d3.svg.arc()
            .innerRadius(function(dd){ return dd.data.innerR })
            .outerRadius(function(dd){ return dd.data.outerR })
        );
    svg.selectAll(".link").transition().duration(100)
        .attr("stroke-width", function(d) { return mstLinkScale(d[mstWeightLabel])+"px" });
    svg.selectAll("text.node-label").attr("opacity",function(d){return (showMSTNodeLabels ? 1 : 1e-6)});
    svg.selectAll("text.node-label.shadow").attr("opacity",function(d){return (showMSTNodeLabels ? .9 : 1e-6)});
    svg.selectAll(".hole").attr("r", minMSTR);
    mstTick();
}

function changeMSTNodeAppearance(sliderId, textId) {
    showSliderValue(sliderId, textId);
    if (sliderId=='slider-MST-length') {
        mstLinkLength = parseInt(document.getElementById(sliderId).value);
        mstForce.linkDistance( function(link) { return mstLinkLength } );
        //shakeUpMST();
    } else if (sliderId=='slider-MST-charge') {
        mstForceCharge = parseInt(document.getElementById(sliderId).value);
        mstForce = mstForce.charge(mstForceCharge);
        mstForce.start();
    } else {
        redrawMST();
    }
}

function setMSTMinInnerR(noRedraw) {
    readMSTNodeParams();
    if (showMSTNodeLabels) {
        $('#slider-MST-innerR').prop("min",minMSTInnerRwithLabels);
        $('#MST-node-numbering-info').fadeIn();
    } else {
        $('#slider-MST-innerR').prop("min",minMSTInnerRwithoutLabels);
        $('#MST-node-numbering-info').fadeOut();
    }
    if (!noRedraw) {
        changeMSTNodeAppearance('slider-MST-innerR','value-MST-innerR');
        redrawMSTNodes();
    }
}


//  BUTTONS

shakeUpMST = function() {
    mstForce.nodes().forEach(function(node) {
        delete node.x, node.y;
        node.fixed = false;
    });
    mstForce.start()
}

function fixMSTInOriginalPos() {
    fixInOriginalPos(mstForce, d3.select("#mst-viz svg"));
    mstTick();
}


function hideMSTResColorPickers() {
    $("#color-MST-R-div").hide();
    $("#color-MST-S-div").hide();
    $("#color-MST-unknown-name").text('Nodes');
}
function showMSTResColorPickers() {
    $("#color-MST-R-div").show();
    $("#color-MST-S-div").show();
    $("#color-MST-unknown-name").text('Unknown');
}

$( document ).ready(function() {
    // set up the colour pickers
    $("#mst-display input[type=color]").spectrum({
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
        move: setMSTColors,
        change: setMSTColors,
        hide: setMSTColors,
    });
    // set the min innerR
    setMSTMinInnerR(true);
    // show the slider values
    showSliderValue('slider-MST-gap','value-MST-gap');
    showSliderValue('slider-MST-innerR','value-MST-innerR');
    showSliderValue('slider-MST-size','value-MST-size');
    showSliderValue('slider-MST-length','value-MST-length');
    //showSliderValue('slider-MST-thickness','value-MST-thickness');

    // check if the viz has been resized
    setInterval(function(){
        var viz = $("#mst-viz"),
            svg = $("#mst-viz svg"),
            vizWidth = viz.width(),
            vizHeight = viz.height(),
            svgWidth = svg.width(),
            svgHeight = svg.height(),
            scrollBarSize = 20;
        var vizBiggerThanSvg = (vizWidth>svgWidth+scrollBarSize) || (vizHeight>svgHeight+scrollBarSize),
            svgWiderThanMinAndViz = (svgWidth>minWidth) && (svgWidth>vizWidth),
            svgTallerThanMinAndViz = (svgHeight>minHeight) && (svgHeight>vizHeight),
            svgBiggerThanMinAndViz = svgWiderThanMinAndViz || svgTallerThanMinAndViz;
        if (vizBiggerThanSvg) {
            svg.width(Math.max(minWidth, vizWidth-scrollBarSize))
                .height(Math.max(minHeight,vizHeight-scrollBarSize));
            mstForce.size([svg.width(), svg.height()]);
            mstForce.start();
        } else if (svgBiggerThanMinAndViz) {
            svg.width(Math.max(minWidth, vizWidth-scrollBarSize))
                .height(Math.max(minHeight, vizHeight-scrollBarSize));
            mstForce.size([svg.width(), svg.height()]);
            mstForce.start();
        }
    }, 800);
});


