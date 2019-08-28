//var forestForce, mstForce;
var palette = [ // d3.scale.category20().range()];
    ["#DADAEB", "#BCBDDC", "#9E9AC8", "#756BB1", "#54278F", ],
    ["#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C", ],
    ["#CCECE6", "#99D8C9", "#66C2A4", "#2CA25F", "#006D2C", ],
    ["#C7E9C0", "#A1D99B", "#74C476", "#31A354", "#198D42", ],
    ["#FDD0A2", "#FDAE6B", "#FD8D3C", "#E6550D", "#A63603", ],
    //["#FFFFB2", "#FED976", "#FEB24C", "#F03B20", "#BD0026", ],
    //["#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", "#E34A33", "#B30000", ],
    ["#FCBBA1", "#FC9272", "#FB6A4A", "#DE2D26", "#A50F15", ],
    ["#D9D9D9", "#BDBDBD", "#969696", "#636363", "#252525", ]];

var tooltip = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 1e-6);

Shiny.addCustomMessageHandler("smef_uploaded", updateSmefStatus);
Shiny.addCustomMessageHandler("available_types", updateVisibleTabs);
Shiny.addCustomMessageHandler("ab_names", updateAntibioticNames);

function updateAntibioticNames(names) {
    antibioticNames = names;
    console.log(antibioticNames);
}

function updateSmefStatus(message) {
    console.log("Updating smef status");
    // smefUploaded = true;
    // smefAnalysed = true;
    $('#forest-popover').hide();
    $('#mst-popover').hide();
    $('#resfreq-popover').hide();
    $('#resld-popover').hide();
    $('#mlvafreq-popover').hide();
    $('#spolfreq-popover').hide();
}

function updateVisibleTabs(types) {
    console.log("Updating tabs");
    console.log(types);
    $('a[href="#drawspol-tab"]').parent().addClass('hidden');
    $('a[href="#resplot-tab"]').parent().addClass('hidden');
    $('a[href="#respairplot-tab"]').parent().addClass('hidden');
    $('a[href="#plotmlva-tab"]').parent().addClass('hidden');
    $('a[href="#forest-tab"]').parent().addClass('hidden');
    $('a[href="#tree-tab"]').parent().addClass('hidden');
    $('a[href="#summary-tab"]').parent().removeClass('hidden');
    $('a[href="#data-tab"]').parent().removeClass('hidden');
    if($.inArray('spol', types)>=0) {
        $('a[href="#drawspol-tab"]').parent().removeClass('hidden');
        // load the forest js, show the forest tab, and make it the active pane
        $.getScript('js/forest.js').done(function(script, textStatus) {
            $('a[href="#forest-tab"]').parent().removeClass('hidden');
            $('a[href=#forest-tab]').tab('show');
            var vizHeight = $("#forest-viz").height();
            $('#forest-viz').scrollTop((height-vizHeight)/2);
        }).fail(function(jqxhr, settings, exception) {
            console.log( "Error loading forest script. "+exception);
        });
    }
    if($.inArray('res', types)>=0) {
        $('a[href="#resplot-tab"]').parent().removeClass('hidden');
        $('a[href="#respairplot-tab"]').parent().removeClass('hidden');
    }
    if($.inArray('mlva', types)>=0) {
       $('a[href="#plotmlva-tab"]').parent().removeClass('hidden'); 
    }
    if($.inArray('mlva', types)>=0 || $.inArray('spol', types)>=0 || $.inArray('snp', types)>=0) {
        // load the MST
        $.getScript('js/mst.js').done(function(script, textStatus) {
            $('a[href="#tree-tab"]').parent().removeClass('hidden');
            var vizHeight = $("#mst-viz").height();
            $('#mst-viz').scrollTop((height-vizHeight)/2);
        }).fail(function(jqxhr, settings, exception) {
            console.log( "Error loading MST script. "+exception);
        });
    }   
}

function setResBarColors(color) {
    Shiny.onInputChange("resbar_color", color.toHexString());
}

function setResLDHighColor(color) {
    Shiny.onInputChange("resld_color_high", color.toHexString());
}

function setResLDLowColor(color) {
    Shiny.onInputChange("resld_color_low", color.toHexString());
}

function setResLDNAColor(color) {
    Shiny.onInputChange("resld_color_na", color.toHexString());
}

function setSpolFreqHighColor(color) {
    Shiny.onInputChange("spolfreq_color_high", color.toHexString());
}

function setSpolFreqLowColor(color) {
    Shiny.onInputChange("spolfreq_color_low", color.toHexString());
}

function setMLVAFreqHighColor(color) {
    Shiny.onInputChange("mlvafreq_color_high", color.toHexString());
}

function setMLVAFreqLowColor(color) {
    Shiny.onInputChange("mlvafreq_color_low", color.toHexString());
}

function changeResBarLeg(sliderId, textId) {
    showSliderValue(sliderId, textId);
    if (sliderId=='slider-resbar-legx') {
        xval = parseInt(document.getElementById(sliderId).value);
        Shiny.onInputChange("resbar_legx", xval);
    } else {
        yval = parseInt(document.getElementById(sliderId).value);
        Shiny.onInputChange("resbar_legy", yval);
    }
}

function setResBarOrderBy() {
    var valueChecked = $('#show-resbar-order').prop('checked');
    if(valueChecked) {
        Shiny.onInputChange("resbar_orderby", true);
    } else {
        Shiny.onInputChange("resbar_orderby", false);
    }
}

function setResBarRevOrder() {
    var valueChecked = $('#show-resbar-rev').prop('checked');
    if(valueChecked) {
        Shiny.onInputChange("resbar_revorder", true);
    } else {
        Shiny.onInputChange("resbar_revorder", false);
    }
}

function setResBarFlip() {
    var valueChecked = $('#show-resbar-flip').prop('checked');
    if(valueChecked) {
        Shiny.onInputChange("resbar_flip", true);
    } else {
        Shiny.onInputChange("resbar_flip", false);
    }
}

function setResBarLegend() {
    var valueChecked = $('#show-resbar-legend').prop('checked');
    if(valueChecked) {
        Shiny.onInputChange("resbar_legend", true);
    } else {
        Shiny.onInputChange("resbar_legend", false);
    }
}

function setResCorValues() {
    var valueChecked = $('#show-resld-values').prop('checked');
    if(valueChecked) {
        Shiny.onInputChange("resld_values", true);
    } else {
        Shiny.onInputChange("resld_values", false);
    }
}

function setResCorLegend() {
    var legendChecked = $('#show-resld-legend').prop('checked');
    if(legendChecked) {
        Shiny.onInputChange("resld_legend", true);
    } else {
        Shiny.onInputChange("resld_legend", false);
    }
}

function setResCorSym() {
    var symChecked = $('#show-resld-sym').prop('checked');
    if(symChecked) {
        Shiny.onInputChange("resld_sym", true);
    } else {
        Shiny.onInputChange("resld_sym", false);
    }
}

function setMLVAFreqValues() {
    var valueChecked = $('#show-mlvafreq-values').prop('checked');
    if(valueChecked) {
        Shiny.onInputChange("mlvafreq_values", true);
    } else {
        Shiny.onInputChange("mlvafreq_values", false);
    }
}

function setMLVAFreqLegend() {
    var legendChecked = $('#show-mlvafreq-legend').prop('checked');
    if(legendChecked) {
        Shiny.onInputChange("mlvafreq_legend", true);
    } else {
        Shiny.onInputChange("mlvafreq_legend", false);
    }
}


$( document ).ready(function() {

    console.log("In doc ready");
    console.log(smefUploaded);

    // set the subtitle
    //d3.select('#subtitle').html(fileStem); // TODO: get the name from the file
    // add a popover
    document.getElementById("stem").value = fullFileStem;

    $('#smef-popover').popover();
    $('#rsf-popover').popover();
    $('#svg-popover').popover();
    $('#svg-popover2').popover();
    $('#forest-popover').popover();
    $('#mst-popover').popover();
    $('#resfreq-popover').popover();
    $('#resld-popover').popover();
    $('#mlvafreq-popover').popover();
    $('#spolfreq-popover').popover();
    
    if (smefUploaded) {
        // hide the "what is a spoligoforest" and MST links
        $('#forest-popover').hide();
        $('#mst-popover').hide();
    }

    $('.display').hide();
    setTimeout(function() {
        $('.display').show(); // workaround - color inputs display wrongly initially, so hide them
    }, 500);

    $('.goto-upload').click(function() {
        $('a[href="#upload-tab"]').tab('show');
    });
    $('.goto-data').click(function() {
        if (smefUploaded) {
            window.open(sampleSmef,'_blank');
        } else {
            $('a[href="#data-tab"]').tab('show');
        }
    });

    // Resistance Frequency Colour Pickers

    $("#resplot-display input[type=color]").spectrum({
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
        move: setResBarColors,
        change: setResBarColors,
        hide: setResBarColors,
    });

    // Resistance Correlation Colour Pickers

    $("#color-resld-high-div input[type=color]").spectrum({
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
        move: setResLDHighColor,
        change: setResLDHighColor,
        hide: setResLDHighColor,
    });


    $("#color-resld-low-div input[type=color]").spectrum({
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
        move: setResLDLowColor,
        change: setResLDLowColor,
        hide: setResLDLowColor,
    });

    $("#color-resld-na-div input[type=color]").spectrum({
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
        move: setResLDNAColor,
        change: setResLDNAColor,
        hide: setResLDNAColor,
    });

    // Spoligotype Colour Pickers

    $("#color-spolfreq-high-div input[type=color]").spectrum({
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
        move: setSpolFreqHighColor,
        change: setSpolFreqHighColor,
        hide: setSpolFreqHighColor,
    });

    $("#color-spolfreq-low-div input[type=color]").spectrum({
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
        move: setSpolFreqLowColor,
        change: setSpolFreqLowColor,
        hide: setSpolFreqLowColor,
    });

    // MLVA Colour Pickers

    $("#color-mlvafreq-high-div input[type=color]").spectrum({
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
        move: setMLVAFreqHighColor,
        change: setMLVAFreqHighColor,
        hide: setMLVAFreqHighColor,
    });

    $("#color-mlvafreq-low-div input[type=color]").spectrum({
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
        move: setMLVAFreqLowColor,
        change: setMLVAFreqLowColor,
        hide: setMLVAFreqLowColor,
    });

});

// UTILITY FNS

// calculate the intersection of a line and circle centered on the end point,
// given start and end points s.x,s.y and t.x,t.y of line,
// and radius r of the circle
// returns an object with x,y keys
// of the two intersection pts, return the one closer to the start pt

circleLineIntsn = function(s,t,r) {
    if (t.x===s.x) { t.x += 0.001 } // TODO: write special case for t.x==s.x
    var m = (t.y-s.y)/(t.x-s.x);
    var b = (s.y - m*s.x);
    var B = b - t.y;
    var bq = t.x - m*B;
    var den = m*m+1;
    var q2 = bq*bq - den*(B*B+t.x*t.x-r*r);
    var q = Math.sqrt(q2);
    var xp = (bq + q)/den;
    var yp = m*xp + b;
    var xm = (bq - q)/den;
    var ym = m*xm + b;
    if (Math.abs(s.x-xp)<Math.abs(s.x-xm)) {
        return {x:xp, y:yp};
    } else {
        return {x:xm, y:ym};
    }
}

downloadSvg = function(viz) {
    // e.g. pass viz = "#forest-viz"
    var svg = d3.select(viz+" svg")
    var svg_xml = (new XMLSerializer).serializeToString(svg[0][0]);
    //console.log(svg_xml);
    // var form = document.getElementById("download-form");
    // form['data'].value = svg_xml;
    // form['dataname'].value = fileStem;
    // form.submit();
    var a = document.createElement('a');
    a.href = 'data:image/svg+xml; charset=utf8, ' + encodeURIComponent(svg_xml.replace(/></g, '>\n\r<'));
    a.download = 'network.svg';
    a.innerHTML = 'download the svg file';
    a.dispatchEvent(new MouseEvent('click', {bubbles: true, cancelable: true, view: window}));
}

function showSliderValue(sliderId, textId) {
    $(document.getElementById(textId)).text(document.getElementById(sliderId).value);
}

String.prototype.capitalise = function() {
    return this.charAt(0).toUpperCase()+this.slice(1).toLowerCase();
}

function tooltipTextFromSubNode(subnode) {
    // common to MST and spoligoforest
    var txt = "";
    for (var field in subnode) {
        if (subnode.hasOwnProperty(field) && field.toLowerCase()!='name' && field.toLowerCase()!='sid') {
            txt += "<p><span>"+field.replace("_"," ")
                                    .capitalise()
                                    .replace("Id","ID list")
                                    //.replace("id"," ID")
                                    .replace("Mlva","MLVA")
                                    .replace("Spol","Spoligotypes")
                                    .replace("Res","Resistance")
                              +"</span>"
                              +subnode[field]+"</p>";
        }
    };
    return txt;
}

function tooltipX(evt, tooltipWidth) {
    // returns topleft of tooltip location {x: X}
    // x: if it doesn't fit in the window, put it on the left edge;
    //    else if it fits to the right, put it there; 
    //    else if it fits to the left, put it there;
    //    else put it flush against the right edge.
    var margin = {top: 5, right: 35, bottom: 35, left: 5}; // room for scroll bars on right, bottom
    var x, y;

    if (tooltipWidth > window.innerWidth - margin.left - margin.right) {
        x = margin.left;
    } else if (evt.pageX + margin.left + tooltipWidth <= window.innerWidth - margin.right) {
        x = evt.pageX + margin.left;
    } else if (evt.pageX - margin.left - tooltipWidth - 16 >= margin.left) { // 16 for 8px padding * 2
        x = evt.pageX - margin.left - tooltipWidth - 16;
    } else {
        x = window.innerWidth - margin.right - tooltipWidth;
    }

    return {x: x};
}

