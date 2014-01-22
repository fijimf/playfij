function deleteItem(key,keyType,domain){
    var hiddenId = '<input type="hidden" name="'+keyType+'" value="' + key + '"/>';
    var action = '/admin/'+domain+'/delete';
    $('<form>', { "html":hiddenId, "method":'POST', "action":action } ).appendTo(document.body).submit();
}

function loadQuote() {
    var jsonUrl = "/api/quote";
    $.getJSON(jsonUrl, {},
        function (json) {
                $("#qotd").html("<p class='quote'><a href='"+json.url+"' target='_blank' title='"+json.source+"' >"+json.quote+"</a></p>");
        }
    );
}

function loadStats(key) {
    var jsonUrl = "/api/stats/"+key;
    $.getJSON(jsonUrl, {},
        function (json) {
            if (json["status"]=="ok") {
                var name = json["statistic"].name;
                $("#statDisplay").html("<h3>"+name+"</h3>");
                var dates = json.statistic.data.map(function(e){return e.date});
                var dateSlider = $("#dateSlider");
                dateSlider.attr({
                    min: 0,
                    max: dates.length - 1,
                    step: 1
                });
                dateSlider.change(function() {
                    $("#dateShown").text(dates[this.value]);
                     var dataset = json.statistic.data[this.value].observations;
                     d3.select("#statPanel")
                        .append("table")
                        .style("border-collapse", "collapse")
                        .style("border", "2px black solid")

                        .selectAll("tr")
                        .data(dataset)
                        .enter().append("tr")

                        .selectAll("td")
                        .data(function(d){return [d.rank, d.team.name, d.value];})
                        .enter().append("td")
                        .style("border", "1px black solid")
                        .style("padding", "10px")
                        .text(function(d){return d;})
                        .style("font-size", "12px");
                });
                dateSlider.attr({value : 0});

            } else {
                $("#statDisplay").html("<p>There was an error loading stats for the key "+key+"</p>");
            }

        }
    );
}

function headlineTeam(src, teamName, nickname, confName, confWins, confLosses, wins, losses) {
    $("#headline").html("<h3><img style='height:70px; width:70px;' src='"+src+"' />"+teamName+"&nbsp;"+nickname+"</h3><h4>("+wins+"-"+losses+", "+confWins+"- "+confLosses+" "+confName+" )</h4>");
}

function dropHeadline() {
    $("#headline").html("<h3><img style='height:70px; width:70px;' src='/assets/images/blank.png' />&nbsp;</h3><h4>&nbsp;</h4>");
}