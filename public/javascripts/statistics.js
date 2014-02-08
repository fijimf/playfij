/**
 * Created by Jim on 1/22/14.
 */

function loadStats(key) {
    var jsonUrl = "/api/stats/" + key;
    $.getJSON(jsonUrl, {},
        function (json) {
            if (json["status"] == "ok") {
                var name = json["statistic"].name;
                $("#statDisplay").html("<h3>" + name + "</h3>");
                var dates = json.statistic.data.map(function (e) {
                    return e.date
                });
                var teamData = json.teams;
                var pctileFormat= d3.format("6.3f");
                var zFormat= d3.format(" 5.3f");
                var valueFormat = d3.format(json.statistic.shortFormat.replace("%",""));
                var dateSlider = $("#dateSlider");
                dateSlider.attr({
                    min: 0,
                    max: dates.length - 1,
                    step: 1
                });
                dateSlider.change(function () {
                    $("#dateShown").text(dates[this.value]);
                    var statistic = json.statistic.data[this.value];
                    var dataset = json.statistic.data[this.value].observations;

                    svgContainer = d3.select("#statPanel svg")
                        .attr("width", 900)
                        .attr("height", 2 + dataset.length * 31);


                    var boxes = svgContainer.selectAll("rect.statBox").data(dataset, function(d){return d.teamKey})
                    boxes.enter().append("rect")
                        .attr("class","statBox")
                        .attr("x", function () { return 25; })
                        .attr("y", function (d, i) { return (1+i) * 32; })
                        .attr("width", function () { return 750; })
                        .attr("height", function () { return 30; })
                        .style("fill", function (d) { return teamData[d.teamKey].c2; });
                    boxes.attr("y", function (d, i) { return (1+i) * 32; })
                    boxes.exit().remove()

                    var highlights = svgContainer.selectAll("rect.statHiLite").data(dataset, function(d){return d.teamKey})
                    highlights.enter().append("rect")
                        .attr("class","statHiLite")
                        .attr("x", function () { return 25; })
                        .attr("y", function (d, i) { return (1+i) * 32; })
                        .attr("width", function () { return 25; })
                        .attr("height", function () { return 30; })
                        .style("fill", function (d) { return teamData[d.teamKey].c1; });
                    highlights.attr("y", function (d, i) { return (1+i) * 32; })
                    highlights.exit().remove()

                    var teams =  svgContainer.selectAll("text.statTeam").data(dataset, function(d){return d.teamKey})
                    teams.enter().append("a")
                        .attr("xlink:href", function(d){return "/deepfij/teams/"+ d.teamKey;})  // <-- reading the new "url" property
                        .append("text")
                        .attr("class","statTeam")
                        .attr("x", function () { return 130; })
                        .attr("y", function (d, i) { return 21 + (1+i) * 32; })
                        .text(function (d) { return teamData[d.teamKey].name; });
                    teams.exit().remove()
                    teams.attr("y", function (d, i) { return 21 + (1+i) * 32; })

                    var ranks =  svgContainer.selectAll("text.statRank").data(dataset, function(d){return d.teamKey})
                    ranks.enter().append("text")
                        .attr("class","statRank")
                        .attr("x", function () { return 75; })
                        .attr("y", function (d, i) { return 21 + (1+i) * 32; })
                        .attr("text-anchor","end")
                        .text(function (d) { return d.rank; });
                    ranks.exit().remove()
                    ranks.attr("y", function (d, i) { return 21 + (1+i) * 32; })
                        .text(function (d) { return d.rank; });

                    var values =  svgContainer.selectAll("text.statValue").data(dataset, function(d){return d.teamKey})
                    values.enter().append("text")
                        .attr("class","statValue")
                        .attr("x", function () { return 500; })
                        .attr("y", function (d, i) { return 21 + (1+i) * 32; })
                        .attr("text-anchor","end")
                        .text(function (d) { return valueFormat(d.value); });
                    values.exit().remove()
                    values.attr("y", function (d, i) { return 21 + (1+i) * 32; })
                        .text(function (d) { return valueFormat(d.value); });


                    var percentiles =  svgContainer.selectAll("text.statPctile").data(dataset, function(d){return d.teamKey})
                    percentiles.enter().append("text")
                        .attr("class","statPctile")
                        .attr("x", function () { return 600; })
                        .attr("y", function (d, i) { return 21 + (1+i) * 32; })
                        .attr("text-anchor","end")
                        .text(function (d) { return pctileFormat((dataset.length-d.rank)/dataset.length); });
                    percentiles.exit().remove()
                    percentiles.attr("y", function (d, i) { return 21 + (1+i) * 32; })
                        .text(function (d) { return pctileFormat((dataset.length-d.rank)/dataset.length); });

                    var zScores =  svgContainer.selectAll("text.statZScore").data(dataset, function(d){return d.teamKey})
                    zScores.enter().append("text")
                        .attr("class","statZScore")
                        .attr("x", function () { return 700; })
                        .attr("y", function (d, i) { return 21 + (1+i) * 32; })
                        .attr("text-anchor","end")
                        .text(function (d) { return zFormat((d.value-statistic.mean)/statistic.stdDev); });
                    zScores.exit().remove()
                    zScores.attr("y", function (d, i) { return 21 + (1+i) * 32; })
                        .text(function (d) { return zFormat((d.value-statistic.mean)/statistic.stdDev); });

                });
                dateSlider.attr({value: 0});
                dateSlider.change();
                dateSlider.css("visibility","visible");
            } else {
                $("#statDisplay").html("<p>There was an error loading stats for the key " + key + "</p>");
            }

        }
    );
}

function loadGamesScatter(key) {
    var jsonUrl = "/api/games";
    $.getJSON(jsonUrl, {},
        function (json) {
            if (json["status"] == "ok") {

                var teamData = json.games;
                var svgContainer = d3.select("#scatter").append("svg")
                    .attr("width", 900)
                    .attr("height", 900);


                var xScale = d3.scale.linear().domain([20,140]).range([20,880]);
                var yScale = d3.scale.linear().domain([20,140]).range([880,20]);
                var xAxis = d3.svg.axis().scale(xScale).orient("bottom").innerTickSize(3).outerTickSize(3);
                var yAxis = d3.svg.axis().scale(yScale).orient("left").innerTickSize(3).outerTickSize(3);

                svgContainer.append("polygon")
                    .attr("points",
                        xScale(20)+","+yScale(20) +" " +
                            xScale(20)+","+yScale(140) +" " +
                            xScale(140)+","+yScale(140) +" " +
                            xScale(140)+","+yScale(20) +" ")
                    .attr("fill","#ddd");

                var dots = svgContainer.selectAll("circle").data(teamData);
                dots.enter().append("circle")
                    .attr("cx", function (d) {
                        return xScale(d.hs);
                    })
                    .attr("cy", function (d) {
                        return yScale(d.as);
                    })
                    .attr("r", 3)
                    .attr("stroke", "black")
                    .attr("stroke-width", 0)
                    .attr("fill", "rgba(16 ,192,16,0.33)");

                svgContainer.append("g")
                    .attr("class", "xAxis")
                    .attr("transform", "translate(0,880)")
                    .call(xAxis);
                svgContainer.append("g")
                    .attr("class", "yAxis")
                    .attr("transform", "translate(20,0)")
                    .call(yAxis);
            }

        });
}
