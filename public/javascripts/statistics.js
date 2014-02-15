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

function loadGamesScatter(data) {
    var svgContainer = d3.select("#scatterBox").append("svg")
        .attr("width", 680)
        .attr("height", 680);
    var mx = d3.max([d3.max(data, function(d){return d.hv;}),d3.max(data, function(d){return d.av;})]);
    var mn = d3.min([d3.min(data, function(d){return d.hv;}),d3.min(data, function(d){return d.av;})]);
    var xScale = d3.scale.linear().domain([mn, mx]).range([20, 660]);
    var yScale = d3.scale.linear().domain([mn, mx]).range([660, 20]);
    var xAxis = d3.svg.axis().scale(xScale).orient("bottom").innerTickSize(1).outerTickSize(1);
    var yAxis = d3.svg.axis().scale(yScale).orient("left").innerTickSize(1).outerTickSize(1);

    var dots = svgContainer.selectAll("circle").data(data);
    var jitter = d3.random.normal(0, 3);
    dots.enter().append("circle")
        .attr("cx", function (d) {
            return xScale(d.av)+jitter();
        })
        .attr("cy", function (d) {
            return yScale(d.hv)+jitter();
        })
        .attr("r", 3)
        .attr("stroke", "black")
        .attr("stroke-width", 0)
        .attr("fill", function(d) {if (d.mg<0) { return "rgba(192, 32, 32, 0.5)" } else {return "rgba(32,192,32,0.5)";}});

    svgContainer.append("g")
        .attr("class", "xAxis")
        .attr("transform", "translate(0,660)")
        .call(xAxis);
    svgContainer.append("g")
        .attr("class", "yAxis")
        .attr("transform", "translate(20,0)")
        .call(yAxis);

    var zContainer = d3.select("#zBox").append("svg")
        .attr("width", 680)
        .attr("height", 680);
    var zmx = d3.max([d3.max(data, function(d){return d.hz;}),d3.max(data, function(d){return d.az;})]);
    var zmn = d3.min([d3.min(data, function(d){return d.hz;}),d3.min(data, function(d){return d.az;})]);
    var zxScale = d3.scale.linear().domain([zmn, zmx]).range([20, 660]);
    var zyScale = d3.scale.linear().domain([zmn, zmx]).range([660, 20]);
    var zxAxis = d3.svg.axis().scale(zxScale).orient("bottom").innerTickSize(1).outerTickSize(1);
    var zyAxis = d3.svg.axis().scale(zyScale).orient("left").innerTickSize(1).outerTickSize(1);

    var zdots = zContainer.selectAll("circle").data(data);
    var zjitter = d3.random.normal(0, 3);
    zdots.enter().append("circle")
        .attr("cx", function (d) {
            return zxScale(d.az)+jitter();
        })
        .attr("cy", function (d) {
            return zyScale(d.hz)+jitter();
        })
        .attr("r", 3)
        .attr("stroke", "black")
        .attr("stroke-width", 0)
        .attr("fill", function(d) {if (d.mg<0) { return "rgba(192, 32, 32, 0.5)" } else {return "rgba(32,192,32,0.5)";}});

    zContainer.append("g")
        .attr("class", "xAxis")
        .attr("transform", "translate(0,660)")
        .call(zxAxis);
    zContainer.append("g")
        .attr("class", "yAxis")
        .attr("transform", "translate(20,0)")
        .call(zyAxis);
}

function loadHistBox() {
    var dataCells = d3.selectAll("td.statvalue")[0];
    var dd = dataCells.map(function(d){return parseFloat(d.innerText);});
    var formatCount = d3.format(",.0f");
    var svg = d3.select("#histBox").append("svg")
        .attr("width", 680)
        .attr("height", 400)
        .append("g")
        ;

    var x = d3.scale.linear()
        .domain([d3.min(dd), d3.max(dd)])
        .range([30, 660]);

    var data = d3.layout.histogram()
        .bins(x.nice().ticks(20))
        (dd);

    var y = d3.scale.linear()
        .domain([0, d3.max(data, function(d) { return d.y; })])
        .range([380, 20]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom").innerTickSize(1).outerTickSize(1);


    var bar = svg.selectAll(".bar")
        .data(data)
        .enter().append("g")
        .attr("class", "bar")
        .attr("transform", function(d) { return "translate(" + x(d.x) + "," + y(d.y) + ")"; });

    bar.append("rect")
        .attr("x", 1)
        .attr("width", x(data[1].x) - x(data[0].x))
        .attr("height", function(d) { return 380 - y(d.y); })
        .style("fill", "rgba(64, 64, 232, 0.6)");

    bar.append("text")
        .attr("dy", "-0.75em")
        .attr("y", 6)
        .attr("x", (x(data[1].x) - x(data[0].x)) / 2)
        .attr("text-anchor", "middle")
        .attr("class", "xAxis")
        .text(function(d) { return formatCount(d.y); });

    svg.append("g")
        .attr("class", "xAxis")
        .attr("transform", "translate(0,380)")
        .call(xAxis);
}

function loadSeriesBox(data) {
    var svgContainer = d3.select("#seriesBox").append("svg")
        .attr("width", 680)
        .attr("height", 400);
    var minDate = d3.time.format("%Y-%m-%d").parse(series.series[0].date);
    var maxDate = d3.time.format("%Y-%m-%d").parse(series.series[series.series.length - 1].date);
    var minValue = d3.min(series.series, function (d) {
        return d.min;
    });
    var maxValue = d3.max(series.series, function (d) {
        return d.max;
    });
    var margin = (maxValue - minValue) / 20;
    var xScale = d3.time.scale().domain([minDate, maxDate]).range([30, 660]);
    var yScale = d3.scale.linear().domain([minValue - margin, maxValue + margin]).range([380, 20]);
    var xAxis = d3.svg.axis().scale(xScale).orient("bottom").innerTickSize(1).outerTickSize(1);
    var yAxis = d3.svg.axis().scale(yScale).orient("left").innerTickSize(1).outerTickSize(1);

    var area1 = d3.svg.area()
        .x(function (d) {
            return xScale(d3.time.format("%Y-%m-%d").parse(d.date));
        })
        .y0(function (d) {
            return yScale(d.mean - d.stdDev);
        })
        .y1(function (d) {
            return yScale(d.mean + d.stdDev);
        });
    var area2 = d3.svg.area()
        .x(function (d) {
            return xScale(d3.time.format("%Y-%m-%d").parse(d.date));
        })
        .y0(function (d) {
            return yScale(d.mean - 2 * d.stdDev);
        })
        .y1(function (d) {
            return yScale(d.mean + 2 * d.stdDev);
        });
    var min = d3.svg.line()
        .x(function (d) {
            return xScale(d3.time.format("%Y-%m-%d").parse(d.date));
        })
        .y(function (d) {
            return yScale(d.min);
        });
    var median = d3.svg.line()
        .x(function (d) {
            return xScale(d3.time.format("%Y-%m-%d").parse(d.date));
        })
        .y(function (d) {
            return yScale(d.med);
        });
    var max = d3.svg.line()
        .x(function (d) {
            return xScale(d3.time.format("%Y-%m-%d").parse(d.date));
        })
        .y(function (d) {
            return yScale(d.max);
        });
    svgContainer.append("svg:path")
        .data([series.series])
        .attr("d", area1)
        .style("fill", "rgba(64, 64, 232, 0.2)");
    svgContainer.append("svg:path")
        .data([series.series])
        .attr("d", area2)
        .style("fill", "rgba(64, 64, 232, 0.2)");
    svgContainer.append("svg:path")
        .data([series.series])
        .attr("d", min)
        .style("stroke", "red")
        .style("stroke-width", "1")
        .style("fill","none");
    svgContainer.append("svg:path")
        .data([series.series])
        .attr("d", median)
        .style("stroke", "yellow")
        .style("stroke-width", "1")
        .style("fill","none");
    svgContainer.append("svg:path")
        .data([series.series])
        .attr("d", max)
        .style("stroke", "green")
        .style("stroke-width", "1")
        .style("fill","none");
    svgContainer.append("g")
        .attr("class", "xAxis")
        .attr("transform", "translate(0,380)")
        .call(xAxis);
    svgContainer.append("g")
        .attr("class", "yAxis")
        .attr("transform", "translate(30,0)")
        .call(yAxis);
}

function loadLogGraph(b1,b2) {

}
