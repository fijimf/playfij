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
                var pctileFormat= d3.format("6.3f");
                var zFormat= d3.format(" 5.3f");
                var dateSlider = $("#dateSlider");
                dateSlider.attr({
                    min: 0,
                    max: dates.length - 1,
                    step: 1
                });
                dateSlider.change(function () {
                    $("#dateShown").text(dates[this.value]);
                    var dataset = json.statistic.data[this.value].observations;

                    svgContainer = d3.select("#statPanel svg")
                        .attr("width", 900)
                        .attr("height", 4 + dataset.length * 32);


                    var boxes = svgContainer.selectAll("rect").data(dataset, function(d){return d.team.key})

                    boxes.enter().append("rect")
                        .attr("x", function () {
                            return 25;
                        })
                        .attr("y", function (d, i) {
                            return i * 32;
                        })
                        .attr("width", function () {
                            return 750;
                        })
                        .attr("height", function () {
                            return 30;
                        })
                        .style("fill", function (d) {
                            return d.team.background;
                        });

                    boxes.attr("y", function (d, i) {
                        return i * 32;
                    })
                    boxes.exit().remove()

                    var teams =  svgContainer.selectAll(".teamname").data(dataset, function(d){return d.team.key})
                    teams.enter().append("text")
                        .attr("x", function () {
                            return 130;
                        })
                        .attr("y", function (d, i) {
                            return 21 + i * 32;
                        })
                        .attr("class","teamname")
                        .style("font-size", "16px")
                        .style("font-weight", "bold")
                        .text(function (d) {
                            return d.team.name;
                        });

                    teams.exit().remove()

                    teams.attr("y", function (d, i) {
                        return 21 + i * 32;
                    })

//                    var ranks =  svgContainer.selectAll(".teamrank")
//                        .data(dataset, function(d){return d.team.key})
//                        .enter()
//                        .append("text");
//
//                    var rankAttributes = ranks
//                        .attr("x", function () {
//                            return 100;
//                        })
//                        .attr("y", function (d, i) {
//                            return 21 + i * 32;
//                        })
//                        .attr("class","teamrank")
//                        .attr("text-anchor","end")
//                        .style("font-size", "16px")
//                        .style("font-weight", "bold")
//                        .text(function (d) {
//                            return d.rank;
//                        });
//                  var values =  svgContainer.selectAll(".teamvalue")
//                      .data(dataset, function(d){return d.team.key})
//                        .enter()
//                        .append("text");
//
//                    var valueAttributes = values
//                        .attr("x", function () {
//                            return 400;
//                        })
//                        .attr("y", function (d, i) {
//                            return 21 + i * 32;
//                        })
//                        .attr("class","teamvalue")
//                        .attr("text-anchor","end")
//                        .style("font-size", "16px")
//                        .style("font-weight", "bold")
//                        .text(function (d) {
//                            return d.value;
//                        });
//                  var pctiles =  svgContainer.selectAll(".teampct")
//                      .data(dataset, function(d){return d.team.key})
//                        .enter()
//                        .append("text");
//
//                    var pctileAttributes = pctiles
//                        .attr("x", function () {
//                            return 500;
//                        })
//                        .attr("y", function (d, i) {
//                            return 21 + i * 32;
//                        })
//                        .attr("class","teampct")
//                        .attr("text-anchor","end")
//                        .style("font-size", "16px")
//                        .style("font-weight", "bold")
//                        .text(function (d) {
//                            return pctileFormat(100*d.percentile);
//                        });
//                  var zscores =  svgContainer.selectAll(".teamz")
//                      .data(dataset, function(d){return d.team.key})
//                        .enter()
//                        .append("text")
//
//
//                    var zAttributes = zscores
//                        .attr("x", function () {
//                            return 625;
//                        })
//                        .attr("y", function (d, i) {
//                            return 21 + i * 32;
//                        })
//                        .attr("class","teamz")
//                        .attr("text-anchor","end")
//                        .style("font-size", "16px")
//                        .style("font-weight", "bold")
//                        .text(function (d) {
//                            return zFormat(d.z);
//                        });
                });
                dateSlider.attr({value: 0});
                dateSlider.change();
            } else {
                $("#statDisplay").html("<p>There was an error loading stats for the key " + key + "</p>");
            }

        }
    );
}
