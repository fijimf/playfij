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

function headlineTeam(src, teamName, nickname, confName, confWins, confLosses, wins, losses) {
    $("#team-header-logo").html("<img style='height:70px; width:70px;' src='"+src+"' />");
    $("#team-header-name").html("<h3>"+teamName+"&nbsp;"+nickname+"</h3>");
    $("#team-header-record").html("<h4>("+wins+"-"+losses+", "+confWins+"- "+confLosses+" "+confName+" )</h4>");
}
function headlineTeamStat(statName, src, teamName, nickname, rank, value) {
    $("#change-title").text(statName);
    $("#headline").html("<h2 style='font-size:350%; padding-top:25px; font-weight:bold;'><img style='height:70px; width:70px;' src='"+src+"' />"+value+" "+teamName+"&nbsp;"+nickname+"</h2>");
}

function dropHeadline() {
    $("#team-header-logo").html("<img style='height:70px; width:70px;' src='/assets/images/blank.png' />");
    $("#team-header-name").html("<h3>&nbsp;</h3>");
    $("#team-header-record").html("<h4>&nbsp;</h4>");

    $("#headline").html("<h3><img style='height:70px; width:70px;' src='/assets/images/blank.png' />&nbsp;</h3><h4>&nbsp;</h4>");
    $("#change-title").text("Stats");
}