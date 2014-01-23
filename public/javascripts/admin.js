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
    $("#headline").html("<h3><img style='height:70px; width:70px;' src='"+src+"' />"+teamName+"&nbsp;"+nickname+"</h3><h4>("+wins+"-"+losses+", "+confWins+"- "+confLosses+" "+confName+" )</h4>");
}

function dropHeadline() {
    $("#headline").html("<h3><img style='height:70px; width:70px;' src='/assets/images/blank.png' />&nbsp;</h3><h4>&nbsp;</h4>");
}