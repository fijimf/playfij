function deleteItem(key,keyType,domain){
    var hiddenId = '<input type="hidden" name="'+keyType+'" value="' + key + '"/>';
    var action = '/admin/'+domain+'/delete';
    $('<form>', { "html":hiddenId, "method":'POST', "action":action } ).appendTo(document.body).submit();
}

function loadQuote() {
    var jsonUrl = "/api/quote";
    $.getJSON(jsonUrl, {},
        function (json) {
                $("#qotd").html("<p class='quote'><a href='"+json.url+"' title='"+json.source+"' >"+json.quote+"</a></p>");
        }
    );
}