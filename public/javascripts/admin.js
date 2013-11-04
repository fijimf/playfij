function deleteItem(key,keyType,domain){
    var hiddenId = '<input type="hidden" name="'+keyType+'" value="' + key + '"/>';
    var action = '/admin/'+domain+'/delete';
    $('<form>', { "html":hiddenId, "method":'POST', "action":action } ).appendTo(document.body).submit();
}