$(document).ready(function() {
    var cname = $('#title').attr('class');
    $.validator.messages.remote = 'The save value is already in use.';
    $.validator.addMethod('format', function(value, element) {
       return value.match($(element).attr('format')) !== null
    }, 'Please fix this field.');
    var validator = $('#'+cname+'_form').validate({
        errorPlacement: function(error, element) {
            error.appendTo( element.parent() )
        }
    });
})
