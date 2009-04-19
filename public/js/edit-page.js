$(document).ready(function() {
    var class = $('#title').attr('class');
    $.validator.messages.remote = 'The save value is already in use.';
    $.validator.addMethod('format', function(value, element) {
       return value.match($(element).attr('format')) !== null
    }, 'Please fix this field.');
    var validator = $('#'+class+'_form').validate({
        errorPlacement: function(error, element) {
            error.appendTo( element.parent() )
        },
    })
    $(this).changeSelectDate();
})
