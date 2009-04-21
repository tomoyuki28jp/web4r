$(document).ready(function() {
    var class = $('#title').attr('class');
    var pager = new $.pager();
    var total = $('.page_summary #total_items').text();
    var removeMsgs = function() { $('ul.errors, ul.msgs').remove(); }

    $('.sort').click(function() {
        removeMsgs();
        pager.goto_page(1);
        var cp = $('thead').attr('class').split(' ');
        var order = (cp[0] == $(this).attr('id') && cp[1] == 'asc') ? 'desc' : 'asc';
        if (cp[0] !== 'updated-at' && cp[0] !== 'created-at') {
            $('#'+cp[0]+' img').attr('src', '/images/order_no.gif');
        }
        $(this).children('img').attr('src', '/images/order_'+order+'.gif');
        $('thead').attr('class', $(this).attr('id')+' '+order);
        $.get('/ajax/'+class+'/list/?item='+$(this).attr('id')+'&order='+order, function(x) { 
            $('#table_list tbody').html(x);
        })
    })

    $('.delete').live('click', function() {
        pager.remove_item(1);
        pager.goto_page(1);
        var oid = $(this).attr('href').split('/'); var oid = oid[oid.length - 2];
        $.get('/ajax/'+class+'/delete/'+oid, function(r) {
            var msg = '<ul class="msgs"><li>'+r+'</li></ul>';
            ($('ul.msgs').length) ? $('ul.msgs').replaceWith(msg) : $('body').prepend(msg);
            var cp = $('thead').attr('class').split(' ');
            $.get('/ajax/'+class+'/list/?item='+cp[0]+'&order='+cp[1], function(x) { 
                $('#table_list tbody').html(x);
            })
        })
        return false;
    })

    $('.page_links a').live('click', function() {
        removeMsgs();
        pager.goto_page($(this).text());
        var cp = $('thead').attr('class').split(' ');
        var pp = $(this).attr('href').substring(1);
        $.get('/ajax/'+class+'/list/?item='+cp[0]+'&order='+cp[1]+'&'+pp, function(x) { 
            $('#table_list tbody').html(x);
        })
        return false;
    })
})
