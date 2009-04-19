$(document).ready(function() {
    var class = $('#title').attr('class');
    var per_page = $('.page_summary').attr('per_page');
    var total = $('.page_summary #total_items').text();
    var removeMsgs = function() { $('ul.errors, ul.msgs').remove(); }
    var pageOffset = function(start, end) {
        $('.page_summary #item_start').text(start);
        $('.page_summary #item_end').text(end);
    }
    var topPage = function() {
        pageOffset(1, Math.min(total, per_page));
        var p = $('.page_links span').text();
        $('.page_links span').replaceWith('<a href=\"?page='+p+'\">'+p+'</a>');
        $('.page_links :first').replaceWith('<span>1</span>');
    }

    $('.sort').click(function() {
        removeMsgs();
        topPage();
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
        $('.page_summary #total_items').text(total = total - 1);
        topPage();
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
        var np = $(this).text();
        pageOffset(1 + (np - 1) * per_page, Math.min(total, (np * per_page)));
        var p = $('.page_links span').text();
        $('.page_links span').replaceWith('<a href=\"?page='+p+'\">'+p+'</a>');
        $(this).replaceWith('<span>'+np+'</span>');
        var cp = $('thead').attr('class').split(' ');
        var pp = $(this).attr('href').substring(1);
        $.get('/ajax/'+class+'/list/?item='+cp[0]+'&order='+cp[1]+'&'+pp, function(x) { 
            $('#table_list tbody').html(x);
        })
        return false;
    })
})
