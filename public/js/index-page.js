$(document).ready(function() {
    var class = $('#title').attr('class');
    var per_page = $('.page_summary').attr('per_page');
    var total = $('.page_summary #total_items').text();

    $('.sort').click(function() {
        // page summary
        $('.page_summary #item_start').text(1);
        $('.page_summary #item_end').text(Math.min(total, per_page));
        // page links
        var p = $('.page_links span').text();
        $('.page_links span').replaceWith('<a href=\"?page='+p+'\">'+p+'</a>');
        $('.page_links :first').replaceWith('<span>1</span>');
        // replace the list
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

    $('.page_links a').live('click', function() {
        // page summary
        var new_page = $(this).text();
        $('.page_summary #item_start').text((1 + (new_page - 1) * per_page));
        $('.page_summary #item_end').text(Math.min(total, (new_page * per_page)));
        // page links
        var p = $('.page_links span').text();
        $('.page_links span').replaceWith('<a href=\"?page='+p+'\">'+p+'</a>');
        $(this).replaceWith('<span>'+new_page+'</span>');
        // replace the list
        var cp = $('thead').attr('class').split(' ');
        var pp = $(this).attr('href').substring(1);
        $.get('/ajax/'+class+'/list/?item='+cp[0]+'&order='+cp[1]+'&'+pp, function(x) { 
            $('#table_list tbody').html(x);
        })
        return false;
    })
})
