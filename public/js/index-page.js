$(document).ready(function() {
    var class = $('#title').attr('class');
    var pager = new $.pager();
    var total = $('.page_summary #total_items').text();
    var removeMsgs = function() { $('ul.errors, ul.msgs').remove(); }
    var updateList = function(slot, order, add) {
        var c = $('thead').attr('class').split(' ');
        var slot  = slot  || c[0];
        var order = order || c[1];
        var add   = add   || "";
        add += "&items_per_page="+pager.items_per_page;
        $.get('/ajax/'+class+'/list/?slot='+slot+'&order='+order+add, function(list) { 
            $('#table_list tbody').html(list);
        });
    };

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
        updateList($(this).attr('id'), order);
	return false;
    })

    $('.delete').live('click', function() {
        pager.remove_item(1);
        var oid = $(this).attr('href').split('/'); var oid = oid[oid.length - 2];
        $.get('/ajax/'+class+'/delete/'+oid, function(r) {
            var msg = '<ul class="msgs"><li>'+r+'</li></ul>';
            ($('ul.msgs').length) ? $('ul.msgs').replaceWith(msg) : $('body').prepend(msg);
            updateList(null, null, '&page='+pager.current_page);
        })
        return false;
    })

    $('.page_links a').live('click', function() {
	var page = $(this).attr('href').split('page=')[1].split('&')[0];
        removeMsgs();
        pager.goto_page( page );
        updateList( null, null, '&'+'page='+page );
        return false;
    })
})
