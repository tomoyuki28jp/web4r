$(document).ready(function() {
    var class = $('#title').attr('class');
    var pager = new $.pager();
    var total = $('.page_summary #total_items').text();
    var removeMsgs = function() { $('ul.errors, ul.msgs').remove(); }
    var updateList = function(item, order, add) {
        var c = $('thead').attr('class').split(' ');
	var item  = item  || c[0];
	var order = order || c[1];
	var add   = add   || "";
	add += "&items_per_page="+pager.items_per_page;
	add += "&links_per_page="+pager.links_per_page;
        $.get('/ajax/'+class+'/list/?item='+item+'&order='+order+add, function(list) { 
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
    })

    $('.delete').live('click', function() {
        pager.remove_item(1);
        pager.goto_page(1);
        var oid = $(this).attr('href').split('/'); var oid = oid[oid.length - 2];
        $.get('/ajax/'+class+'/delete/'+oid, function(r) {
            var msg = '<ul class="msgs"><li>'+r+'</li></ul>';
            ($('ul.msgs').length) ? $('ul.msgs').replaceWith(msg) : $('body').prepend(msg);
	    updateList();
        })
        return false;
    })

    $('.page_links a').live('click', function() {
        removeMsgs();
        pager.goto_page($(this).text());
	updateList(null, null, '&'+$(this).attr('href').substring(1));
        return false;
    })
})
