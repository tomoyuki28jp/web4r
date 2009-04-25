$.pager = function(total_items, current_page) {
    this.total_items = total_items || parseInt($('.page_summary #total_items').text());
    this.items_per_page = parseInt($('.page_summary').attr('items_per_page')) || 10;
    this.links_per_page = parseInt($('.page_summary').attr('links_per_page')) || 10;
    this.current_page = current_page || 1;
    this.set_total_pages();
    this.set_item_start();
    this.set_item_end();
    var link_offset = this.link_offset();
    this.link_start = link_offset[0];
    this.link_end   = link_offset[1];
    this.next_link  = '>>';
    this.prev_link  = '<<';
};

$.pager.prototype.set_total_pages = function() {
    this.total_pages = Math.ceil(this.total_items / this.items_per_page);
};

$.pager.prototype.set_item_start = function() {
    this.item_start = ((this.current_page - 1) * this.items_per_page) + 1;
};

$.pager.prototype.set_item_end = function() {
    this.item_end = Math.min(this.total_items, this.current_page * this.items_per_page);
};

$.pager.prototype.link_offset = function() {
    if (this.total_pages === 1) return [0, 0];
    if (this.links_per_page >= this.total_pages) return [1, this.total_pages];
    var left  = Math.round(this.links_per_page / 2);
    var right = this.links_per_page - left;
    if (this.current_page <= left) return [1, this.links_per_page];
    if ((this.current_page + right) > this.total_pages)
        return [(this.total_pages - this.links_per_page) + 1, this.total_pages];
    return [this.current_page - left, this.current_page + right];
};

$.pager.prototype.goto_page = function(page) {
    this.current_page = parseInt(page);
    this.set_item_start();
    this.set_item_end();
    var link_offset = this.link_offset();
    var e = this.link_end;
    this.link_start = link_offset[0];
    this.link_end   = link_offset[1];
    if (e != link_offset[1]) {
        this.set_page_links(link_offset[0], link_offset[1]);
    } else {
        var p = $('.page_links span').text();
        $('.page_links span').replaceWith('<a href=\"?page='+p+'\">'+p+'</a>');
        $('.page_links li a').filter(function(index) {
            return $(this).text() == page
        }).replaceWith('<span>'+this.current_page+'</span>');
    }
    if (this.item_end === 0) {
	$('.page_summary').empty();
    } else {
	$('.page_summary #item_start').text(this.item_start);
	$('.page_summary #item_end').text(this.item_end);
    }
};

$.pager.prototype.set_page_links = function(start, end) {
    if (end <= 1) { return $('.page_links ul').replaceWith('<ul></ul>'); }

    var links = '<ul>'+this._prev_link();
    for (var p=start; p<=end; p++) {
        links += (p == this.current_page)
            ? '<li><span>'+p+'</span></li>'
            : '<li><a href="?page='+p+'">'+p+'</a></li>';
    }
    $('.page_links ul').replaceWith(links+this._next_link()+'</ul>');
}

$.pager.prototype._prev_link = function() {
    var page = Math.max(1, this.current_page - this.links_per_page);
    return (1 < this.link_start) ?
	'<li><a href="?page='+page+'">'+this.prev_link+'</a></li>' : '';
}

$.pager.prototype._next_link = function() {
    var page = Math.min(this.total_pages, this.current_page + this.links_per_page);
    return (this.link_end < this.total_pages) ?
	'<li><a href="?page='+page+'">'+this.next_link+'</a></li>' : '';
}

$.pager.prototype.remove_item = function(n) {
    this.total_items = this.total_items - n;
    $('.page_summary #total_items').text(this.total_items);
    this.set_total_pages();
    this.goto_page( (this.current_page > this.total_pages)
		    ? this.total_pages : this.current_page );
}
