$(document).ready(function() {
    var days_of = function(y, m) {
        if (m == 2) {
            return (new Date(y, 1, 29).getDate() ==  29) ? 29 : 28
        } else {
            return [31, null, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m-1];
        }
    };

    var change_date = function(y, m, id) {
	    var dl = days_of(y, m);
	    var cl = $("#"+id+" option").length;
        if (cl != dl) {
            if (cl < dl) {
                for (var i=(cl + 1); i<=dl; i++)
                    $("#"+id).append("<option value='"+i+"'>"+i+"</option>");
            } else {
                for (var i=cl; i>dl; i--)
                    $("#"+id+" option[value='"+i+"']").remove();
            }
        }
    };

	var today = new Date();
	$(".change_date .d").each(function() {
        change_date(today.getFullYear(), (today.getMonth() + 1), $(this).attr("id"));
    });

    $(".change_date .y, .m").change(function() {
        var id = $(this).attr("name").split("_").slice(0, -1).join("_");
	    change_date($("#"+id+"_y option:selected").text(),
			        $("#"+id+"_m option:selected").text(),
			        id+"_d");
    });
});
