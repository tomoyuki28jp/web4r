(function($) {
$.extend($.fn, {
    changeSelectDate: function() {
        var days_of = function(y, m) {
            var d = [31, null, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
            if (m == 2) {
                return (new Date(y, 1, 29).getDate() ==  29) ? 29 : 28;
            } else {
                return d[m-1];
            }
        }
        $(".change_date .y, .m").change(function () {
            var id = $(this).attr("name").split("_").slice(0, -1).join("_");
            var cl = $("#"+id+"_d option").length;
            var dl = days_of( $("#"+id+"_y option:selected").text(),
                              $("#"+id+"_m option:selected").text());
            if (cl != dl) {
                if (cl < dl) {
                    for (var i=(cl + 1); i<=dl; i++)
                        $("#"+id+"_d").append("<option value='"+i+"'>"+i+"</option>");
                } else {
                    for (var i=cl; i>dl; i--)
                        $("#"+id+"_d option[value='"+i+"']").remove();
                }
            }
        })}
})})(jQuery);
