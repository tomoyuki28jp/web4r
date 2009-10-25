function whenDefined(name,callback,interval) {
    (function attempt() {
        if (typeof window[name] != "undefined") callback();
        else window.setTimeout(attempt,interval||10);
    })();
}

function loadScript(url,nocache) {
    window.setTimeout(function(){
        url += nocache ? "?nocache="+new Date().getTime() : "";
        var script = document.createElement('script');
        script.type = 'text/javascript'; script.src = url;
        document.getElementsByTagName('head')[0].appendChild(script);
    },0);
}

function $(func) {
    loadScript("/js/jquery.js");
    whenDefined("jQuery",function() {
        if (document.body) {
            func();
        } else {
            $(func);
        }
    });
}
