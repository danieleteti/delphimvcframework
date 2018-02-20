function AJAX() {
    var me = this;
    this.callback = null;

    function listener() {
        me.callback(this.status, this.response);
    }

    var oReq = new XMLHttpRequest();
    oReq.addEventListener("load", listener);

    this.get = function(URL, callback) {
        me.callback = callback;
        oReq.open("GET", URL);
        oReq.send();
    }
}