importScripts('ajax.js')

var ajax = new AJAX();
var clientid = null;

function loop() {

    ajax.get('/topics/test1/' + clientid, function(status, response) {
        if (status == 200) {
            //console.log(response);
            var json = JSON.parse(response);
            //console.log(json);
            self.postMessage(json);
            setTimeout(loop, 500);
        }
    });
}

self.onmessage = function(event) {
    self.clientid = event.data;
    self.loop();
}