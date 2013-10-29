function sendData() {
  $.ajax({
    url: '/hello',
    type: 'POST',
    contentType: 'application/json',
    data: JSON.stringify({name: "Bob"}),
    processData: false,
    dataType: 'json'
  }).success(function(data) {
    content.html(JSON.stringify(data));
  }).fail(function() {
    console.log(arguments)
  });
}


var content = null;
var btn1, btn2;
$(document).ready(function() {
  content = $('#content');
  $('#btn1').click(function() {
    console.log("Button1");
    $.get("/hello")
              .success(function(data) { content.html(data);})
              .fail(function(){console.log(arguments)});
  });
  $('#btn2').click(function() {
    console.log("Button2");
    sendData();
  });
  $('#btn3').click(function() {
    console.log("Button3");
    $.get('/div/10/0')
            .success(function(data) {})
            .fail(function() {console.log(arguments)});
  });
});