function sendData() {
  $.ajax({
    url: '/hello',
    type: 'POST',
    contentType: 'application/json',
    data: JSON.stringify({ name: 'Bob' }),
    processData: false,
    dataType: 'json'
  })
    .success(function(data) {
      content.html(JSON.stringify(data));
    })
    .fail(function() {
      console.log(arguments);
    });
}

var content = null;
var btn1, btn2;
$(document).ready(function() {
  content = $('#content');

  $('#btn0').click(function() {
    content.html('');
  });

  $('#btn1').click(function() {
    console.log('Button1 Clicked');
    $.get('/hello')
      .success(function(data) {
        content.html(data);
      })
      .fail(function() {
        console.log(arguments);
      });
  });

  $('#btn2').click(function() {
    console.log('Button2 Clicked');
    sendData();
  });

  $('#btn3').click(function() {
    console.log('Button3 Clicked');
    $.get('/div/20/2')
      .success(function(data) {
        content.html('20 / 2 = ' + data.result);
      })
      .fail(function() {
        console.log(arguments);
      });
  });

  $('#btn5').click(function() {
    alert('This will raise an exception');
    console.log('Button5 Clicked... this will raise an exception');
    $.get('/div/20/0')
      .success(function(data) {
        /*never called in this demo*/
      })
      .fail(function(obj, err, message) {
        var html = '<h1>Error raised!</h1>';
        html += '<p>HTTP Status code: ' + obj.status + '</p>';
        html += '<p>Response Text: ' + message + '</p>';
        content.html(html);
        console.log(arguments);
      });
  });
});
