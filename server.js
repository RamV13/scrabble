
var express = require('express');
var app = module.exports = express();

var proxy = require('http-proxy').createProxyServer({
  host: 'http://localhost:8000'
});

app.use('/api', function(req, res, next) {
  var data = "";
  req.on('data', function(chunk) {data += chunk});
  req.on('end', function() {
     console.log(req.method + '\t\t' + req.path + '\t\t\t' + data);
  });
  proxy.web(req, res, {
    target: 'http://localhost:8000/api'
  }, next);
});

app.use(express.static(__dirname + '/public'));

app.listen(80, function () {
  console.log('----SERVER STARTED---- ' + new Date().toISOString());
});
