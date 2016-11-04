
var express = require('express');
var app = module.exports = express();

var proxy = require('http-proxy').createProxyServer({
  host: 'http://localhost:8000',
});

app.use('/api', function(req, res, next) {
  proxy.web(req, res, {
    target: 'http://localhost:8000/api'
  }, next);
});

app.use(express.static('public'));

app.listen(80);

