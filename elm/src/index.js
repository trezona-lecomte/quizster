'use strict';

require('bulma/custom_styles.css');
require('font-awesome/css/font-awesome.css');
require('../img/QuizsterLogo.png');

// Ensure index.html is copied to dist:
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// The third value on embed are the initial values for incoming ports
// into Elm:
var app = Elm.embed(Elm.Main, mountNode, {getConfirmations: 0});

// confirmations is called by sending a message to a port in elm. When
// this happens the browser shows s confirmation window, if the user
// responds with yes then we send a messsage back to elm:
app.ports.confirmations.subscribe(function (args) {
  console.log('confirmation', args);
  var id = args[0];
  var message = args[1];
  var response = window.confirm(message);
  if (response) {
    app.ports.getConfirmations.send(id);
  }
});
