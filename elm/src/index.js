'use strict';

require('basscss/css/basscss.css');
require('font-awesome/css/font-awesome.css');

// Ensure index.html is copied to dist:
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// The third value on embed are the initial values for incoming ports
// into Elm:
var app = Elm.embed(Elm.Main, mountNode);
