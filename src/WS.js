"use strict";

const express = require('express');
const WebSocket = require("uws");
const path = require('path');

exports.mkServer = function(PORT){
  return function() {
    const DIST = path.join(__dirname, 'dist');
    const JS = path.join(DIST, 'js');
    const ASSETS = path.join(DIST, 'assets');
    const INDEX = path.join(DIST, 'index.html');
    console.log('dist folder: ' + DIST);

    const app = express();
    app.get('/', function(req, res) { res.sendFile(INDEX); });
    app.use('/js', express.static(JS));
    app.use('/assets', express.static(ASSETS));
    const server = app.listen(PORT, function() { console.log('Listening on ' + PORT); });

    const wss = new WebSocket.Server({ server: server });

    return wss;
  };
};

const unsafeSendMessage = function(client) {
  return function(data) {
    return function() {
      if (client.readyState === WebSocket.OPEN) {
        console.log("sending: " + data);
        client.send(data);
      }
    };
  };
};

exports.unsafeSendMessage = unsafeSendMessage;

exports.unsafeBroadcast = function(server) {
  return function(data) {
    return function(exceptClient) {
      return function() {
        server.clients.forEach(function(client) {
          if (client !== exceptClient) {
            unsafeSendMessage(client)(data)();
          }
        });
      };
    };
  };
};
