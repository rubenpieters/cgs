"use strict";

const express = require('express');
const WebSocket = require("uws");
const path = require('path');

exports.mkServer = function(PORT){
  return function() {
    const DIST = path.join(__dirname, 'dist');
    console.log('dist folder: ' + DIST);

    const server = express()
      .listen(PORT, function() { console.log('Listening on ' + PORT) });

    express().use(express.static(DIST));

    const wss = new WebSocket.Server({ server: server });

    return wss
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
