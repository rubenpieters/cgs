"use strict";

const WebSocket = require("uws");

exports.mkServer = function(config){
  return function() {
    return new WebSocket.Server(config);
  };
};

const unsafeSendMessage = function(data) {
  return function(client) {
    return function() {
      if (client.readyState === WebSocket.OPEN) {
        console.log("sending: " + data);
        client.send(data);
      }
    };
  };
};

exports.unsafeSendMessage = unsafeSendMessage;

exports.unsafeBroadcast = function(data) {
  return function(exceptClient) {
    return function(server) {
      return function() {
        server.clients.forEach(function(client) {
          if (client !== exceptClient) {
            unsafeSendMessage(data)(client)();
          }
        });
      };
    };
  };
};
