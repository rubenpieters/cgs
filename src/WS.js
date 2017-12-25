"use strict";

const WebSocket = require("uws");

exports.mkServer = function(config){
  return function() {
    return new WebSocket.Server(config);
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

var serverState;

exports.initServerState = function() {
  serverState = [];
};

exports.getServerState = function() {
  return serverState;
};

exports.setServerState = function(update) {
  return function() {
    serverState = update;
  };
};