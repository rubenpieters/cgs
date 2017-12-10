"use strict";

const WebSocket = require("uws");

var players;

exports.initPlayers = function() {
  players = [];
};

exports.getPlayers = function() {
  return players;
};

exports.setPlayers = function(update) {
  return function() {
    players = update;
  };
}
