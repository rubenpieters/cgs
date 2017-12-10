const Util = require("util");
const WebSocket = require("uws");
const PS = require("./purs.js");

const wss = new WebSocket.Server({ port: 8080 });

function init() {
  players = [];

  //socket = io.listen(8000);

  setEventHandlers();

  Util.log("server started");
};

var setEventHandlers = function() {
  wss.on('connection', onSocketConnection);
};

function onSocketConnection(client) {
  // TODO: generate unique id
  var rnd = getRandomInt(0, 1000000000);

  Util.log("New player has connected: " + rnd);
  // set event handlers
  client.on("message", onMessage(client, rnd));
  client.on("close", onDisconnect(rnd));
  // send id to client
  sendMessage(client, {type: "player id", data: {id: rnd}});

  // update other players
  broadcast({type: "new player", data: {id: rnd}}, client);
  // update new player of other players
  players.forEach(function(player) {
    sendMessage(client, {type: "new player", data: {id: player.playerId}});
  });

  players.push({playerId: rnd, socket: client});
};

function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
};

function onMessage(ws, playerId) {
  return function(message) {
    Util.log("received message " + message);
    var parsedMsg = JSON.parse(message);

    var msgType = parsedMsg.type;
    var msgData = parsedMsg.data;
    switch (msgType) {
    case "move gid":
      Util.log("player " + playerId + " moving gid " + JSON.stringify(msgData));
      broadcast({type: "move gid", data: msgData}, ws);
      break;
    case "gamestate update":
      Util.log("player " + playerId + " gamestate update " + JSON.stringify(msgData));
      sendMessage(ws, {type: "confirm update", data: msgData});
      break;
    default:
      Util.log("unknown message type " + msgType);
      break;
    }
  };
};

function onDisconnect(playerId) {
  return function() {
    var toRemove = players.findIndex(function(player) { return player.playerId === playerId; });

    if (toRemove <= -1) {
      Util.log("Player not found");
    } else {
      var removed = players.splice(toRemove, 1)[0];
      Util.log("player " + removed.playerId + " disconnected");

      broadcast({type: "remove player", data: {id: removed.playerId}}, removed.client);
    }
  };
};

function broadcast(data, exceptWs) {
  wss.clients.forEach(function(client) {
    if (client !== exceptWs) {
      sendMessage(client, data);
    }
  });
};

function sendMessage(ws, data) {
  if (ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify(data));
  }
};

init();
