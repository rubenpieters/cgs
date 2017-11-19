var util = require("util"),
    io = require("socket.io");
var PS = require("./purs.js");

var socket,
    players;

function init() {
  players = [];

  socket = io.listen(8000);

  setEventHandlers();

  console.log("server started");
};

var setEventHandlers = function() {
  socket.sockets.on("connection", onSocketConnection);
};

function onSocketConnection(client) {
  util.log("New player has connected: "+client.id);
  client.on("disconnect", onClientDisconnect);
  client.on("new player", onNewPlayer);
  client.on("move gid", onMoveGid);
  client.on("gamestate update", onGameStateUpdate);
};

function onClientDisconnect() {
  // the this object refers to the client variable from the onSocketConnection function
  util.log("Player has disconnected: "+this.id);

  var removePlayer = playerById(this.id);

	// Player not found
	if (!removePlayer) {
		util.log("Player not found: "+this.id);
		return;
	};

	// Remove player from players array
	players.splice(players.indexOf(removePlayer), 1);

	// Broadcast removed player to connected socket clients
  this.broadcast.emit("remove player", {playerId: this.id});
};

function onNewPlayer(data) {
  var newPlayerId = this.id;

  this.broadcast.emit("new player", { playerId: newPlayerId });

  var i, existingPlayer;
  for (i = 0; i < players.length; i++) {
    existingPlayer = players[i];
    this.emit("new player", { playerId: existingPlayer.playerId });
  };

  players.push({ playerId: newPlayerId });
};

function onMoveGid(data) {
  util.log("player " + this.id + " moving gid " + JSON.stringify(data));

  this.broadcast.emit("move gid", data);
};

function onGameStateUpdate(data) {
  util.log("player " + this.id + " gamestate update " + JSON.stringify(data));

  // TODO: add sequencing to gamestate updates
  this.emit("confirm update", data);
};

function playerById(id) {
	var i;
	for (i = 0; i < players.length; i++) {
		if (players[i].playerId == id)
			return players[i];
	};

	return false;
};

init();

