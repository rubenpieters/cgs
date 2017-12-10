var connected = false;
//var socket;
//const WebSocket = require('uws');

var socket;

function connectToServer() {
  if (! connected) {
    socket = new WebSocket('ws://localhost:8080');
    socket.onopen = function() {
      socket.onmessage = onMessage;
      connected = true;
    };
  } else {
    console.log("already connected!");
  }
}

function disconnectFromServer() {
  if (connected) {
    console.log("disconnecting...");
    socket.close();
    // TODO: should this be called after an ack from server disconnect?
    onSocketDisconnected();
  } else {
    console.log("already disconnected!");
  }
}

function onMessage(message) {
  console.log("received message: " + message.data);

  var msgPayload = JSON.parse(message.data);
  var msgType = msgPayload.type;
  var msgData = msgPayload.data;
  switch (msgType) {
  case "player id":
    console.log("assigned player id: " + msgData.id);
    break;
  case "new player":
    onNewPlayer(msgData);
    break;
  case "remove player":
    onRemovePlayer(msgData);
    break;
  case "move gid":
    onMoveGid(msgData);
    break;
  case "confirm update":
    onConfirmUpdate(msgData);
    break;
  default:
    console.log("unknown message type " + msgType);
    break;
  }
};

/*
function onSocketConnected() {
	console.log("Connected to socket server");

	// Send local player data to the game server
	socket.emit("new player", {});
};
*/

function onSocketDisconnected() {
  console.log("Disconnected from socket server");
  socket = undefined;

  connected = false;
};

function onNewPlayer(data) {
  console.log("new player connected: " + JSON.stringify(data));
};

function onRemovePlayer(data) {
  console.log("player disconnect: " + JSON.stringify(data));
};

function onMoveGid(data) {
  console.log("mov gid: " + JSON.stringify(data));
  PS.Main.onCard(data.gid)(PS.Main.moveCard(data.x)(data.y))();
};

function onConfirmUpdate(data) {
  console.log("received game update confirmation");
  console.log("events " + JSON.stringify(data));

  //console.log(data.events[0] instanceof PS.Main.Select);
  var decodedEvents = PS.Main.unsafeDecodeGEA(data.events)();

  PS.Main.updateGameState(decodedEvents)();
};

var gameH = 600;
var gameW = 800;

var config = {
  width: 800,
  height: 600,
  renderer: Phaser.AUTO,
  state: {
    preload: preload,
    create: create,
    update: update,
    render: render,
  }
};

var game = new Phaser.Game(config);

var botMenH = 100;
var rgtMenH = 200;
var playRegionY = gameH - botMenH;
var playRegionX = gameW - rgtMenH;

var cardH = 40;
var cardW = 24;

var preview;
var prevH = cardH * 3;
var prevW = cardW * 3;

var infoText;

var newCardMarker = {
  x: 60,
  y: 60,
};

var popupGroup;
var cardGroup;

var overlapCard;
var overlapDropMenu;

var globalId = 0;

var allCards = [];
var draggedCards = [];
var selectedCards = [];

var gameState = PS.Main.emptyGS;
var eventBuffer = [];

var dragTrigger = { status: 'none' };

function preload() {
  game.load.image('card', 'assets/card.png');
  game.load.image('menu', 'assets/menu.png');
  game.load.image('empty', 'assets/empty.png');
}

function create() {
  game.canvas.oncontextmenu = function (e) { e.preventDefault(); };

  // Menu

  // - bottom
  var bottomMenu = game.add.sprite(0, playRegionY, 'menu');
  bottomMenu.height = botMenH;
  bottomMenu.width = gameW;

  // - bottom - create card button
  var button = game.add.button(10, playRegionY + 10, 'empty', PS.Main.phMkCard({x: 100, y: 100, pack: [PS.Main.newCard]}), this, 2, 1, 0);
  button.height = 20;
  button.width = 50;
  var style = { font: "10px Arial", fill: "#000000", align: "center" };
  game.add.text(12, playRegionY + 15, "Add Card", style);

  // - bottom - card properties buttons
  cardDragProp = game.add.sprite(100, playRegionY + 10, 'empty');
  cardDragProp.inputEnabled = true;
  cardDragProp.height = 20;
  cardDragProp.width = 100;
  cardDragProp.visible = false;
  cardDragPropText = game.add.text(102, playRegionY + 15, "", style);
  cardDragPropText.visible = false;

  // - right
  bottomMenu = game.add.sprite(playRegionX, 0, 'menu');
  bottomMenu.height = gameH - botMenH;
  bottomMenu.width = rgtMenH;

  // - right - card preview
  preview = game.add.sprite(playRegionX + 40, 30, 'empty');
  preview.height = prevH;
  preview.width = prevW;

  // - right - info text
  infoText = game.add.text(playRegionX + 40, 50 + prevH, "", style);

  // Cards
  cardGroup = game.add.group();
  PS.Main.phMkCard({x: 10, y: 10, pack: [PS.Main.newCard]})();

  // Popup Menu
  popupGroup = game.add.group();
  game.world.bringToTop(popupGroup);

  // - on overlap drop
  ///overlapDropMenu = game.add.button(-1000, -1000, 'empty', overlapDropClick, this, 2, 1, 0);
  overlapDropMenu = game.add.sprite(-1000, -1000, 'empty');
  overlapDropMenu.inputEnabled = true;
  overlapDropMenu.height = 20;
  overlapDropMenu.width = 50;
  overlapDropMenu.visible = false;
  popupGroup.add(overlapDropMenu);

  // Key - A - add card
  var keyA = game.input.keyboard.addKey(Phaser.Keyboard.A);
  keyA.onDown.add(PS.Main.phMkCard({x: 100, y: 100, pack: [PS.Main.newCard]}), this);

  // Key - G - gather
  var keyG = game.input.keyboard.addKey(Phaser.Keyboard.G);
  keyG.onDown.add(function () { eventBuffer.push(new PS.Main.Gather()); });

  // Key - J - connect to server
  var keyJ = game.input.keyboard.addKey(Phaser.Keyboard.J);
  keyJ.onDown.add(connectToServer);

  // Key - K - disconnect from server
  var keyK = game.input.keyboard.addKey(Phaser.Keyboard.K);
  keyK.onDown.add(disconnectFromServer);
}

function update() {
  if (eventBuffer.length > 0) {
    if (!connected) {
      PS.Main.updateGameState(eventBuffer)();
      eventBuffer = [];
    } else {
      console.log("test: " + PS.Main.showGameEvent(eventBuffer[0]));
      PS.Main.emit(socket)(new PS.Main.GameStateUpdate({ events : PS.Main.encodeGEA(eventBuffer) }))();
      // TODO: keep track of buffer so we can resend if server lost these updates
      eventBuffer = [];
    }
  }

  PS.Main.updateCards();

  updateDragTrigger();
}

function render() {
  
}

function updateDragTrigger() {
  if (dragTrigger.status != "none" && dragTrigger.status != "dragging" && dragTrigger.left) {
    if (Math.abs(dragTrigger.x - game.input.x) > 2 || Math.abs(dragTrigger.y - game.input.y) > 2) {
      if (dragTrigger.c.pack.dragMode === "drag") {
        dragTrigger.c.pack.dragging = true;
        dragTrigger = { status: "dragging", left: dragTrigger.left, right: dragTrigger.right, c: dragTrigger.c };
      } else {
        var newCard = PS.Main.drawFromPack({x: dragTrigger.c.x, y: dragTrigger.c.y})(dragTrigger.c)();
        dragTrigger = { status: "dragging", left: dragTrigger.left, right: dragTrigger.right, c: newCard };
        console.log("draw");
      }
    }
  }
}

function cardInputDown(sprite, pointer) {
  console.log("cardInputDown, " + sprite.pack.gid);

  var leftDown = pointer.leftButton.isDown;
  var rightDown = pointer.rightButton.isDown;

  dragTrigger = { x: pointer.x, y: pointer.y, c: sprite, left: leftDown, right: rightDown };
}

function cardInputUp(sprite, pointer) {
  console.log("cardInputUp, " + sprite.pack.gid);
  if (typeof dragTrigger.c != 'undefined' && dragTrigger.c.pack.gid === sprite.pack.gid) {
    if (dragTrigger.left) {
      eventBuffer.push(new PS.Main.Select(sprite.pack.gid));
    } else if (dragTrigger.right) {
      // TODO: only right-click if mouse bounds are still within card bounds?
      console.log("right click!");
      eventBuffer.push(new PS.Main.Flip(sprite.pack.gid));
    }
  } else {
    // this branch occurs when drawing from a pack and then the newly dragged card is released
    eventBuffer.push(new PS.Main.Select(dragTrigger.c.pack.gid));
  }


  dragTrigger = { status: "none" };
}
