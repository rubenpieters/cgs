var connected = false;

var socket;

var clientPlayerId = 0;

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
  console.log("received message: " + JSON.stringify(message.data));

  PS.ClientMain.onServerStrMessage(message.data)();
};

function onSocketDisconnected() {
  console.log("Disconnected from socket server");
  socket = undefined;

  connected = false;
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
var handZoneY = 80;

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
var zoneGroup;

var overlapCard;
PS.ClientMain.clearOverlapCard();
var overlapDropMenu;
var playerHandZone;

var hotbarBtnShufflePack;
var hotbarBtnShufflePackText;

var globalId = 0;

var allCards = [];
var draggedCards = [];
//var selectedCards = [];
var selectedCard;

var gameState = PS.ClientMain.emptyGS;
var eventBuffer = [];

var dragTrigger = { status: 'none' };
var dragTriggerText;

var drawAmount = { amount: 1 };
var drawAmountText;

function preload() {
  game.load.image('card', 'assets/card.png');
  game.load.image('menu', 'assets/menu.png');
  game.load.image('empty', 'assets/empty.png');
  game.load.image('hand', 'assets/hand.png');
  game.load.image('background_board', 'assets/background_board.png');
}

function create() {
  game.canvas.oncontextmenu = function (e) { e.preventDefault(); };

  cardGroup = game.add.group();
  zoneGroup = game.add.group();

  game.world.bringToTop(cardGroup);

  // Player Hand

  playerHandZone = game.add.sprite(0, playRegionY - handZoneY, 'hand');
  zoneGroup.add(playerHandZone);
  playerHandZone.height = handZoneY;
  playerHandZone.width = playRegionX;
  playerHandZone.tint = 0xd3ffce;

  // Menu

  // - bottom
  var bottomMenu = game.add.sprite(0, playRegionY, 'menu');
  bottomMenu.height = botMenH;
  bottomMenu.width = gameW;

  // - bottom - shuffle pack button
  hotbarBtnShufflePack = game.add.button(10, playRegionY + 10, 'empty', shufflePack, this, 2, 1, 0);
  hotbarBtnShufflePack.height = 20;
  hotbarBtnShufflePack.width = 50;
  hotbarBtnShufflePack.visible = false;
  var style = { font: "10px Arial", fill: "#000000", align: "center" };
  hotbarBtnShufflePackText = game.add.text(12, playRegionY + 15, "Shuffle", style);
  hotbarBtnShufflePackText.visible = false;

  // - bottom - card properties buttons
  cardDragProp = game.add.sprite(100, playRegionY + 10, 'empty');
  cardDragProp.inputEnabled = true;
  cardDragProp.height = 20;
  cardDragProp.width = 100;
  cardDragProp.visible = false;
  cardDragPropText = game.add.text(102, playRegionY + 15, "", style);
  cardDragPropText.visible = false;

  // - bottom - drag trigger value
  dragTriggerText = game.add.text(12, playRegionY + 75, dragTrigger.status, style);

  // - bottom - left mode text
  drawAmountText = game.add.text(72, playRegionY + 75, drawAmount.amount, style);

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
//  PS.ClientMain.phMkCard({x: 10, y: 10, pack: [PS.ClientMain.newCard]})();

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
  keyA.onDown.add(PS.ClientMain.phMkCard({x: 100, y: 100, pack: [PS.ClientMain.newCard]}), this);

  // Key - G - gather
  var keyG = game.input.keyboard.addKey(Phaser.Keyboard.G);
  keyG.onDown.add(function () { eventBuffer.push(new PS.ClientMain.Gather()); });

  // Key - J - connect to server
  var keyJ = game.input.keyboard.addKey(Phaser.Keyboard.J);
  keyJ.onDown.add(connectToServer);

  // Key - K - disconnect from server
  var keyK = game.input.keyboard.addKey(Phaser.Keyboard.K);
  keyK.onDown.add(disconnectFromServer);

  // Key - B - cycle left mode
  var keyB = game.input.keyboard.addKey(Phaser.Keyboard.B);
  keyB.onDown.add(cycleDraw);

  // Key - S - shuffle
  var keyS = game.input.keyboard.addKey(Phaser.Keyboard.S);
  keyS.onDown.add(shufflePack);
}

function cycleDraw() {
  if (drawAmount.amount === 1) {
    drawAmount.amount = 10;
  } else if (drawAmount.amount === 10) {
    drawAmount.amount = 1;
  }
  drawAmountText.text = drawAmount.amount;
};

function shufflePack() {
  if (typeof selectedCard !== "undefined") {
    eventBuffer.push(new PS.SharedData.ClShuffle(selectedCard.props.gid));
  }
};

function update() {
  if (eventBuffer.length > 0) {
    if (!connected) {
      // TODO: buffer needs to be converted to Sv messages
      PS.ClientMain.updateGameState(eventBuffer)();
    } else {
      console.log("test: " + PS.ClientMain.showGameEvent(eventBuffer[0]));
      PS.ClientMain.sendUpdates(socket)(eventBuffer)();
      // TODO: keep track of buffer so we can resend if server lost these updates
    }
    eventBuffer = [];
  }

  PS.ClientMain.updateCards();

  updateDragTrigger();
}

function render() {

}

function updateDragTrigger() {
  if (dragTrigger.status != "none" && dragTrigger.status != "dragging" && dragTrigger.status != "waiting" && (dragTrigger.left || dragTrigger.right)) {
    if (Math.abs(dragTrigger.x - game.input.x) > 2 || Math.abs(dragTrigger.y - game.input.y) > 2) {
      if (PS.ClientMain.cardLocked(dragTrigger.c.props.gid)()) {
        // pack is locked: noop
      } else {
        // pack is not locked: can draw
        if (dragTrigger.c.props.inhand) {
          // pack is in hand: can draw immediately
          PS.ClientMain.setDragTrigger(dragTrigger.c)();
        } else {
          // pack not in hand: send server
          if (dragTrigger.right || dragTrigger.c.props.cards.length <= drawAmount.amount) {
            // drawing complete pack = dragging
            eventBuffer.push(new PS.SharedData.ClLock(dragTrigger.c.props.gid));
          } else {
            // draw `amount` from pack
            eventBuffer.push(new PS.SharedData.ClDraw(dragTrigger.c.props.gid, { amount: drawAmount.amount }));
          }
          dragTrigger.status = "waiting";
          dragTriggerText.text = "waiting";
        }
      }
    }
  }
}

function cardInputDown(sprite, pointer) {
  console.log("cardInputDown, " + sprite.props.gid);

  var leftDown = pointer.leftButton.isDown;
  var rightDown = pointer.rightButton.isDown;
  var middleDown = pointer.middleButton.isDown;

  dragTrigger = {
    x: pointer.x,
    y: pointer.y,
    c: sprite,
    left: leftDown,
    right: rightDown,
    middle: middleDown,
  };
}

function cardInputUp(sprite, pointer) {
  playerHandZone.tint = 0x0d3ffce;

  console.log("cardInputUp, " + sprite.props.gid);

  // if dragTrigger is none, it means that server denied a lock
  // and the dragTrigger was reset
  if (dragTrigger.status != "none") {
    const draggedCard = dragTrigger.c;
    // card was dragged
    console.log("X: " + dragTrigger.x + " -- " + game.input.x);
    console.log("Y: " + dragTrigger.y + " -- " + game.input.y);
    if (Math.abs(dragTrigger.x - game.input.x) > 2 || Math.abs(dragTrigger.y - game.input.y) > 2) {
      const wasInHand = draggedCard.props.inhand;
      const droppedInHand = PS.ClientMain.checkOverlap(draggedCard)(playerHandZone);
      if (wasInHand && droppedInHand) {
        // client movement only
        PS.ClientMain.dropCard(draggedCard)();
      } else if (!wasInHand && droppedInHand) {
        // send server dropInHand
        eventBuffer.push(new PS.SharedData.ClToHand(draggedCard.props.gid));
      } else if (!droppedInHand) {
        // inform server drop
        //const draggingDrawnCard = dragTrigger.c.props.gid !== sprite.props.gid;
        if (dragTrigger.left || dragTrigger.right) {
          dropCard(draggedCard);
        } else if (dragTrigger.middle) {
          eventBuffer.push(new PS.SharedData.ClFlip(sprite.props.gid));
        }
      }
    // card was selected
    } else {
      console.log("select: " + draggedCard.props.gid);
      selectedCard = draggedCard;
      const packText = PS.ClientMain.packText(draggedCard.props.cards);
      infoText.text = "gid: " + selectedCard.props.gid + "\n" + packText;
      hotbarBtnShufflePack.visible = true;
      hotbarBtnShufflePackText.visible = true;
    }
  }

  dragTrigger = { status: "none" };
  dragTriggerText.text = "none";
}

function dropCard(c) {
  PS.ClientMain.foldOverlapCard(function(overlap) {
    return function () {
      eventBuffer.push(new PS.SharedData.ClDropIn(c.props.gid, { tgt: overlap.props.gid }));
    };})(function() {
      eventBuffer.push(new PS.SharedData.ClDrop(c.props.gid, { x: c.x, y: c.y }));
    })();
};
