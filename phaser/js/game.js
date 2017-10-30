var gameH = 600;
var gameW = 800;

var game = new Phaser.Game(
  gameW, gameH, Phaser.AUTO, '',
  { preload: preload, create: create, update: update, render: render }
);

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
  var button = game.add.button(10, playRegionY + 10, 'empty', createCardClick, this, 2, 1, 0);
  button.height = 20;
  button.width = 50;
  var style = { font: "10px Arial", fill: "#000000", align: "center" };
  game.add.text(12, playRegionY + 15, "Add Card", style);

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
  addNewCard();

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
}

function addNewCard() {
  var newCard = mkCard(newCardMarker.x, newCardMarker.y, 'card');
  newCardMarker.x += 5;
  newCardMarker.y += 7;
  allCards[newCard.cardInfo.gid] = newCard;
  cardGroup.add(newCard);
  return newCard;
}

function mkCard(x, y, textureName) {
  var card = game.add.sprite(x, y, textureName);
  card.height = cardH;
  card.width = cardW;

  card.dragging = false;
  card.selecting = false;
  card.overlapping = false;
  card.selected = false;
  card.inputEnabled = true;
  card.input.enableDrag(false, true);
  card.events.onDragStart.add(onDragStart, this);
  card.events.onDragStop.add(onDragStop, this);
  card.events.onInputOver.add(onInputOver, this);
  card.events.onInputOut.add(onInputOut, this);

  var style = { font: "10px Arial", fill: "#ffffff", align: "center", stroke: "black", strokeThickness: 1};
  var packText = game.add.text(card.x + 3, card.y + 3, 1, style);
//  packText.visible = false;

  card.cardInfo = {
    texture: 'card',
    pack: [],
    packText: packText,
    gid: globalId,
  };
  globalId++;
  return card;
}

function render() {
  //cards.forEach(function(card) {
  //  game.debug.spriteBounds(card);
  //});
}

function update() {
  // clear info overlapping card
  if (overlapCard != null) {
    overlapCard = null;
  }
  infoText.setText("");

  draggedCards.forEach(function(card) {
    if (card != null) {
      // update card text
      card.cardInfo.packText.x = card.x + 3;
      card.cardInfo.packText.y = card.y + 3;
      card.cardInfo.packText.setText(card.cardInfo.pack.length + 1);
      card.cardInfo.packText.visible = card.visible;

      // clamp x/y position
      card.x = PS.Main.clamp(card.x)({lBound: 0, uBound: playRegionX - cardW});
      card.y = PS.Main.clamp(card.y)({lBound: 0, uBound: playRegionY - cardH});

      // highlight card overlap
      var overlap = firstOverlappingCard(card);
      if (overlap != null) {
        overlapCard = overlap;
      }

      card.tint = 0xffffff;
    }
  });

  selectedCards.forEach(function(card) {
    if (card != null) {
      // update card text
      card.cardInfo.packText.setText(card.cardInfo.pack.length + 1);
      card.cardInfo.packText.visible = card.visible;

      card.tint = 0x558855;
    }
  });

  allCards.forEach(function(card) {
    if (card != null) {
      // update card text
      card.cardInfo.packText.setText(card.cardInfo.pack.length + 1);
      card.cardInfo.packText.visible = card.visible;

      card.tint = 0xffffff;
    }
  });

  if (overlapCard != null) {
    overlapCard.tint = 0x885555;
  }
}

function firstOverlappingCard(card) {
  var result = allCards.find(function(c) {
    if (c != null) {
      return (!(card.x === c.x && card.y === c.y)) && checkOverlap(card, c);
    }
  });
  if (result != null)
    return result;
  return selectedCards.find(function(c) {
    if (c != null) {
      return (!(card.x === c.x && card.y === c.y)) && checkOverlap(card, c);
    }
  });
}

function checkOverlap(spriteA, spriteB) {
  var boundsA = spriteA.getBounds();
  var boundsB = spriteB.getBounds();
  return Phaser.Rectangle.intersects(boundsA, boundsB);
}

function onInputOver(sprite, pointer) {
  if ('cardInfo' in sprite) {
    preview.loadTexture(sprite.cardInfo.texture, 0, false);
    preview.height = prevH;
    preview.width = prevW;
  }
}

function onInputOut(sprite, pointer) {
  console.log("onInputOut");
  if (typeof selectedCards[sprite.cardInfo.gid] == 'undefined') {
    delete draggedCards[sprite.cardInfo.gid];
    delete selectedCards[sprite.cardInfo.gid];
    allCards[sprite.cardInfo.gid] = sprite;
  }
}

function onDragStart(sprite, pointer) {
  console.log("onDragStart");
  var origLoc;
  if (typeof allCards[sprite.cardInfo.gid] != 'undefined') {
    origLoc = "all";
  } else if (typeof selectedCards[sprite.cardInfo.gid] != 'undefined') {
    origLoc = "selected";
  } else {
    throw "no original location";
  }
  delete allCards[sprite.cardInfo.gid];
  delete selectedCards[sprite.cardInfo.gid];
  draggedCards[sprite.cardInfo.gid] = sprite;
  sprite.events.onDragStop.removeAll();
  sprite.events.onDragStop.add(onDragStop(sprite.x, sprite.y, origLoc));
}

function onDragStop(origX, origY, origLoc) {
    return function(sprite, pointer) {
    console.log("onDragStop");
    // clamp x/y position
    sprite.x = PS.Main.clamp(sprite.x)({lBound: 0, uBound: playRegionX - cardW});
    sprite.y = PS.Main.clamp(sprite.y)({lBound: 0, uBound: playRegionY - cardH});

    // update card text
    sprite.cardInfo.packText.x = sprite.x + 3;
    sprite.cardInfo.packText.y = sprite.y + 3;
    sprite.cardInfo.packText.setText(sprite.cardInfo.pack.length + 1);
    sprite.cardInfo.packText.visible = sprite.visible;

    delete draggedCards[sprite.cardInfo.gid];

    if (Math.abs(origX - sprite.x) > 3 && Math.abs(origY - sprite.y) > 3) {
      if (origLoc === "selected") {
        selectedCards[sprite.cardInfo.gid] = sprite;
      } else if (origLoc === "all") {
        allCards[sprite.cardInfo.gid] = sprite;
      } else {
        throw "unexpected origLoc";
      }

      if (overlapCard != null) {
        overlapDropMenu.x = sprite.x;
        overlapDropMenu.y = sprite.y;
        sprite.visible = false;
        overlapDropMenu.events.onInputDown.add(overlapDropClick(sprite, overlapCard));
        overlapDropMenu.visible = true;
      }
    } else {
      if (origLoc === "selected") {
        allCards[sprite.cardInfo.gid] = sprite;
      } else if (origLoc === "all") {
        selectedCards[sprite.cardInfo.gid] = sprite;
      } else {
        throw "unexpected origLoc";
      }
    }
  }
}

function createCardClick(sprite, pointer) {
  addNewCard();
}

function overlapDropClick(draggedCard, overlapCard) {
  return function () {
    overlapCard.cardInfo.pack.push(draggedCard);
    overlapCard.cardInfo.pack = overlapCard.cardInfo.pack.concat(draggedCard.cardInfo.pack);
    draggedCard.cardInfo.pack = [];
    delete draggedCards[draggedCard.cardInfo.gid];
    draggedCard.kill();
    overlapDropMenu.visible = false;
    overlapDropMenu.events.onInputDown.removeAll();
  };
}
