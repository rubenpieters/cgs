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

var cards = [];

var newCardMarker = {
  x: 60,
  y: 60,
};

var popupGroup;
var cardGroup;

var overlapCard;
var overlapDropMenu;

var globalId = 0;

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
  cards.push(newCard);
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
  card.events.onInputDown.add(onInputDown, this);

  var style = { font: "10px Arial", fill: "#ffffff", align: "center", stroke: "black", strokeThickness: 1};
  var packText = game.add.text(card.x + 6, card.y + 6, 1, style);
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
  cards.forEach(function(card) {
    if (card != null) {
      card.overlapping = false;
    }
  });
  overlapCard = null;
  infoText.setText("");

  //
  cards.forEach(function(card) {
    if (card != null) {
      // update card text
      card.cardInfo.packText.x = card.x + 3;
      card.cardInfo.packText.y = card.y + 3;
      card.cardInfo.packText.setText(card.cardInfo.pack.length + 1);
      card.cardInfo.packText.visible = card.visible;

      if (card.dragging) {
        // clamp x/y position
        card.x = PS.Main.clamp(card.x)({lBound: 0, uBound: playRegionX - cardW});
        card.y = PS.Main.clamp(card.y)({lBound: 0, uBound: playRegionY - cardH});

        // highlight card overlap
        var overlap = firstOverlappingCard(card);
        if (overlap != null) {
          overlap.overlapping = true;
          overlapCard = overlap;
        }
      }

      if (card.selected) {
        infoText.setText("gid: " + card.cardInfo.gid);
      }
    }
  });
  cards.forEach(function(card) {
    if (card.overlapping) {
      card.tint = 0x885555;
    } else if (!card.selecting && card.selected) {
      card.tint = 0x558855;
    } else if (!card.selecting) {
      card.tint = 0xffffff;
    }
  });

}

function firstOverlappingCard(card) {
  return cards.find(function(c) {
    return (!(card.x === c.x && card.y === c.y)) && checkOverlap(card, c);
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
  selectCard(sprite);
}

function onInputOut(sprite, pointer) {
  unselectCard(sprite);
}

function onDragStart(sprite, pointer) {
  unselectCard(sprite);
  sprite.height /= 2;
  sprite.width /= 2;
  sprite.dragging = true;
}

function selectCard(card) {
  card.selecting = true;
  card.tint = 0x777777;
}

function unselectCard(card) {
  card.selecting = false;
  card.tint = 0xffffff;
}

function onDragStop(sprite, pointer) {
  sprite.height *= 2;
  sprite.width *= 2;
  sprite.dragging = false;
  if (overlapCard != null) {
    overlapDropMenu.x = sprite.x;
    overlapDropMenu.y = sprite.y;
    sprite.visible = false;
    overlapDropMenu.events.onInputDown.add(overlapDropClick(sprite, overlapCard));
    overlapDropMenu.visible = true;
  }
}

function onInputDown(sprite, pointer) {
  sprite.selected = !sprite.selected;
}

function createCardClick(sprite, pointer) {
  addNewCard();
}

function overlapDropClick(draggedCard, overlapCard) {
  return function () {
    overlapCard.cardInfo.pack.push(draggedCard);
    overlapCard.cardInfo.pack = overlapCard.cardInfo.pack.concat(draggedCard.cardInfo.pack);
    draggedCard.cardInfo.pack = [];
    draggedCard.kill();
    overlapDropMenu.visible = false;
    overlapDropMenu.events.onInputDown.removeAll();
  };
}
