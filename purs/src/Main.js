"use strict";

// {x :: Int, y :: Int, textureName :: String}
exports.phMkCard=function(o) {
  return function() {
    console.log("mk card");
    var card = game.add.sprite(o.x, o.y, o.textureName);
    card.height = cardH;
    card.width = cardW;

    card.dragging = false;
    card.selecting = false;
    card.overlapping = false;
    card.selected = false;
    card.inputEnabled = true;
    card.input.enableDrag(false, true);
    //  card.events.onDragStart.add(onDragStart, this);
    //  card.events.onDragStop.add(onDragStop, this);
    //  card.events.onInputOver.add(onInputOver, this);
    //  card.events.onInputOut.add(onInputOut, this);
    card.events.onInputDown.add(onInputDown, this);

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
    newCardMarker.x += 5;
    newCardMarker.y += 7;
    gameState = PS.Main.addCard(card)(gameState);
    cardGroup.add(card);

    return card;
  };
};

exports.cardInfo=function(card) {
  return card.cardInfo;
};

exports.toggleSelected=function(card) {
  return function() {
    card.cardInfo.selected = ! card.cardInfo.selected;
    PS.Main.updateCardTint(card)();
  };
};

exports.updateCardTint=function(card) {
  return function() {
    if (card.cardInfo.selected) {
      card.tint = 0x00ff00;
    } else {
      card.tint = 0xffffff;
    }
  };
};

exports.showCardSelectMenu=function(card) {
  return function() {
    cardDragProp.visible = true;
    cardDragPropText.visible = true;
    //  cardDragProp.events.onInputDown.add(toggleCardDrag(card));
  };
};

exports.hideCardSelectMenu=function() {
  cardDragProp.visible = false;
  cardDragPropText.visible = false;
  cardDragProp.events.onInputDown.removeAll();
};

exports.checkOverlap=function(c1, c2) {
  var bound1 = spriteA.getBounds();
  var bound2 = spriteB.getBounds();
  return Phaser.Rectangle.intersects(bound1, bound2);
};

exports.gameState=function() {
  return gameState;
};

