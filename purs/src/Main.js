"use strict";

// {x :: Int, y :: Int, pack :: Array Card}
exports.phMkCard=function(o) {
  return function() {
    console.log("mk card");
    var card = game.add.sprite(o.x, o.y, o.pack[0].texture);
    card.height = cardH;
    card.width = cardW;

    card.inputEnabled = true;
    //  card.input.enableDrag(false, true);
    //  card.events.onDragStart.add(onDragStart, this);
    //  card.events.onDragStop.add(onDragStop, this);
    //  card.events.onInputOver.add(onInputOver, this);
    //  card.events.onInputOut.add(onInputOut, this);
    card.events.onInputDown.add(cardInputDown, this);
    card.events.onInputUp.add(cardInputUp, this);

    var style = { font: "10px Arial", fill: "#ffffff", align: "center", stroke: "black", strokeThickness: 1};
    var packText = game.add.text(card.x + 3, card.y + 3, o.pack.length, style);
    //  packText.visible = false;

    card.pack = {
      pack: o.pack,
      packText: packText,
      gid: globalId,
      selected: false,
      dragging: false,
      overlapped: false,
    };

    globalId++;
    newCardMarker.x += 5;
    newCardMarker.y += 7;
    gameState = PS.Main.addCard(card)(gameState)();
    cardGroup.add(card);

    return card;
  };
};

exports.packInfo=function(card) {
  return function() {
    return card.pack;
  };
};

exports.toggleSelected=function(card) {
  return function() {
    card.pack.selected = ! card.pack.selected;
    PS.Main.updateCardTint(card)();
  };
};

exports.updateCardTint=function(card) {
  return function() {
    if (card.pack.selected) {
      card.tint = 0x00ff00;
    } else {
      card.tint = 0xffffff;
    }
  };
};

exports.updateCardInfo=function(card) {
  return function(newInfo) {
    return function() {
      Object.assign(card.pack, newInfo);
    };
  };
};

exports.setTint=function(card) {
  return function(color) {
    return function() {
      card.tint = color;
    };
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

exports.checkOverlap=function(c1) {
  return function(c2) {
    var bound1 = c1.getBounds();
    var bound2 = c2.getBounds();
    return Phaser.Rectangle.intersects(bound1, bound2);
  };
};

exports.gameState=function() {
  return gameState;
};

exports.updateDraggedCard=function(card) {
  return function() {
    var newX = PS.Main.clamp(game.input.x - (cardW / 2))({lBound: 0, uBound: playRegionX - cardW});
    var newY = PS.Main.clamp(game.input.y - (cardH / 2))({lBound: 0, uBound: playRegionY - cardH});
    card.x = newX;
    card.y = newY;
    card.pack.packText.x = newX + 3;
    card.pack.packText.y = newY + 3;
  };
};

exports.phaserProps=function(card) {
  return function() {
    return card;
  };
};

exports.phKill=function(o) {
  return function() {
    o.pack.packText.kill();
    o.kill();
  };
};

exports.phLoadTexture=function(o) {
  return function(texture) {
    return function(frame) {
      return function (stopAnimation) {
        return function() {
          console.log("t " + texture + ", f " + frame + ", sa: " + stopAnimation);
          o.loadTexture(texture, frame, stopAnimation);
          // need to reset height/width, otherwise it takes texture dimensions
          o.height = cardH;
          o.width = cardW;
        };
      };
    };
  };
};
