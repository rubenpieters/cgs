"use strict";

exports.getClientPlayerId=function() {
  return clientPlayerId;
};

exports.setClientPlayerId=function(id) {
  return function() {
    clientPlayerId = id;
  };
};

exports.clearPhaserState=function() {

};

// {x :: Int, y :: Int, texture :: String, size :: Int, pack :: Pack}
exports.materializeCard=function(o) {
  return function() {
    var card = game.add.sprite(o.x, o.y, o.texture);
    card.height = cardH;
    card.width = cardW;

    card.inputEnabled = true;
    card.events.onInputDown.add(cardInputDown, this);
    card.events.onInputUp.add(cardInputUp, this);

    var style = { font: "10px Arial", fill: "#ffffff", align: "center", stroke: "black", strokeThickness: 1};
    var packText = game.add.text(card.x + 3, card.y + 3, o.size, style);
    console.log("test: " + JSON.stringify(o.pack));
    card.packText = packText;
    card.props = {
      selected: false,
      dragging: false,
      overlapped: false,
      inhand: false,
      // copy pack info from o.pack
      gid: o.pack.gid,
      cards: o.pack.cards,
      lockedBy: o.pack.lockedBy,
    };

    cardGroup.add(card);

    return card;
  };
};

exports.getProps=function(c) {
  return function() {
    return c.props;
  };
};

exports.setProps=function(props) {
  return function(c) {
    return function() {
      c.props = props;
    };
  };
};

exports.activateDragTrigger=function() {
  //if (dragTrigger.c.pack.dragMode === "drag") {
    dragTrigger.c.props.dragging = true;
  dragTrigger = { status: "dragging", left: dragTrigger.left, right: dragTrigger.right, c: dragTrigger.c, x: dragTrigger.x, y: dragTrigger.y };
    dragTriggerText.text = "dragging";
    console.log("drag");
  /*} else {
    var newCard = PS.ClientMain.drawFromPack({x: dragTrigger.c.x, y: dragTrigger.c.y})(dragTrigger.c)();
    dragTrigger = { status: "dragging", left: dragTrigger.left, right: dragTrigger.right, c: newCard };
    console.log("draw");
  }*/
};

exports.setDragTrigger=function(c) {
  return function() {
    //if (dragTrigger.c.pack.dragMode === "drag") {
      c.props.dragging = true;
    dragTrigger = { status: "dragging", left: dragTrigger.left, right: dragTrigger.right, c: c, x: dragTrigger.x, y: dragTrigger.y };
      dragTriggerText.text = "dragging";
      console.log("drag");
    /*} else {
      var newCard = PS.ClientMain.drawFromPack({x: dragTrigger.c.x, y: dragTrigger.c.y})(dragTrigger.c)();
      dragTrigger = { status: "dragging", left: dragTrigger.left, right: dragTrigger.right, c: newCard };
      console.log("draw");
    }*/
  };
};

exports.resetDragTrigger=function() {
  dragTrigger = { status: "none" };
  dragTriggerText.text = "none";
};

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
      dragMode: "drag"
    };

    globalId++;
    newCardMarker.x += 5;
    newCardMarker.y += 7;
    gameState = PS.ClientMain.addCard(card)(gameState)();
    cardGroup.add(card);

    return card;
  };
};

exports.toggleSelected=function(card) {
  return function() {
    card.pack.selected = ! card.pack.selected;
    PS.ClientMain.updateCardTint(card)();
  };
};

exports.updateCardInfo=function(card) {
  return function(newInfo) {
    return function() {
      Object.assign(card.pack, newInfo);
    };
  };
};

exports.setTint=function(color) {
  return function(card) {
    return function() {
      card.tint = color;
    };
  };
};

exports.setZoneTint=function(color) {
  return function(zone) {
    return function() {
      zone.tint = color;
    };
  };
};

exports.showCardSelectMenu=function(card) {
  return function() {
    cardDragProp.visible = true;
    cardDragPropText.visible = true;
    cardDragPropText.setText("dragMode: " + card.pack.dragMode);
    cardDragProp.events.onInputDown.add(exports.toggleCardDrag(card));
  };
};

exports.toggleCardDrag=function(card) {
  return function() {
    if (card.pack.dragMode === "drag") {
      card.pack.dragMode = "draw";
    } else if (card.pack.dragMode === "draw") {
      card.pack.dragMode = "drag";
    } else {
      throw "unknown Drag Mode!";
    }
    cardDragPropText.setText("dragMode: " + card.pack.dragMode);
  };
};

exports.hideCardSelectMenu=function() {
  cardDragProp.visible = false;
  cardDragPropText.visible = false;
  cardDragProp.events.onInputDown.removeAll();
};

const checkOverlap=function(c1) {
  return function(c2) {
    var bound1 = c1.getBounds();
    var bound2 = c2.getBounds();
    return Phaser.Rectangle.intersects(bound1, bound2);
  };
};

exports.checkOverlap = checkOverlap;

exports.gameState=function() {
  return gameState;
};

exports.getGameState=function() {
  return gameState;
};

exports.setGameState=function(gs) {
  return function() {
    gameState = gs;
  };
};

exports.getOverlapCard=function() {
  return overlapCard;
};

exports.clearOverlapCard=function() {
  overlapCard = undefined;
};

exports.setMOverlapCard=function(c) {
  return function() {
    overlapCard = c;
  };
};

exports.updateDraggedCard=function(card) {
  return function() {
    var newX = PS.ClientMain.clamp(game.input.x - (cardW / 2))({lBound: 0, uBound: playRegionX - cardW});
    var newY = PS.ClientMain.clamp(game.input.y - (cardH / 2))({lBound: 0, uBound: playRegionY - cardH});
    card.x = newX;
    card.y = newY;
    card.packText.x = newX + 3;
    card.packText.y = newY + 3;
    // TODO: should this check be here? Maybe move to updateCard
    if (checkOverlap(card)(playerHandZone)) {
      playerHandZone.tint = 0xd366ce;
    } else {
      playerHandZone.tint = 0xd3ffce;
    }
  };
};

exports.handZoneNoHighlight=function() {
  playerHandZone.tint = 0xd3ffce;
};

exports.isConnected=function() {
  return connected;
};

exports.moveCard=function(inputX) {
  return function(inputY) {
    return function(card) {
      return function() {
        var newX = PS.ClientMain.clamp(inputX)({lBound: 0, uBound: playRegionX - cardW});
        var newY = PS.ClientMain.clamp(inputY)({lBound: 0, uBound: playRegionY - cardH});
        card.x = newX;
        card.y = newY;
        card.packText.x = newX + 3;
        card.packText.y = newY + 3;
      };
    };
  };
};

exports.phaserProps=function(card) {
  return function() {
    return card;
  };
};

exports.phKill=function(o) {
  return function() {
    o.packText.kill();
    o.kill();
    gameState = PS.ClientMain.removeCardGS(o)(gameState)();
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

exports.phSetVisible=function(o) {
  return function() {
    o.packText.visible = true;
    o.visible = true;
  };
};

exports.phSetInvisible=function(o) {
  return function() {
    o.visible = false;
    o.packText.visible = false;
  };
};

exports.phSetPos=function(pos) {
  return function(o) {
    return function() {
      o.x = pos.x;
      o.y = pos.y;
      o.packText.x = pos.x;
      o.packText.y = pos.y;
    };
  };
};

exports.setCards=function(cards) {
  return function(c) {
    return function() {
      c.props.cards = cards;
      c.packText.setText(c.props.cards.length);
      const packText = PS.ClientMain.packText(cards);
      if (typeof selectedCard !== "undefined") {
        infoText.text = "gid: " + selectedCard.props.gid + "\n" + packText;
      }
    };
  };
};

exports.setPackText=function(pack) {
  return function() {
    pack.packText.setText(pack.props.cards.length);
    const packText = PS.ClientMain.packText(pack.props.cards);
    if (typeof selectedCard !== "undefined") {
      infoText.text = "gid: " + selectedCard.props.gid + "\n" + packText;
    }
  };
};

// server interacting code

exports.getSocket=function() {
  return socket;
};

exports.unsafeEmit=function(socket) {
  return function(data) {
    return function() {
      socket.send(data);
    };
  };
};


exports.getEntities=function() {
  return entities;
};

exports.setEntities=function(es) {
  return function() {
    entities = es;
  };
};

exports.updateDragged=function(card) {
  return function() {
    var newX = PS.ClientMain.clamp(game.input.x - (cardW / 2))({lBound: 0, uBound: playRegionX - cardW});
    var newY = PS.ClientMain.clamp(game.input.y - (cardH / 2))({lBound: 0, uBound: playRegionY - cardH});
    card.x = newX;
    card.y = newY;
    card.packText.x = newX + 3;
    card.packText.y = newY + 3;
  };
};

exports.getXY=function(o) {
  return function() {
    return { x: o.x, y: o.y };
  };
};

exports.setPackXY=function(xy) {
  return function(card) {
    return function() {
      card.x = xy.x;
      card.y = xy.y;
      card.packText.x = xy.x + 3;
      card.packText.y = xy.y + 3;
    };
  };
};

exports.fieldDrop=function(c) {
  return function() {
    console.log("dropping " + c.props.gid + " in field");
    eventBuffer.push(new PS.SharedData.ClDrop(c.props.gid, { x: c.x, y: c.y }));
  };
};

exports.inPackDrop=function(c) {
  return function(tgt) {
    return function() {
      console.log("dropping " + c.props.gid + " into " + tgt.props.gid);
      eventBuffer.push(new PS.SharedData.ClDropIn(c.props.gid, { tgt: tgt.props.gid }));
    };
  };
};

exports.inZoneDrop=function(c) {
  return function(zone) {
    return function() {
      console.log("dropping " + c.props.gid + " into " + "zone");
      eventBuffer.push(new PS.SharedData.ClToHand(c.props.gid));
    };
  };
};

exports.playerHandZone=function() {
  return playerHandZone;
};
