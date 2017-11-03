"use strict";

exports.phMkCard=function(x, y, textureName) {
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
