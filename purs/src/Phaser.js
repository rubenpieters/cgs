"use strict";



exports.createFunc=function(game) {
  return function() {
    var botMenH = 100;
    var rgtMenH = 200;
    var playRegionY = gameH - botMenH;
    var playRegionX = gameW - rgtMenH;

    var cardH = 40;
    var cardW = 24;

    var preview;
    var prevH = cardH * 3;
    var prevW = cardW * 3;

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
  };
};
