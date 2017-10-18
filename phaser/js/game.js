var game = new Phaser.Game(
	  800, 600, Phaser.AUTO, '',
	  { preload: preload, create: create, update: update }
);

var cardH = 60;
var cardW = 60;

var preview;
var prevH = cardH * 2;
var prevW = cardW * 2;

function preload() {
    game.load.image('card', 'assets/card.png');
  	game.load.image('menu', 'assets/menu.png');
  	game.load.image('empty', 'assets/empty.png');
}

function create() {
    // Menu

    // - bottom

    var botMenH = 100;
  	var botMenY = game.world.height - botMenH;
    var bottomMenu = game.add.sprite(0, botMenY, 'menu');
  	bottomMenu.height = botMenH;
    bottomMenu.width = game.world.width;

	  // - right

    var rgtMenH = 200;
	  var rgtMenX = game.world.width - rgtMenH;
    bottomMenu = game.add.sprite(rgtMenX, 0, 'menu');
  	bottomMenu.height = game.world.height - botMenH;
    bottomMenu.width = rgtMenH;

    // - right - card preview
  	preview = game.add.sprite(rgtMenX + 40, 30, 'empty');
  	preview.height = prevH;
	  preview.width = prevW;

    // Cards
    var card = game.add.sprite(60, 60, 'card');
  	card.height = cardH;
  	card.width = cardW;

    card.inputEnabled = true;
    card.input.enableDrag();
    card.events.onDragStart.add(onDragStart, this);
    card.events.onDragStop.add(onDragStop, this);
  	card.events.onInputOver.add(onInputOver, this);

  	card.cardInfo = {
	  	texture: 'card'
  	};
}

function update() {
}

function onInputOver(sprite, pointer) {
  if ('cardInfo' in sprite) {
      preview.loadTexture(sprite.cardInfo.texture, 0, false);
	  	preview.height = prevH;
		  preview.width = prevW;
      PS.Main.logTest();
	}
}

function onDragStart(sprite, pointer) {

}

function onDragStop(sprite, pointer) {

}
