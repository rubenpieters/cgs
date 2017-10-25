var gameH = 600;
var gameW = 800;

var game = new Phaser.Game(
	  gameW, gameH, Phaser.AUTO, '',
	  { preload: preload, create: create, update: update }
);

var botMenH = 100;
var rgtMenH = 200;
var playRegionY = gameH - botMenH;
var playRegionX = gameW - rgtMenH;

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

    var bottomMenu = game.add.sprite(0, playRegionY, 'menu');
  	bottomMenu.height = botMenH;
    bottomMenu.width = gameW;

	  // - right

    bottomMenu = game.add.sprite(playRegionX, 0, 'menu');
  	bottomMenu.height = gameH - botMenH;
    bottomMenu.width = rgtMenH;

    // - right - card preview
  	preview = game.add.sprite(playRegionX + 40, 30, 'empty');
  	preview.height = prevH;
	  preview.width = prevW;

    // Cards
    card = game.add.sprite(60, 60, 'card');
  	card.height = cardH;
  	card.width = cardW;

		card.dragging = false;
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
  if (card != null && card.dragging) {
    card.x = PS.Main.clamp(card.x)({lBound: 0, uBound: playRegionX - cardW});
	  card.y = PS.Main.clamp(card.y)({lBound: 0, uBound: playRegionY - cardH});
	}

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
  sprite.dragging = true;

}

function onDragStop(sprite, pointer) {
  sprite.dragging = false;

}
