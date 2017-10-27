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

var cardH = 40;
var cardW = 25;

var preview;
var prevH = cardH * 3;
var prevW = cardW * 3;

var cards = [];

var newCardMarker = {
	x: 60,
	y: 60,
}

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

    // Cards

		cards.push(addNewCard());
}

function addNewCard() {
	var newCard = mkCard(newCardMarker.x, newCardMarker.y, 'card');
	newCardMarker.x += 5;
	newCardMarker.y += 7;
	return newCard;
}

function mkCard(x, y, textureName) {
	card = game.add.sprite(x, y, textureName);
	card.height = cardH;
	card.width = cardW;

	card.dragging = false;
	card.inputEnabled = true;
	card.input.enableDrag(false, true);
	card.events.onDragStart.add(onDragStart, this);
	card.events.onDragStop.add(onDragStop, this);
	card.events.onInputOver.add(onInputOver, this);
	card.events.onInputOut.add(onInputOut, this);

	card.cardInfo = {
		texture: 'card'
	};
	return card;
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
	}
	sprite.tint = 0x777777;
}

function onInputOut(sprite, pointer) {
	sprite.tint = 0xffffff;
}

function onDragStart(sprite, pointer) {
  sprite.dragging = true;

}

function onDragStop(sprite, pointer) {
  sprite.dragging = false;

}

function createCardClick(sprite, pointer) {
	addNewCard();
}
