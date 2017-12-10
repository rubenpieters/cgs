const WebSocket = require("uws");
//const PS = require("./purs.js");

const wss = new WebSocket.Server({ port: 8080 });

function init() {
  console.log("init");
  //PS.Main.startServer();
};

init();
