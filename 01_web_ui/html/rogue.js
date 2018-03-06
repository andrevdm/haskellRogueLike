var sendCmd = ( c, d ) => {}
var tilesMain = null;

//!SECTION< 01_config
var config = { "drawId": 0,
               "help": "",
               "tileWidth": 32,
               "tileHeight": 32,
               "gridWidth": 0,
               "gridHeight" : 0
             };
//!SECTION> 01_config


//!SECTION< 01_sendKey
function sendKey(k){
  //Stop next moves, until the server responds
  Mousetrap.pause();

  const act = () => {
    sendCmd("key", k);
  };
  
  act();
}
//!SECTION> 01_sendKey

//!SECTION< 01_runWebSocket_fn
function runWebSocket(userName)
{
//!SECTION< 01_runWebSocket_open
  var ws = new WebSocket("ws://localhost:61492/");
  ws.binaryType = 'arraybuffer';
//!SECTION> 01_runWebSocket_open
	
//!SECTION< 01_runWebSocket_onopen
  ws.onopen = function() {
    sendCmd = ( c, d ) => {
      ws.send(c + "|" + (d || ""));
    }

    sendCmd("init", gridSizeStr());
  };
//!SECTION> 01_runWebSocket_onopen
  
//!SECTION< 01_runWebSocket_onmessage_decode
  ws.onmessage = function (evt) { 
    var bytes = new Uint8Array(evt.data);
    var m = bzip2.simple(bzip2.array(bytes));
    cmd = JSON.parse(m);
//!SECTION> 01_runWebSocket_onmessage_decode

//!SECTION< 01_runWebSocket_onmessage_switch
    switch( cmd.cmd ){
      case "config":
        //Load keys
        config.help = "";
        for( var i in cmd.data.keys ){
          const s = cmd.data.keys[i].shortcut;
          const a = cmd.data.keys[i].action; //const avoids variable capture in the closure
          Mousetrap.bind( s, () => sendKey( a ), "keyup" ); 
          config.help += s + ": " + a + "\n";
        }

        sendCmd("redraw", gridSizeStr());
        break;
        
      case "log":
        console.log( cmd.message );
        break;
        
      case "error":
        alert( cmd.message );
        break;
    }
//!SECTION> 01_runWebSocket_onmessage_switch
    
    Mousetrap.unpause();
  };
  
  ws.onclose = function() { 
    sendCmd = ( c, d ) => {}
  };
		
  window.onbeforeunload = function(evt) {
    sendCmd = ( c, d ) => {}
    socket.close();
  };
}
//!SECTION> 01_runWebSocket_fn

//!SECTION< 01_resizeCanvas
function resizeCanvas() {
  var container = document.getElementById("container");

  var main = document.getElementById("main");
  var mainr = main.getBoundingClientRect();

  const resize = n => {
    var e = document.getElementById(n);
    e.style.main = mainr.x;
    e.style.top = mainr.y
    e.style.width = mainr.width;
    e.style.height = mainr.height;
    e.width = mainr.width;
    e.height = mainr.height;

    config.blank = null;
    config.gridWidth = Math.floor(e.width / config.tileWidth);
    config.gridHeight = Math.floor(e.height / config.tileHeight);
  };

  R.forEach( resize, ["tilesCanvas", "topInteractionLayer"] );
  
  sendCmd("redraw", gridSizeStr() );
}
//!SECTION> 01_resizeCanvas

//!SECTION< 01_start
function start(){
  tilesMain = document.getElementById("tilesMain");
  resizeCanvas();
  runWebSocket();
}
//!SECTION> 01_start

function gridSizeStr(){
  return (config.gridWidth) + "|" + (config.gridHeight);
}

//!SECTION< 01_debounce
//Debounce: https://gist.github.com/nmsdvid/8807205
function debounce(func, wait, immediate) {
  var timeout;
  return function() {
    var context = this, args = arguments;
    clearTimeout(timeout);
    timeout = setTimeout(function() {
      timeout = null;
      if (!immediate)
        func.apply(context, args);
    }, wait);
    if (immediate && !timeout) func.apply(context, args);
  };
}

window.addEventListener('resize', debounce(resizeCanvas, 250), false);
//!SECTION> 01_debounce


//!SECTION< 01_mousetrap_bind
Mousetrap.bind( "?", () => alert( config.help ) ); 
//!SECTION> 01_mousetrap_bind
