


function KeyboardClass() {

	this.Keys = new Array();
	this.Listeners = new Array();
	
	var me = this;
	document.onkeydown = function(e) { me.down(e); };
	document.onkeyup = function(e) { me.up(e); };

};

KeyboardClass.prototype.down = function(event) {

	this.Keys["K" + event.keyCode] = true;

	for(var i = 0; i < this.Listeners.length; i++) {
		try {
			this.Listeners[i].keyDown(event);
		}
		catch(x) { alert(x); }
	}
	
};

KeyboardClass.prototype.up = function(event) {

	this.Keys["K" + event.keyCode] = false;
	
	for(var i = 0; i < this.Listeners.length; i++) {
		try {
			this.Listeners[i].keyUp(event);
		}
		catch(x) { alert(x); }
	}
};


var GlobalKeyboard = new KeyboardClass();

function Keyboard(htmlObject) {

	this.Object = htmlObject;
	this.Keys = new Array();
	
	this.GlobalKeyboard = GlobalKeyboard;
	this.Ctrl = false;
	this.Alt = false;
	this.Shift = false;
	this.Listeners = new Array();
	
	this.GlobalKeyboard.Listeners.push(this);
	
	var me = this;
	/*this.Object.onmouseover = function(e) { htmlObject.focus(); htmlObject.MouseOver = true; };
	this.Object.onmouseout = function(e) { 
		htmlObject.MouseOver = false; 
		
		try {
			for(var i = 0; i < me.Keys.length; i++) {
				
				if(me.isKeyDown(i)) {
					alert(i);
					me.keyUp({ keyCode : i });
					
				}
			}
		}
		catch(x) { alert(x); }
		
	};*/
};

function getFocussedElement() {

	var focused = document.activeElement;
	if (!focused || focused == document.body)
		focused = null;
	else if (document.querySelector)
		focused = document.querySelector(":focus");

	return focused;
}

Keyboard.prototype.addListener = function(listener) {
	this.Listeners.push(listener);
};

Keyboard.prototype.keyDown = function(event) {

	if(event.target.nodeName == "INPUT")return;
	
	if(event.keyCode == Keys.Ctrl) this.Ctrl = true;
	else if(event.keyCode == Keys.Alt) this.Alt = true;
	else if(event.keyCode == Keys.Shift) this.Shift = true;
	
	this.Keys["K" + event.keyCode] = true;

	for(var i = 0; i < this.Listeners.length; i++) {
		try {
			this.Listeners[i].keyDown(event);
		}
		catch(x) { alert(x); }
	}
	
};

Keyboard.prototype.keyUp = function(event) {
	
	//if(!this.Object.MouseOver)return;

	if(event.keyCode == Keys.Ctrl) this.Ctrl = false;
	else if(event.keyCode == Keys.Alt) this.Alt = false;
	else if(event.keyCode == Keys.Shift) this.Shift = false;
	
	this.Keys["K" + event.keyCode] = false;
	
	for(var i = 0; i < this.Listeners.length; i++) {
		try {
			this.Listeners[i].keyUp(event);
		}
		catch(x) { alert(x); }
	}
};

Keyboard.prototype.isKeyDown = function(key) {

	var d = this.Keys["K" + key];
	return d != undefined && d == true;
};



Keys = {
	Backspace : 8,
	Tab : 9,
	Enter : 13,
	Shift : 16,
	Ctrl : 17,
	Alt : 18,
	Break : 19,
	CapsLock : 20,
	Escape : 27,
	PageUp : 33,
	PageDown : 34,
	End : 35,
	Home : 36,
	Left : 37,
	Up : 38,
	Right : 39,
	Down : 40,
	Insert : 45,
	Delete : 46,
	N0 : 48,
	N1 : 49,
	N2 : 50,
	N3 : 51,
	N4 : 52,
	N5 : 53,
	N6 : 54,
	N7 : 55,
	N8 : 56,
	N9 : 57,
	A : 65,
	B : 66,
	C : 67,
	D : 68,
	E : 69,
	F : 70,
	G : 71,
	H : 72,
	I : 73,
	J : 74,
	K : 75,
	L : 76,
	M : 77,
	N : 78,
	O : 79,
	P : 80,
	Q : 81,
	R : 82,
	S : 83,
	T : 84,
	U : 85,
	V : 86,
	W : 87,
	X : 88,
	Y : 89,
	Z : 90,
	WinLeft : 91,
	WinRight : 92,
	Select : 93,
	Numpad0 : 96,
	Numpad1 : 97,
	Numpad2 : 98,
	Numpad3 : 99,
	Numpad4 : 100,
	Numpad5 : 101,
	Numpad6 : 102,
	Numpad7 : 103,
	Numpad8 : 104,
	Numpad9 : 105,
	Multiply : 106,
	Add : 107,
	Subtract : 109,
	DecimalPoint : 110,
	Divide : 111,
	F1 : 112,
	F2 : 113,
	F3 : 114,
	F4 : 115,
	F5 : 116,
	F6 : 117,
	F7 : 118,
	F8 : 119,
	F9 : 120,
	F10 : 121,
	F11 : 122,
	F12 : 123,
	NumLock : 144,
	ScrollLock : 145,
	Semicolon : 186,
	Equal : 187,
	Comma : 188,
	Dash : 189,
	Period : 190,
	ForwardSlash : 191,
	GraveAccent : 192,
	OpenBracket : 219,
	Backslash : 220,
	CloseBracket : 221,
	SingleQuote : 222,
	
	PageUp : 33,
	PageDown : 34,
};

