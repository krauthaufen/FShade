
//listeners can listen for mouseMoved, mouseDown, mouseUp, mouseScroll
function Mouse(htmlObject) {

	this.Object = htmlObject;
	
	this.Right = false;
	this.Left = false;
	this.Middle = false;
	this.Position = new V2f(0,0);
	
	
	this.Listeners = new Array();
	
	var me = this;
	htmlObject.onmousedown =function(e) { me.down(e); };
	htmlObject.onmouseup =function(e) { me.up(e); };
	htmlObject.onmousemove = function(e) { me.moved(e); };

	var scroll = function(e) { me.scroll(e); };
	htmlObject.onmousewheel = scroll;
	htmlObject.addEventListener ("DOMMouseScroll", scroll, false);
};

Mouse.prototype.addListener = function(listener) {
	this.Listeners.push(listener);
};

Mouse.prototype.moved = function(event) {
	
	this.Position = new V2f(event.clientX, event.clientY);
	//alert("move");
	for(var i = 0; i < this.Listeners.length; i++) {
		try {
			this.Listeners[i].mouseMoved(event);
		}
		catch(x) { alert(x); }
	}
	
};

Mouse.prototype.down = function(event) {

	if(event.button == 0) this.Left = true;
	else if(event.button == 1) this.Middle = true;
	else if(event.button == 2) this.Middle = true;
	
	this.Position = new V2f(event.clientX, event.clientY);
	
	for(var i = 0; i < this.Listeners.length; i++) {
		try {
			this.Listeners[i].mouseDown(event);
		}
		catch(x) { alert(x); }
	}
	
};

Mouse.prototype.up = function(event) {
	
	if(event.button == 0) this.Left = false;
	else if(event.button == 1) this.Middle = false;
	else if(event.button == 2) this.Middle = false;
	
	this.Position = new V2f(event.clientX, event.clientY);
	
	for(var i = 0; i < this.Listeners.length; i++) {
		try {
			this.Listeners[i].mouseUp(event);
		}
		catch(x) { alert(x); }
	}
	
};

Mouse.prototype.scroll = function(event) {
	var delta = 0;
	if (!event) /* For IE. */
			event = window.event;
	if (event.wheelDelta) { /* IE/Opera. */
			delta = event.wheelDelta/120;
	} else if (event.detail) { /** Mozilla case. */
			/** In Mozilla, sign of delta is different than in IE.
			 * Also, delta is multiple of 3.
			 */
			delta = -event.detail/3;
	}
	/** If delta is nonzero, handle it.
	 * Basically, delta is now positive if wheel was scrolled up,
	 * and negative, if wheel was scrolled down.
	 */
	if (delta) {

		for(var i = 0; i < this.Listeners.length; i++) {
			try {
				this.Listeners[i].mouseScroll(delta);
			}
			catch(x) { alert(x); }
		}
	}
	/** Prevent default actions caused by mouse wheel.
	 * That might be ugly, but we handle scrolls somehow
	 * anyway, so don't bother here..
	 */
	if (event.preventDefault)
			event.preventDefault();
		
	event.returnValue = false;
};