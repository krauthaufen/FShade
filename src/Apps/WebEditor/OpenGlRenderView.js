
//var gl = null;
var renderViews = new Array();
//must override Prepare and Render

function OpenGlRenderView(htmlObject, viewTrafo, projTrafo) {

	this.Canvas = htmlObject;
	this.GL = null;
	this.initGl();
	
	this.OnFrameTimeUpdate = new Array();
	
	
	this.Context = new OpenGlContext(this.GL);
	this.Mouse = new Mouse(htmlObject);
	this.Keyboard = new Keyboard(htmlObject);
	this.CameraController = new CameraController(this.Mouse, this.Keyboard, viewTrafo, projTrafo);
	
	
	this.FrameTime = 0.0;
	this.FPS = 0.0;
	this.FrameCount = 0;
	
	this.LastFrameCount = 0;
	this.LastMeasured = 0.0;
	this.LastFrameTime = 0.0;
};

OpenGlRenderView.prototype.run = function() {
	try {
		this.Prepare();
	}
	catch(x) { alert(x); }
	
	renderViews.push(this);
	renderTick();
};

OpenGlRenderView.prototype.resizeCanvas = function() {
   // only change the size of the canvas if the size it's being displayed
   // has changed.
   if (this.Canvas.width != this.Canvas.clientWidth ||
       this.Canvas.height != this.Canvas.clientHeight) {
     // Change the size of the canvas to match the size it's being displayed
     this.Canvas.width = this.Canvas.clientWidth;
     this.Canvas.height = this.Canvas.clientHeight;
	 
	 try {
		this.Resize();
	 }
	 catch(x) { }
	 
	 this.GL.viewport(0, 0, this.Canvas.width, this.Canvas.height);
   }
}

OpenGlRenderView.prototype.initGl = function() {
	
	try {
		//canvas = document.getElementById("webgl");
		this.Canvas.onselectstart = function() { return false; };
	
		this.Canvas.focus();
	
		// Initialize the global variable gl to null.
		this.GL = null;
		  
		// Try to grab the standard context. If it fails, fallback to experimental.
		this.GL = this.Canvas.getContext("webgl") || this.Canvas.getContext("experimental-webgl");

		  
		//alert("GL: " + gl);
		// If we don't have a GL context, give up now
		if (!this.GL || this.GL == undefined) {
			alert("Unable to initialize WebGL. Your browser may not support it.");
		}
		
		
		this.GL.enable(this.GL.DEPTH_TEST);
		this.GL.disable(this.GL.CULL_FACE);
	} catch (err) {
		throw "Your web browser does not support WebGL!";
	}
};

requestAnimFrame = (function() {
  return window.requestAnimationFrame ||
	 window.webkitRequestAnimationFrame ||
	 window.mozRequestAnimationFrame ||
	 window.oRequestAnimationFrame ||
	 window.msRequestAnimationFrame ||
	 function(/* function FrameRequestCallback */ callback, /* DOMElement Element */ element) {
	   window.setTimeout(callback, 1000/200);
	 };
})();
	
function renderTick() {
	requestAnimFrame(renderTick);
	
	for(var i = 0; i < renderViews.length; i++) {

		var view = renderViews[i];
		
		var t = new Date().getTime();
		
		if(view.LastMeasured == 0.0) view.LastMeasured = t;
	
		if(t - view.LastMeasured > 100 && view.FrameCount > view.LastFrameCount) {
			var count = view.FrameCount - view.LastFrameCount;
			view.FrameTime = (t - view.LastMeasured) / (1000.0 * count);
			view.FPS = 1.0 / view.FrameTime;
			view.LastMeasured = t;
			view.LastFrameCount = view.FrameCount;
		
			for(var j = 0; j < view.OnFrameTimeUpdate.length; j++) {
				view.OnFrameTimeUpdate[j]();
			}
		
		}
		
		
		//try {
			view.CameraController.idle(view.FrameTime);
			view.resizeCanvas();
		
			view.Render();
		//}
		//catch(x) { alert(x);}
		
		view.FrameCount++;
	}
};