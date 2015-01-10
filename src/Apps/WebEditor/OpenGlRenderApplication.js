
function OpenGlRenderApplication(htmlObject) {
	this.Object = htmlObject;
	this.SceneGraph = null;
	this.RenderView = new OpenGlRenderView(htmlObject);
	
	
	
	this.RootState = new TraversalState(this.RenderView);
	this.RenderTraversal = new RenderTraversal(this.RootState);
	this.GetBoundingBoxTraversal = new GetBoundingBoxTraversal(this.RootState);
	
	var me = this;
	
	
	this.RenderView.Resize = function() {
		this.RenderView.CameraController.ProjTrafo = perspectiveTrafo(60.0 / 57.296, this.RenderView.Canvas.width / this.RenderView.Canvas.height, 0.1, 1000.0); 
	};
	
	this.RenderView.Prepare = function() {
	};
	
	this.RenderView.Render = function() {
		me.RenderTraversal.apply(this.SceneGraph);
	};
	
	
};


OpenGlRenderApplication.prototype.run = function() {
	this.RenderView.run();
};