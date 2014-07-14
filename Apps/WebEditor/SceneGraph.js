

function Leaf(vg) {
	this.Geometry = vg;
};

function SurfaceApplicator(surface, scene) {
	this.Surface = surface;
	this.Scene = scene;
};

function TrafoApplicator(trafo, scene) {
	this.Trafo = trafo;
	this.Scene = scene;
};

function Group(children) {
	this.Children = children;
};

Leaf.prototype.traverse = function(traversal) {
	return traversal.DefaultResult;
};

Leaf.prototype.getBoundingBox = function(state) {
	return this.Geometry.BoundingBox;
};

Leaf.prototype.render = function(state) {

	this.RenderView = state.RenderView;

	if(this.Result == undefined || this.Surface != state.Surface) {
		this.Surface = state.Surface;
		this.Result = state.RenderView.Context.createRenderable(state, this, this.Geometry, state.Surface);
	}
	
	
	this.Result.render();
	return true;
};

Group.prototype.traverse = function(traversal) {
	
	var results = new Array();
	for(var i = 0;i < this.Children.length; i++) {
		results.push(traversal.apply(this.Children[i]));
	}
	
	return traversal.combine(results);
};

SurfaceApplicator.prototype.traverse = function(traversal) {
	var old = traversal.State.Surface;
	traversal.State.Surface = this.Surface.getSurface(traversal.State);
	
	var result = traversal.apply(this.Scene);
	
	traversal.State.Surface = old;
	
	return result;
};

TrafoApplicator.prototype.traverse = function(traversal) {
	var old = traversal.State.ModelTrafo;
	var trafo = this.Trafo;
	traversal.State.ModelTrafo = function() { return old().mul(trafo); };
	
	var result = traversal.apply(this.Scene);
	
	traversal.State.ModelTrafo = old;
	
	return result;
};






function TraversalState(renderView) {

	this.RenderView = renderView;
	this.ModelTrafo = function() { return translationTrafo(new V3f(0,0,0)); };
	this.Surface = null;
};

function RenderTraversal(state) {
	this.State = state;
	
	this.DefaultResult = true;
};

function GetBoundingBoxTraversal(state) {
	this.State = state;
	
	this.DefaultResult = new Box3f(new V3f(666,666,666), new V3f(-666,-666,-666));
};

RenderTraversal.prototype.combine = function(array) {
	return true;
};

RenderTraversal.prototype.apply = function(node) {

	var renderType = node.render;
	
	if(renderType == undefined) {
		return node.traverse(this);
	}
	else return node.render(this.State);
};

GetBoundingBoxTraversal.prototype.combine = function(array) {

	var bb = array[0];
	for(var i = 1; i < array.length; i++) {
		bb = bb.union(array[i]);
	}
	
	return bb;

};

GetBoundingBoxTraversal.prototype.apply = function(node) {

	var getbb = node.getBoundingBox;
	
	if(getbb == undefined) {
		return node.traverse(this);
	}
	else return node.getBoundingBox(this.State);

};


function SurfaceLeaf(vs, ps) {

	this.VertexCode = vs;
	this.PixelCode = ps;

};

SurfaceLeaf.prototype.getSurface = function(state) {
	
	if(this.Result == undefined) {
		var vs = state.RenderView.Context.CreateShader(
			this.VertexCode, 
			35633 /*gl.VERTEX_SHADER*/);
		
		var ps = state.RenderView.Context.CreateShader(
			this.PixelCode,
			35632 /*gl.FRAGMENT_SHADER*/);
		
		this.Result = state.RenderView.Context.CreateProgram(vs, ps);
	}
	
	return this.Result;
};


//=========================================================================================================================
//														Uniforms
//=========================================================================================================================
Leaf.prototype.DiffuseColorTexture = function() {
	
	var path = this.Geometry.Material.DiffuseTexture;
	if(path == undefined) {
		var texture = new OpenGlTexture(null); 
		this.RenderView.Context.Textures[path] = texture;
		
		return texture;
	}
	else {
		var texture = this.RenderView.Context.Textures[path];
		if(texture == undefined) {
			texture = this.RenderView.Context.CreateTextureFromFile(path);
			this.RenderView.Context.Textures[path] = texture;
		}
		
		return texture;
	}
	
	/*if(this.DiffuseColorTextureValue == undefined) {
		
		
		if(path == undefined) this.DiffuseColorTextureValue = new OpenGlTexture(null);
		else {
			this.DiffuseColorTextureValue = view.Context.CreateTextureFromFile(path);
		}
	}
	
	return this.DiffuseColorTextureValue;*/
};

Leaf.prototype.HasDiffuseColorTexture = function() {
	
	var path = this.Geometry.Material.DiffuseColorTexture;
	if(path != undefined && path != null)return 1;
	else return 0;
};

Leaf.prototype.NormalMap = function() {

	var path = this.Geometry.Material.NormalMap;
	if(path == undefined) {
		var texture = new OpenGlTexture(null); 
		this.RenderView.Context.Textures[path] = texture;
		
		return texture;
	}
	else {
		var texture = this.RenderView.Context.Textures[path];
		if(texture == undefined) {
			texture = this.RenderView.Context.CreateTextureFromFile(path);
			this.RenderView.Context.Textures[path] = texture;
		}
		
		return texture;
	}
};

Leaf.prototype.HasNormalMap = function() {
	var path = this.Geometry.Material.NormalMap;
	if(path != undefined && path != null)return 1;
	else return 0;
};

TraversalState.prototype.ModelViewTrafo = function() {
	return this.ModelTrafo().mul(this.RenderView.CameraController.ViewTrafo);
};

TraversalState.prototype.ModelViewProjTrafo = function() {
	return this.ModelTrafo().mul(this.RenderView.CameraController.ViewTrafo).mul(this.RenderView.CameraController.ProjTrafo);
};

TraversalState.prototype.ViewProjTrafo = function() {
	return this.RenderView.CameraController.ViewTrafo.mul(this.RenderView.CameraController.ProjTrafo);
};

TraversalState.prototype.CameraPosition = function() {
	return this.RenderView.CameraController.ViewTrafo.Backward.transformPos(new V3f(0,0,0));
};

TraversalState.prototype.LightPosition = function() {
	return this.RenderView.CameraController.ViewTrafo.Backward.transformPos(new V3f(0,0,0));
};
