<!DOCTYPE html>
<html>
<head>
	<meta charset="utf-8" />
	<title>Example</title>
	<style>
	  #loading {
		position: absolute;
		left: 50%;
		top: 50%;
		
		font-family:Verdana;
		font-size: 15px;
		z-index: 1;
		
	  }
	  #help {
		font-family:Verdana;
		font-size: 15px;
		position: absolute;
		bottom: 20px;
		right: 20px;
		z-index: 1;
		display: none;
      }
      #renderer {
		position: relative;
      }
    </style>

	
	<script id="VS" type="x-shader/x-vertex">
	attribute vec3 Positions; 
	attribute vec2 TexCoords;
	attribute vec3 Normals;
	attribute vec3 Tangents;
	attribute vec3 BiNormals;


	varying vec2 outCoord;
	varying vec3 lightDirection;
	varying vec3 cameraDirection;
	varying vec3 normal;
	varying vec3 binormal;
	varying vec3 tangent;

	uniform vec3 LightPosition;
	uniform vec3 CameraPosition;
	uniform mat4 ViewProjTrafo; 
	uniform mat4 ModelTrafo; 

	void main() 
	{ 
		vec3 p = (ModelTrafo * vec4(Positions,1.0)).xyz;
		gl_Position = ViewProjTrafo * vec4(p,1.0); 
		
		
		lightDirection = normalize(LightPosition - p);
		cameraDirection = normalize(CameraPosition - p);
		normal = Normals;
		binormal = BiNormals;
		tangent = Tangents;
		
		outCoord = TexCoords;
	}
	</script>

	<script id="PS" type="x-shader/x-vertex">
	precision highp float;

	uniform sampler2D DiffuseColorTexture; 
	uniform sampler2D NormalMap; 
	uniform int HasNormalMap;

	varying vec2 outCoord;
	varying vec3 lightDirection;
	varying vec3 cameraDirection;
	varying vec3 normal;
	varying vec3 binormal;
	varying vec3 tangent;

	void main() 
	{ 
		
		vec3 bump = texture2D(NormalMap, outCoord).xyz;
		vec3 n;
		if(HasNormalMap == 0) bump = vec3(0,0,1);
		else bump = (2.0*bump - 1.0);
		n = normalize(normalize(normal) * bump.z + normalize(binormal) * bump.y + normalize(tangent) * bump.x);

		vec3 l = normalize(lightDirection);
		vec3 v = normalize(cameraDirection);
		
		
		float diffuse = abs(dot(l, n));

		vec3 c = texture2D(DiffuseColorTexture, outCoord).xyz;

		float spec = pow(max(dot(normalize(l + v), n), 0.0), 5.0);

		
		float tb = abs(dot(tangent,binormal));
		gl_FragColor = vec4(c*(diffuse*0.6 + 0.4*spec), 1);//vec4((diffuse + 0.2) * c, 1.0); 
	}
	</script>

	<script src="spin.js" type="text/javascript"></script>
	<script src="Runtime.js" type="text/javascript"></script>
	<script src="Mouse.js" type="text/javascript"></script>
	<script src="Keyboard.js" type="text/javascript"></script>
	<script src="CameraController.js" type="text/javascript"></script>
	<script src="Math.js" type="text/javascript"></script>
	<script src="OpenGlContext.js" type="text/javascript"></script>
	<script src="OpenGlRenderView.js" type="text/javascript"></script>
	<script src="SceneGraph.js" type="text/javascript"></script>
	<script src="OBJImporter.js" type="text/javascript"></script>

	
	<script type="text/javascript">

//============================================================================
//							    Rendering
//============================================================================

var env = new Object();
var renderables = new Array();
var view = null;
var view2 = null;
var objects = new Array();
var args = new Array();

function getPhpArguments( name ) {
	name = name.replace(/[\[]/,"\\\[").replace(/[\]]/,"\\\]");
	var regexS = "[\\?\\&]"+name+"=([^&#]*)";
	var regex = new RegExp(regexS);
	var values = new Array();
	var match = null;
	var index = 0;
	var url = window.location.href;
	
	while ((match = regex.exec(url)) !== null) {
		if(match != undefined) {
			url = url.substr(match.index + match.length);
			values[index] = match[1].toString();
			index++;
		}
	}
	
	
	return values;

}

var sg = null;
var sg2 = null;

var duplLevel = 0;
function initHUD() {

	var helpDisplay = document.getElementById("help");

	view.ClearColor = new V4f(0,0,0,0);
	
	view.Keyboard.addListener({
	
		keyDown : function(event) {
		
			
			//H toggles the Help-Box
			if(event.keyCode == Keys.H) {
				if(helpDisplay.style.display == 'none') helpDisplay.style.display = 'block';
				else helpDisplay.style.display = 'none';
			}
			
			else if(view.Keyboard.Ctrl && event.keyCode == Keys.X) {
				sg = new Group([new TrafoApplicator(translationTrafo(new V3f(-Math.pow(2, duplLevel),0,0)), sg), sg, new TrafoApplicator(translationTrafo(new V3f(Math.pow(2, duplLevel),0,0)), sg)]);
				duplLevel++;
			}
			
		},
		
		keyUp : function(event) {}
	
	});

};

function initView() {

	var canvas = document.getElementById("webgl");
	//setup environment
	var viewTrafo = lookAtTrafo(new V3f(6,3,2), new V3f(0,0,0), new V3f(0,1,0));
	var projTrafo = perspectiveTrafo(60.0 / 57.296, 4/3, 0.1, 1000.0);
	view = new OpenGlRenderView(canvas, viewTrafo, projTrafo);

	
	args = getPhpArguments("obj");
	if(args.length == 0) args =["body.obj", "eyes.obj", "lowerTeeth.obj", "upperTeeth.obj"];
	var index = 0;
	
	renderables = new Array();
	var rootState = new TraversalState(view);
	var renderTraversal = new RenderTraversal(rootState);
	var bbTraversal = new GetBoundingBoxTraversal(rootState);
	

	view.Prepare = function() {

		//load all objects
		for(var i = 0; i < args.length; i++) {
			var path = args[i].toString();
			var objs = parseObj(path.toString());
			
			for(var j = 0; j < objs.length; j++) {
				var o = objs[j];
				objects[index] = o;
				index++;
			}
		}


		var surface = new SurfaceLeaf(document.getElementById("VS").text, document.getElementById("PS").text);
		
		var children = new Array();
		for(var i = 0; i < objects.length; i++) {
			var o = objects[i];
			children.push(new Leaf(o));
		}
		
		var sceneWithSurface = new SurfaceApplicator(surface, 
					new Group(children)
				);
					
		sg = sceneWithSurface;
		
		var box = bbTraversal.apply(sg);
		sg = new TrafoApplicator(box.scaleTo(3), sg);

	
		//sg = new Group([sg, sceneWithSurface]);
		document.getElementById("loading").style.display = 'none';
	};
	
	view.Resize = function() {
		//alert("resize");
		view.CameraController.ProjTrafo = perspectiveTrafo(60.0 / 57.296, view.Canvas.width / view.Canvas.height, 0.1, 1000.0); 
	};
	
	view.Render = function() {

		try {
			
			renderTraversal.apply(sg);

		} catch (e) {
			alert(e);
		};
	}
	
	
	initHUD();
	
	

};

function initView2() {

	var canvas2 = document.getElementById("webgl2");
	//setup environment
	var viewTrafo2 = lookAtTrafo(new V3f(6,3,2), new V3f(0,0,0), new V3f(0,1,0));
	var projTrafo2 = perspectiveTrafo(60.0 / 57.296, 4/3, 0.1, 1000.0);
	view2 = new OpenGlRenderView(canvas2, viewTrafo2, projTrafo2);

	
	var args2 = getPhpArguments("obj");
	if(args2.length == 0) args2 =["eyes.obj"];
	var index = 0;

	var rootState2 = new TraversalState(view2);
	var renderTraversal2 = new RenderTraversal(rootState2);
	var bbTraversal2 = new GetBoundingBoxTraversal(rootState2);
	
	var objects2 = new Array();
	view2.Prepare = function() {

		//load all objects
		for(var i = 0; i < args2.length; i++) {
			var path2 = args2[i].toString();
			var objs2 = parseObj(path2.toString());
			
			for(var j = 0; j < objs2.length; j++) {
				var o = objs2[j];
				objects2[index] = o;
				index++;
			}
		}


		var surface2 = new SurfaceLeaf(document.getElementById("VS").text, document.getElementById("PS").text);
		
		var children2 = new Array();
		for(var i = 0; i < objects2.length; i++) {
			var o = objects2[i];
			children2.push(new Leaf(o));
		}
		
		sg2 = new SurfaceApplicator(surface2, 
					new Group(children2)
				);
		
		var box2 = bbTraversal2.apply(sg2);
		sg2 = new TrafoApplicator(box2.scaleTo(3), sg2);

	};
	
	view2.Resize = function() {
		//alert("resize");
		view2.CameraController.ProjTrafo = perspectiveTrafo(60.0 / 57.296, view2.Canvas.width / view2.Canvas.height, 0.1, 1000.0); 
	};
	
	view2.Render = function() {

		try {
			
			renderTraversal2.apply(sg2);

		} catch (e) {
			alert(e);
		};
	}
	

};


function run() {
	view.run();	
}

function run2() {
	view2.run();
}

function startSpinner() {

	var opts = {
	  lines: 11, // The number of lines to draw
	  length: 15, // The length of each line
	  width: 6, // The line thickness
	  radius: 15, // The radius of the inner circle
	  corners: 0.7, // Corner roundness (0..1)
	  rotate: 0, // The rotation offset
	  direction: 1, // 1: clockwise, -1: counterclockwise
	  color: '#DDDDDD', // #rgb or #rrggbb
	  speed: 0.6, // Rounds per second
	  trail: 41, // Afterglow percentage
	  shadow: true, // Whether to render a shadow
	  hwaccel: false, // Whether to use hardware acceleration
	  className: 'spinner', // The CSS class to assign to the spinner
	  zIndex: 2e9, // The z-index (defaults to 2000000000)
	  top: 'auto', // Top position relative to parent in px
	  left: 'auto' // Left position relative to parent in px
	};
	var target = document.getElementById('running');
	var spinner = new Spinner(opts).spin(target);

}


setTimeout(run, 100);
setTimeout(run2, 100);
</script>
	
</head>
<body onload="startSpinner(); initView(); initView2();">

	<table border="0" width="100%">
		<tr>
			<td colspan="2" align="center">
				<h1>Some Header here</h1>
			</td>
		</tr>
		<tr>
			<td width="15%" valign="top">
				<h2>Menu</h2>
				<ul>
				  <li><h3>Entry1</h3></li>
				  <li><h3>Entry2</h3></li>
				  <li><h3>Entry3</h3></li>
				  <li><h3>Entry4</h3></li>
				  <li><h3>Entry5</h3></li>
				  <li><h3>Entry6</h3></li>
				</ul>
			</td>
			<td>

				<div id="renderer" style="width: 600px; height: 450px; float:left; border: 10px solid transparent;">
				
					<canvas id="webgl" oncontextmenu="return false;" style="width: 100%; height: 100%;">
						Your browser does not Support WebGL
					</canvas>

					<table id="loading">
						<tr>
							<td>
								<div id="running" />
							</td>
						</tr>
						<tr>
							<td>
								<br><br>loading
							</td>
						</tr>
					</table>

					<div id="help">
						<table>
							<tr>
								<td align="right">Left Mouse&nbsp;&nbsp;&nbsp;&nbsp;</td>
								<td>Pan/Tilt Camera</td>
							</tr>
							<tr>
								<td align="right">Middle Mouse&nbsp;&nbsp;&nbsp;&nbsp;</td>
								<td>Move Camera</td>
							</tr>
							<tr>
								<td align="right">Scroll&nbsp;&nbsp;&nbsp;&nbsp;</td>
								<td>Zoom</td>
							</tr>
							<tr>
								<td align="right">R&nbsp;&nbsp;&nbsp;&nbsp;</td>
								<td>Toggle automatic rotation</td>
							</tr>
							<tr>
								<td align="right">F&nbsp;&nbsp;&nbsp;&nbsp;</td>
								<td>Toggle FPS Overlay</td>
							</tr>
							<tr>
								<td align="right">Space&nbsp;&nbsp;&nbsp;&nbsp;</td>
								<td>Toggle Navigation mode</td>
							</tr>
							<tr>
								<td align="right">W/S/A/D&nbsp;&nbsp;&nbsp;&nbsp;</td>
								<td>Move in Explore Mode</td>
							</tr>
							<tr>
								<td align="right">Page Up/Down&nbsp;&nbsp;&nbsp;&nbsp;</td>
								<td>Increase/Decrease move speed</td>
							</tr>
							<tr>
								<td align="right">H&nbsp;&nbsp;&nbsp;&nbsp;</td>
								<td>Toggle this Help</td>
							</tr>
							
						</table>
					</div>

				</div>
				
				<div style="width: 600px; height: 450px; float:none; border: 10px solid transparent;">
				
					<canvas id="webgl2" oncontextmenu="return false;" style="width: 100%; height: 100%;">
						Your browser does not Support WebGL
					</canvas>

				</div>
				
				
				<h2>Lorem Ipsum</h2>Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.   

Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.   

Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi.   

Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat.   

Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis.   

At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat.   

Consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus.   

Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.   

Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.   

Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi.   

Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo
				
			</td>
		</tr>
	</table>
	
</body>
</html>