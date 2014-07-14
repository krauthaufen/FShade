<!DOCTYPE html>
<html>
<head>
	<meta charset="utf-8" />
	<title>Example</title>
	<style>
      body { 
        width: 100%;
        height: 100%;
        border: 0px;
        padding: 0px;
        margin: 0px;
      }
	  html { 
        width: 100%;
        height: 99%;
        border: 0px;
        padding: 0px;
        margin: 0px;
      }
	  #overlay {
		font-family:Verdana;
		font-size: 12px;
		position: absolute;
		top: 20px;
		left: 20px;
		z-index: 1;
		color: #aaaaaa;
		text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000;  
      }
	  #loading {
		position: absolute;
		left: 25%;
		top: 50%;
		
		font-family:Verdana;
		font-size: 15px;
		z-index: 1;
		color: #aaaaaa;
		text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000; 
		
	  }
	  #help {
		font-family:Verdana;
		font-size: 15px;
		position: absolute;
		bottom: 20px;
		right: 20px;
		z-index: 1;
		color: #aaaaaa;
		text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000;  
      }
	  #webgl {
        width: 100%;
        height: 100%;
		background: #000000;
      }
	  #editor {
        width: 100%;
        height: 100%;
		margin: 0px;
      }
	  .CodeMirror {
        width: 98%;
		height: 10%;
		margin: 0px;
      }
      .CodeMirror-scroll {
		height: 500px;
      }
	  .error-line {
		  text-decoration: underline;
		  -moz-text-decoration-style: dotted; 
		  -webkit-text-decoration-style: dotted; 
		  text-decoration-style: dotted; 
		
	  }
	  .sep {
		font-family: Helvetica;
		font-size: 20px;
		font-weight: bold;
		background: #EEEEEE;
		padding: 10px;
		spacing: 0px;
	  }
    </style>

	<link rel=stylesheet href="codemirror/doc/docs.css">
	<script src="codemirror/lib/codemirror.js"></script>
	<link rel="stylesheet" href="codemirror/lib/codemirror.css">
	<script src="codemirror/mode/mllike/mllike.js"></script>
	<script type='text/javascript' src='//code.jquery.com/jquery-1.9.1.js'></script>
    <link rel="stylesheet" type="text/css" href="https://jqwidgets.com/public/jqwidgets/styles/jqx.base.css">
	<link rel="stylesheet" type="text/css" href="https://jqwidgets.com/public/jqwidgets/styles/jqx.energyblue.css">
    <script type='text/javascript' src="https://jqwidgets.com/public/jqwidgets/jqx-all.js"></script>	
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
		normal = (ModelTrafo * vec4(Normals, 0)).xyz;
		binormal = (ModelTrafo * vec4(BiNormals, 0)).xyz;
		tangent = (ModelTrafo * vec4(Tangents, 0)).xyz;
		
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

		
		vec3 c;
		if(dot(outCoord, outCoord) > 0.01){
			c = texture2D(DiffuseColorTexture, outCoord).xyz;
		}
		else c = vec3(1,1,1);
		
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
	<script src="OpenGlRenderer.js" type="text/javascript"></script>
	<script src="OpenGlRenderView.js" type="text/javascript"></script>
	<script src="SceneGraph.js" type="text/javascript"></script>
	<script src="OBJImporter.js" type="text/javascript"></script>
	<script src="3DSImporter.js" type="text/javascript"></script>
	
	<script type="text/javascript">

//============================================================================
//							    Rendering
//============================================================================

var env = new Object();
var renderables = new Array();
var view = null;
var objects = new Array();
var textures = new Object();
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
var duplLevel = 0;
var surfaceApp = null;
var editor = null;

function initHUD() {

	var fpsDisplay = document.getElementById("fps");
	var navigationModeDisplay = document.getElementById("exploreMode");
	var helpDisplay = document.getElementById("help");
	
	fpsDisplay.style.display = 'none';
	
	view.OnFrameTimeUpdate.push(function() {
	
		//update the FPS if the overlay is visible
		if(fpsDisplay.style.display != 'none') {
			fpsDisplay.innerHTML = Math.round(100 * view.FPS) / 100 + " fps";
		}
	
	});
	
	view.Keyboard.addListener({
	
		keyDown : function(event) {
		
			//F toggles the FPS overlay
			if(event.keyCode == Keys.F) {
				if(fpsDisplay.style.display == 'none') fpsDisplay.style.display = 'block';
				else fpsDisplay.style.display = 'none';
			}
			
			//H toggles the Help-Box
			else if(event.keyCode == Keys.H) {
				if(helpDisplay.style.display == 'none') helpDisplay.style.display = 'block';
				else helpDisplay.style.display = 'none';
			}
			
			else if(view.Keyboard.Ctrl && event.keyCode == Keys.X) {
				sg = new Group([new TrafoApplicator(translationTrafo(new V3f(-Math.pow(2, duplLevel),0,0)), sg), sg, new TrafoApplicator(translationTrafo(new V3f(Math.pow(2, duplLevel),0,0)), sg)]);
				duplLevel++;
			}
		
			
			//Since we cannot know which button changes the navigation mode we simply update it on every key-press
			if(view.CameraController.explore) navigationModeDisplay.innerHTML = "Explore Mode";
			else navigationModeDisplay.innerHTML = "Orbit Mode";
			
		},
		
		keyUp : function(event) {}
	
	});

};

function initCodeMirror() {
  editor = CodeMirror.fromTextArea(document.getElementById('code'), {
    mode: "text/x-fsharp",
    lineNumbers: true,
	viewportMargin: Infinity,
	extraKeys: {
      "Ctrl-S": function (instance) {
		 updateGLSLCode();
         return false;
      },
	  "F5": function (instance) {
         updateGLSLCode();
         return false;
      }
	}
  });
  
  editor.setSize("99%", "80%");
}

function initView() {

	$("#mainView").jqxSplitter({
		width: '99%',
		height: '99%',
		theme: 'energyblue',
		panels: [{ size: '600px' }]
	});
	

	initCodeMirror();
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

		var children = new Array();
	
		//load all objects
		for(var i = 0; i < args.length; i++) {
			var path = args[i].toString();
			
			
			var objs = new Array();
			if(path.indexOf(".obj") >= 0) {
				objs = parseObj(path.toString());
				
				for(var j = 0; j < objs.length; j++) {
					var o = objs[j];
					children.push(new Leaf(o));
				}
			}
			else if(path.indexOf(".3ds") >= 0) {
				objs = parse3DS("../gltest/data/" + path.toString());
				for(var j = 0; j < objs.length; j++) {
					var o = objs[j];
					children.push(new TrafoApplicator(rotationTrafo(1.57, new V3f(-1,0,0)), new Leaf(o)));
				}
			}
			
			
		}
		
	
		var surface = new SurfaceLeaf(document.getElementById("VS").text, document.getElementById("PS").text);
		
		
		
		var sceneWithSurface = new SurfaceApplicator(surface, 
					new Group(children)
				);

		surfaceApp = sceneWithSurface;
			
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

		//try {
			
			renderTraversal.apply(sg);

		//} catch (e) {
		//	alert(e);
		//};
	}
	
	
	
	
	initHUD();
	
	

};


function run() {
	view.run();	
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


function compile(code, composition, action)
{
	var request = new XMLHttpRequest();
	request.onload = function() {
		//alert(request.responseText);
		if(request.status == 200)
		{
			action(JSON.parse(request.responseText));
		}
	};
	
	request.open("POST", "http://server.awx.at:8080/", true);
	//request.setRequestHeader("Content-type", "multipart/form-data");
	
	var formData = new FormData();
	formData.append("code", code);
	formData.append("comp", composition);
	
	request.send(formData);//{ code: code, comp : composition });
}


function updateGLSLCode()
{
	var code = editor.getDoc().getValue();
	var comp = document.getElementById("comp").value;

	compile(code, comp, function(reply) {
		
		//alert("new surface");
		
		if(reply.code == null) {
			alert("could not compile shader: " + reply.errors);
		}
		else
		{
			var code = reply.code;
			var vsCode = code.replace("#version 100", "#version 100\r\n#define Vertex\r\nprecision mediump float;").replace("VS(", "main(");
			var psCode = code.replace("#version 100", "#version 100\r\n#define Pixel\r\nprecision mediump float;").replace("PS(", "main(");

			surfaceApp.Surface = new SurfaceLeaf(vsCode, psCode);
		}
	});
	
}


setTimeout(run, 100);
</script>
	
</head>
<body width="100%" height="100%" onload="startSpinner(); initView();">

	<div id="mainView">
	
		<div id="editor">
			<p class="sep">Code:</p>
			<textarea id="code" name="code">
module Simple =
	open Aardvark.Base
	open FShade
	
	type V = { [<Semantic("Positions")>] p : V4d 
			   [<Semantic("Normals")>] n : V3d
               [<Semantic("Tangents")>] t : V3d
               [<Semantic("BiNormals")>] b : V3d
               [<Semantic("TexCoords")>] tc : V2d
               [<Semantic("Colors")>] color : V4d }

	let DiffuseColorTexture =
       	sampler2d {
           	texture uniform?DiffuseColorTexture
        }
                
	let NormalMap =
       	sampler2d {
           	texture uniform?NormalMap
        }

	let trafo(v : V) =
		vertex {
			let model : M44d = uniform?ModelTrafo
			let vp : M44d = uniform?ViewProjTrafo
			let world = model * v.p
			return { v with p = vp * world }
		}

	let normals(v : V) =
		fragment {
			return V4d(0.5 * (v.n.Normalized + V3d.III), 1.0)
		}
        
    let bump (v : V) =
    	fragment {
        	let s = 2.0 * NormalMap.Sample(v.tc).XYZ - V3d.III
            let n = s.X * v.t + s.Y * v.b + s.Z * v.n
        	return { v with n = n.Normalized }
        }
        
    let white (v : V) =
    	fragment {
            return V4d.IIII
        }
            
    let texture (v : V) =
       	fragment {
           	return DiffuseColorTexture.Sample(v.tc)
        }
			
			
	let light (v : V) =
		fragment {
        	let n = v.n.Normalized
			return  v.color.XYZ * (0.6 + 0.6 * Vec.dot n V3d.OIO)
		}	
			
			
			</textarea><br>
			<p class="sep">Composition:</p>
			<input type="text" size="80" id="comp" value="trafo texture bump light" />
			<input type="submit" value="Compile" onclick="updateGLSLCode()" />
		</div>
	
		<div id="renderer">
		
			<canvas id="webgl" oncontextmenu="return false;" >
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

			<div id="overlay">
				<table>
					<tr>
						<td id="exploreMode">Orbit Mode</td>
					</tr>
					<tr>
						<td id="fps"></td>
					</tr>
				</table>
			</div>

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
		
		
	</div>
</body>
</html>