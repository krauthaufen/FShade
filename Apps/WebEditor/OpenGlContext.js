fromGlType = function(context, glType) {

	switch(glType) {
		case context.GL.FLOAT: return Type.Float;
		case context.GL.INT: return Type.Int;
		case context.GL.BOOL: return Type.Int;
		case context.GL.FLOAT_VEC2: return Type.Float2;
		case context.GL.FLOAT_VEC3: return Type.Float3;
		case context.GL.FLOAT_VEC4: return Type.Float4;
		case context.GL.INT_VEC2: return Type.Int2;
		case context.GL.INT_VEC3: return Type.Int3;
		case context.GL.INT_VEC4: return Type.Int4;
		case context.GL.FLOAT_MAT2: return Type.Float2x2;
		case context.GL.FLOAT_MAT3: return Type.Float3x3;
		case context.GL.FLOAT_MAT4: return Type.Float4x4;
		case context.GL.SAMPLER_2D: return Type.Texture2D;
		default: throw "unknown type";
	}
};

PrimitiveTopology = {
	Points : 1,
	Lines : 2,
	Triangles : 3,
	TriangleStrip : 4,
}


function OpenGlContext(gl) {
	this.AnisotropicFiltering = false;
	this.Ext = null;
	this.GL = gl;
	
	this.Textures = new Object();
	
	this.Init();
}


function OpenGlBuffer(handle, elementType, length, kind) {
	this.Handle = handle;
	this.ElementType = elementType;
	this.Length = length;
	this.Kind = kind;
	this.SizeInBytes = getTypeSize(elementType) * length;
};

function OpenGlAttribute(context, index, handle) {
	this.Index = index;
	this.Handle = handle;
	this.Name = handle.name;
	this.Size = handle.size;
	this.Type = fromGlType(context, handle.type);
};

function OpenGlUniform(context, location, handle) {
	this.Location = location;
	this.Handle = handle;
	this.Name = handle.name;
	this.Size = handle.size;
	this.Type = fromGlType(context, handle.type);
};

function OpenGlProgram(handle, attributes, uniforms) {
	this.Handle = handle;
	this.Attributes = attributes;
	this.Uniforms = uniforms;
};

function OpenGlTexture(handle, width, height) {
	this.Width = width;
	this.Height = height;
	this.Handle = handle;
	this.Type = Type.Texture2D;
};


checkGlError = function(message) {
	/*var err = this.GL.getError()
	if(err != this.GL.NO_ERROR) {
		throw "OpenGl-Error: " + message + ": " + err;
	}*/
};

OpenGlContext.prototype.Init = function() {

	this.Extensions = this.GL.getSupportedExtensions();
	
	/*if(contains(this.Extensions,"MOZ_EXT_texture_filter_anisotropic")) {
		this.Ext = this.GL.getExtension("MOZ_EXT_texture_filter_anisotropic");
		this.AnisotropicFiltering = true;
	}
	else if(contains(this.Extensions,"WEBKIT_EXT_texture_filter_anisotropic")) {
		this.Ext = this.GL.getExtension("WEBKIT_EXT_texture_filter_anisotropic");
		this.AnisotropicFiltering = true;
	}
	else {*/
		this.AnisotropicFiltering = false;
		this.Ext = undefined;
	//}
	
	
	this.GL.pixelStorei(this.GL.UNPACK_FLIP_Y_WEBGL, true);
			checkGlError("could not flip texture");
};

OpenGlContext.prototype.CreateBuffer = function(target, array) {
	var buffer = this.GL.createBuffer();
	checkGlError("could not create buffer");
	
	this.GL.bindBuffer(target, buffer);
	checkGlError("could not bind buffer");
	
	this.GL.bufferData(target, toFloatArray(array), this.GL.STATIC_DRAW);
	checkGlError("could not upload buffer data");
	
	this.GL.bindBuffer(target, null);
	checkGlError("could not unbind buffer");
	
	return new OpenGlBuffer(buffer, getElementType(array), array.length, target);
};

OpenGlContext.prototype.CreateShader = function(sourceCode, shaderType) {


	var s = this.GL.createShader(shaderType);
	this.GL.getError(); //error sink for apple
	//checkGlError("could not create shader " + shaderType);
	
	this.GL.shaderSource(s, sourceCode);
	checkGlError("could not set shader source");
	
	this.GL.compileShader(s);
	checkGlError("could not compile shader");
	
	if (!this.GL.getShaderParameter(s, this.GL.COMPILE_STATUS)) {
		var log = this.GL.getShaderInfoLog(s);
		alert(log);
		throw "Could not compile shader:\n\n" + sourceCode + "\r\n" + log;
		
	}
	
	return s;
};

OpenGlContext.prototype.CreateProgram = function(vertexShader, pixelShader) {
	var prog = this.GL.createProgram();
	checkGlError("could not create program");
	
	if(vertexShader != null) {
		this.GL.attachShader(prog, vertexShader);
		checkGlError("could not attach vertexShader to program");
	}
	
	if(pixelShader != null) {
		this.GL.attachShader(prog, pixelShader);
		checkGlError("could not attach pixelShader to program");
	}
	
	this.GL.linkProgram(prog);
	checkGlError("could not link the program");
	if (!this.GL.getProgramParameter(prog, this.GL.LINK_STATUS)) {
		throw "Could not link the shader program:\n\n" + getProgramInfoLog(prog);
	}
	
	var attributes = new Array();
	for(i = 0;;i++) {
		var attrib = this.GL.getActiveAttrib(prog, i);
		var err = this.GL.getError();
		if(attrib == null || err != this.GL.NO_ERROR)break;	
	
		var glAtt = new OpenGlAttribute(this, i, attrib);
		
		//alert(glAtt.Name + ": " + glAtt.Type);
		attributes[i] = glAtt;
	}
	
	var uniforms = new Array();
	for(i = 0;;i++) {
		var u = this.GL.getActiveUniform(prog, i);
		var err = this.GL.getError();
		if(u == null || err != this.GL.NO_ERROR)break;
		
		var location = this.GL.getUniformLocation(prog, u.name);
		uniforms[i] = new OpenGlUniform(this, location, u);
		//alert(uniforms[i].Location + ": " + uniforms[i].Type);
	}
	
	
	
	return new OpenGlProgram(prog, attributes, uniforms);
};




var glAni = null;

function toPowerOfTwo(image) {
    if (!isPowerOfTwo(image.width) || !isPowerOfTwo(image.height)) {
        // Scale up the texture to the next highest power of two dimensions.
        var canvas = document.createElement("canvas");
        canvas.width = nextHighestPowerOfTwo(image.width);
        canvas.height = nextHighestPowerOfTwo(image.height);
        var ctx = canvas.getContext("2d");
        ctx.drawImage(image, 0, 0, canvas.width, canvas.height);
        image = canvas;
    }
    return image;
}
 
function isPowerOfTwo(x) {
    return (x & (x - 1)) == 0;
}
 
function nextHighestPowerOfTwo(x) {
    --x;
    for (var i = 1; i < 32; i <<= 1) {
        x = x | x >> i;
    }
    return x + 1;
}

function contains(a, obj) {
    for (var i = 0; i < a.length; i++) {
        if (a[i] === obj) {
            return true;
        }
    }
    return false;
}

OpenGlContext.prototype.CreateTextureFromFile = function(path) {
	

	//var me = this;
	var newTexture = this.GL.createTexture();
	checkGlError("could not create texture");
	
    var image = new Image();
	image.Me = this;
    image.onload = function() {
		try {
			var me = image.Me;
			image = toPowerOfTwo(image);
			//alert(image.width + "x" + image.height);
			
			
			me.GL.bindTexture(me.GL.TEXTURE_2D, newTexture);
			checkGlError("could not bind texture");
			
			
			
			me.GL.texImage2D(me.GL.TEXTURE_2D, 0, me.GL.RGBA, me.GL.RGBA, me.GL.UNSIGNED_BYTE, image);
			checkGlError("could not upload texture");
			
			me.GL.generateMipmap(me.GL.TEXTURE_2D);
			checkGlError("could not generate mipmaps");
			
			
			me.GL.texParameteri(me.GL.TEXTURE_2D, me.GL.TEXTURE_MAG_FILTER, me.GL.LINEAR);
			checkGlError("could not set texture filter");
			
			me.GL.texParameteri(me.GL.TEXTURE_2D, me.GL.TEXTURE_MIN_FILTER, me.GL.LINEAR_MIPMAP_LINEAR);
			checkGlError("could not set texture filter");
			
			//alert(image.width + "x" + image.height);
			if(me.AnisotropicFiltering) {
				me.GL.texParameterf(me.GL.TEXTURE_2D, me.Ext.TEXTURE_MAX_ANISOTROPY_EXT, 16);
				checkGlError("could not set max anisotropy");
			}
			
			
			me.GL.texParameteri(me.GL.TEXTURE_2D, me.GL.TEXTURE_WRAP_S, me.GL.REPEAT);
			checkGlError("could not set texture repeat");
			me.GL.texParameteri(me.GL.TEXTURE_2D, me.GL.TEXTURE_WRAP_T, me.GL.REPEAT);
			checkGlError("could not set texture repeat");
			
			
			
			me.GL.bindTexture(me.GL.TEXTURE_2D, null);
			checkGlError("could not unbind texture");
			
			me.GL.flush();
			me.GL.finish();
		}
		catch(e){
			alert(e);
		}
	};
	image.src = "../gltest/data/" + path;

	//alert(image.width);
	return new OpenGlTexture(newTexture, image.width, image.height);
};

function OpenGlUniformValue(name, location, getter) {
	this.Name = name;
	this.Location = location;
	this.Getter = getter;
};

function OpenGlVertexAttribute(location, dimension, buffer) {
	this.Location = location;
	this.Buffer = buffer;
	this.Dimension = dimension;
};

function Renderable(gl, vg, attributes, elementBuffer, count, uniforms, program) {
	this.GL = gl;
	this.Geometry = vg;
	this.Attributes = attributes;
	this.ElementBuffer = elementBuffer;
	this.ElementCount = count;
	this.Uniforms = uniforms;
	this.Program = program;
}

function isInt(n) {
   return n % 1 === 0;
}


var textureUnits = null;
Renderable.prototype.render = function() {

	if(textureUnits == null) {
		textureUnits = new Array();
		textureUnits[0] = this.GL.TEXTURE0;
		textureUnits[1] = this.GL.TEXTURE1;

	}

	this.GL.useProgram(this.Program.Handle);
	checkGlError("could not use program");
	
	for(var i = 0; i < this.Attributes.length; i++) {
		if(this.Attributes[i].Buffer.Handle != null) {
			this.GL.bindBuffer(this.GL.ARRAY_BUFFER, this.Attributes[i].Buffer.Handle);
			checkGlError("could not bind buffer");
			
			var attr = this.Attributes[i].Location;
			this.GL.enableVertexAttribArray(attr);
			checkGlError("could not enable vertex attrib " + attr);
			this.GL.vertexAttribPointer(attr, this.Attributes[i].Dimension, this.GL.FLOAT, false, 0, 0);
			checkGlError("could not set vertex attrib pointer");
		}
		else {
			//alert("disabled: " + this.Attributes[i]);
			var attr = this.Attributes[i].Location;
			this.GL.disableVertexAttribArray(attr);
			checkGlError("could not enable vertex attrib " + attr);
		}
	}
	var textureUnit = 0;
	
	for(var u = 0; u < this.Uniforms.length; u++) {

		var value = this.Uniforms[u].Getter(this.Uniforms[u].Name);
		
		if(value instanceof Trafo3d) {
			this.GL.uniformMatrix4fv(this.Uniforms[u].Location, false, value.Forward.toArray());
			checkGlError("could not set matrix uniform");
		}
		else if(value != undefined && value.Type == Type.Float3) {
			this.GL.uniform3f(this.Uniforms[u].Location, value.X, value.Y, value.Z);
			checkGlError("could not set matrix uniform");
		}
		else if(value != undefined && value.Type == Type.Texture2D) {
		
			var unit = textureUnits[textureUnit];
		
			this.GL.uniform1i(this.Uniforms[u].Location, textureUnit);
			checkGlError("could not set texture uniform");
		
			this.GL.activeTexture(unit);
			checkGlError("could not set set active texture");
			
			this.GL.bindTexture(this.GL.TEXTURE_2D, value.Handle);
			checkGlError("could not bind texture");
			//alert(this.GL);
			
			
			//alert(this.Uniforms[u].Name + "["+ textureUnit + "]" + " = " + value.Handle);
			
			
			textureUnit = textureUnit + 1;
		}
		else if(value != undefined && isInt(value)) {
			//alert(this.Uniforms[u].Name);
			this.GL.uniform1i(this.Uniforms[u].Location, value);
			checkGlError("could not set matrix uniform");
		}
		else {
			alert(this.Uniforms[u].Name);
		}
		/*else if(isInt(value) && value >= 0) {
			//alert(this.Uniforms[u].Name + ": " + value);
			this.GL.uniform1i(this.Uniforms[u].Location, value);
			checkGlError("could not set matrix uniform");
		}*/
		//else alert(this.Uniforms[u].Name + " = " + value);
	
	}
	
	
	if(this.ElementBuffer != null) {
		//alert("sdaads");
		this.GL.bindBuffer(this.GL.ELEMENT_ARRAY_BUFFER, this.ElementBuffer.Handle);
		this.GL.drawElements(this.GL.TRIANGLES, this.ElementCount, this.GL.UNSIGNED_SHORT, 0);
		this.GL.bindBuffer(this.GL.ELEMENT_ARRAY_BUFFER, null);
	}
	else {
		this.GL.drawArrays(this.GL.TRIANGLES, 0, this.ElementCount);
		checkGlError("could not draw arrays");
	}
	
	
};



OpenGlContext.prototype.createRenderable = function(env, leaf, vg, program) {

	var buffers = new Array();
	var uniforms = new Array();
	
	for(var i = 0; i < program.Attributes.length; i++) {
		var att = program.Attributes[i];

		if(vg[att.Name] != null && vg[att.Name] != undefined && vg[att.Name].length > 0) {
			
			
			var arr = vg[att.Name];
			//if(arr != null && arr != undefined) {
				var buffer = this.CreateBuffer(this.GL.ARRAY_BUFFER, arr);

				var dim = getTypeSize(buffer.ElementType) / 4;
				
				buffers[i] = new OpenGlVertexAttribute(att.Index, dim, buffer);
			//}
			//else buffers[i] = new OpenGlVertexAttribute(att.Index, 1, new OpenGlBuffer(null, Type.Float, 0, this.GL.ARRAY_BUFFER));
		}
		else buffers[i] = new OpenGlVertexAttribute(att.Index, 1, new OpenGlBuffer(null, Type.Float, 0, this.GL.ARRAY_BUFFER));
	}

	for(var i = 0; i < program.Uniforms.length; i++) {
		var u = program.Uniforms[i];
		
		var envValue = env[u.Name];
		var envType = typeof envValue;
		
		var objValue = leaf[u.Name];
		var objType = typeof objValue;
		
		if(envType != 'undefined') {
	
			if(envType == 'function') {
				uniforms[i] = new OpenGlUniformValue(u.Name, u.Location, function(n) { return env[n](); });
			}
			else {
				uniforms[i] = new OpenGlUniformValue(u.Name, u.Location, function(n) { return env[n]; }); 
			}
		}
		else if(objType != 'undefined') {
			if(objType == 'function') {
				uniforms[i] = new OpenGlUniformValue(u.Name, u.Location, function(n) { return leaf[n](); });
			}
			else {
				uniforms[i] = new OpenGlUniformValue(u.Name, u.Location, function(n) { return leaf[n]; }); 
			}
		}
		else {
			uniforms[i] = new OpenGlUniformValue(u.Name, u.Location, function() { return -1; }); 
		}
		//else alert("dead: " + u.Name);//uniforms[i] = new OpenGlUniformValue(u.Name, u.Location, function() { return -1; }); 
	}
	
	var elements = vg.Positions.length;
	var indexBuffer = null;
	if(vg.Indices != null) {
		indexBuffer = this.CreateBuffer(this.GL.ELEMENT_ARRAY_BUFFER, vg.Indices);
		elements = vg.Indices.length;
	}
	
	
	var result = new Renderable(this.GL, vg, buffers, indexBuffer, elements, uniforms, program);
	
	return result;

};

