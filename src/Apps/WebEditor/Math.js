function V2f(x,y) {
	this.X = x;
	this.Y = y;
	this.Type = Type.Float2;
};


V2f.prototype.plus = function(other) {
	return new V2f(this.X + other.X, this.Y + other.Y);
};

V2f.prototype.minus = function(other) {
	return new V2f(this.X - other.X, this.Y - other.Y);
};

V2f.prototype.toString = function() {
    return "[" + this.X + ", " + this.Y + "]";
};


function V3f(x,y,z) {
	this.X = x;
	this.Y = y;
	this.Z = z;
	this.Type = Type.Float3;
};

V3f.prototype.dot = function(other) {
	return this.X*other.X + this.Y * other.Y + this.Z*other.Z;
};

V3f.prototype.length = function() {
	return Math.sqrt(this.dot(this));
};

V3f.prototype.normalized = function() {
	var l = this.length();
	return new V3f(this.X / l, this.Y / l, this.Z / l);
};

V3f.prototype.toString = function() {
    return "[" + this.X + ", " + this.Y + ", " + this.Z + "]";
};

V3f.prototype.cross = function(other) {

	return new V3f(this.Y * other.Z - this.Z * other.Y,
				   this.Z * other.X - this.X * other.Z,
				   this.X * other.Y - this.Y * other.X);

};

V3f.prototype.plus = function(other) {
	return new V3f(this.X + other.X, this.Y + other.Y, this.Z + other.Z);
};

V3f.prototype.minus = function(other) {
	return new V3f(this.X - other.X, this.Y - other.Y, this.Z - other.Z);
};

V3f.prototype.mul = function(other) {
	return new V3f(this.X * other, this.Y * other, this.Z * other);
};

function V4f(x,y,z,w) {
	this.X = x;
	this.Y = y;
	this.Z = z;
	this.W = w;
	this.Type = Type.Float4;
};


function V2i(x,y) {
	this.X = x;
	this.Y = y;
	this.Type = Type.Int2;
};

function V3i(x,y,z) {
	this.X = x;
	this.Y = y;
	this.Z = z;
	this.Type = Type.Int3;
};

function V4i(x,y,z,w) {
	this.X = x;
	this.Y = y;
	this.Z = z;
	this.W = w;
	this.Type = Type.Int4;
};

function C3b(r,g,b) {
	this.R = r;
	this.G = g;
	this.B = b;
	this.Type = Type.Byte3;
};

function C4b(r,g,b,a) {
	this.R = r;
	this.G = g;
	this.B = b;
	this.A = a;
	this.Type = Type.Byte4;
};

function M33f() {
	this.M00 = 1;
	this.M01 = 0;
	this.M02 = 0;
	this.M10 = 0;
	this.M11 = 1;
	this.M12 = 0;
	this.M20 = 0;
	this.M21 = 0;
	this.M22 = 1;
	this.Type = Type.Float3x3;
};

function M44f() {
	this.M00 = 1;
	this.M01 = 0;
	this.M02 = 0;
	this.M03 = 0;
	this.M10 = 0;
	this.M11 = 1;
	this.M12 = 0;
	this.M13 = 0;
	this.M20 = 0;
	this.M21 = 0;
	this.M22 = 1;
	this.M23 = 0;
	this.M30 = 0;
	this.M31 = 0;
	this.M32 = 0;
	this.M33 = 1;
	this.Type = Type.Float4x4;
};


function Box3f(min, max) {
	this.Min = min;
	this.Max = max;
}

Box3f.prototype.union = function(other) {
	var max = new V3f(this.Max.X, this.Max.Y, this.Max.Z);
	var min = new V3f(this.Min.X, this.Min.Y, this.Min.Z);
	
	if(other.Min.X < min.X)min.X = other.Min.X;
	if(other.Min.Y < min.Y)min.Y = other.Min.Y;
	if(other.Min.Z < min.Z)min.Z = other.Min.Z;
	
	if(other.Max.X > max.X)max.X = other.Max.X;
	if(other.Max.Y > max.Y)max.Y = other.Max.Y;
	if(other.Max.Z > max.Z)max.Z = other.Max.Z;
	
	return new Box3f(min, max);
};

Box3f.prototype.scaleTo = function(maxSize) {

	var sx = this.Max.X - this.Min.X;
	var sy = this.Max.Y - this.Min.Y;
	var sz = this.Max.Z - this.Min.Z;

	var s = sx;
	if(sy > s)s = sy;
	if(sz > s)s = sz;
	
	var avgx = (this.Max.X + this.Min.X) * 0.5;
	var avgy = (this.Max.Y + this.Min.Y) * 0.5;
	var avgz = (this.Max.Z + this.Min.Z) * 0.5;
	
	var trafo = translationTrafo(new V3f(-avgx, -avgy, -avgz)).mul(scaleTrafo(maxSize / s));

	//alert(trafo.Forward);
	
	return trafo;
};



function Trafo3d(forward, backward) {
	this.Forward = forward;
	this.Backward = backward;
}

M44f.prototype.toArray = function() {
	return new Float32Array([this.M00, this.M10, this.M20, this.M30, this.M01, this.M11, this.M21, this.M31, this.M02, this.M12, this.M22, this.M32, this.M03, this.M13, this.M23, this.M33]);
};

M44f.prototype.toString = function() {
	return "[[" + this.M00+ ", " +this.M01+ ", " +this.M02+ ", " +this.M03+ "], [" +this.M10+ ", " +this.M11+ ", " +this.M12+ ", " +this.M13+ "], [" +this.M20+ ", " +this.M21+ ", " +this.M22+ ", " +this.M23+ "], [" +this.M30+ ", " +this.M31+ ", " +this.M32+ ", " +this.M33 + "]]";
};

M44f.prototype.mul = function(other) {

	var mat = new M44f();
	
	
	mat.M00 = this.M00 * other.M00 + this.M01 * other.M10 + this.M02 * other.M20 + this.M03 * other.M30;
	mat.M01 = this.M00 * other.M01 + this.M01 * other.M11 + this.M02 * other.M21 + this.M03 * other.M31; 
	mat.M02 = this.M00 * other.M02 + this.M01 * other.M12 + this.M02 * other.M22 + this.M03 * other.M32; 
	mat.M03 = this.M00 * other.M03 + this.M01 * other.M13 + this.M02 * other.M23 + this.M03 * other.M33; 
	mat.M10 = this.M10 * other.M00 + this.M11 * other.M10 + this.M12 * other.M20 + this.M13 * other.M30; 
	mat.M11 = this.M10 * other.M01 + this.M11 * other.M11 + this.M12 * other.M21 + this.M13 * other.M31; 
	mat.M12 = this.M10 * other.M02 + this.M11 * other.M12 + this.M12 * other.M22 + this.M13 * other.M32; 
	mat.M13 = this.M10 * other.M03 + this.M11 * other.M13 + this.M12 * other.M23 + this.M13 * other.M33; 
	mat.M20 = this.M20 * other.M00 + this.M21 * other.M10 + this.M22 * other.M20 + this.M23 * other.M30; 
	mat.M21 = this.M20 * other.M01 + this.M21 * other.M11 + this.M22 * other.M21 + this.M23 * other.M31; 
	mat.M22 = this.M20 * other.M02 + this.M21 * other.M12 + this.M22 * other.M22 + this.M23 * other.M32; 
	mat.M23 = this.M20 * other.M03 + this.M21 * other.M13 + this.M22 * other.M23 + this.M23 * other.M33; 
	mat.M30 = this.M30 * other.M00 + this.M31 * other.M10 + this.M32 * other.M20 + this.M33 * other.M30; 
	mat.M31 = this.M30 * other.M01 + this.M31 * other.M11 + this.M32 * other.M21 + this.M33 * other.M31; 
	mat.M32 = this.M30 * other.M02 + this.M31 * other.M12 + this.M32 * other.M22 + this.M33 * other.M32; 
	mat.M33 = this.M30 * other.M03 + this.M31 * other.M13 + this.M32 * other.M23 + this.M33 * other.M33;

	
	
	return mat;
};

Trafo3d.prototype.mul = function(other) {

	return new Trafo3d(other.Forward.mul(this.Forward), this.Backward.mul(other.Backward));
}

M44f.prototype.transformDir = function(other) {
	return new V3f( this.M00 * other.X + this.M01 * other.Y + this.M02 * other.Z,
					this.M10 * other.X + this.M11 * other.Y + this.M12 * other.Z,
					this.M20 * other.X + this.M21 * other.Y + this.M22 * other.Z);
};

M44f.prototype.transformPos = function(other) {
	return new V3f( this.M00 * other.X + this.M01 * other.Y + this.M02 * other.Z + this.M03,
					this.M10 * other.X + this.M11 * other.Y + this.M12 * other.Z + this.M13,
					this.M20 * other.X + this.M21 * other.Y + this.M22 * other.Z + this.M23);
};




rotation = function(angle, axis) {
	var halfAngle = angle * 0.5;
	var w = Math.cos(halfAngle);
	var s = Math.sin(halfAngle);
	var n  = axis.normalized();
	n = new V3f(n.X * s, n.Y*s, n.Z * s);
	
	var xx = n.X * n.X;
	var yy = n.Y * n.Y;
	var zz = n.Z * n.Z;
	var xy = n.X * n.Y;
	var xz = n.X * n.Z;
	var yz = n.Y * n.Z;
	var xw = n.X * w;
	var yw = n.Y * w;
	var zw = n.Z * w;
	var mat = new M44f();
	
	mat.M00 = 1 - 2 * (yy + zz);
	mat.M01 = 2 * (xy - zw);
	mat.M02 = 2 * (xz + yw);
	mat.M03 = 0;

	mat.M10 = 2 * (xy + zw);
	mat.M11 = 1 - 2 * (zz + xx);
	mat.M12 = 2 * (yz - xw);
	mat.M13 = 0;

	mat.M20 = 2 * (xz - yw);
	mat.M21 = 2 * (yz + xw);
	mat.M22 = 1 - 2 * (yy + xx);
	mat.M23 = 0;

	mat.M30 = 0;
	mat.M31 = 0;
	mat.M32 = 0;
	mat.M33 = 1;
	
	return mat;
};

lookAt = function(eye, center, up) {
	var zaxis = eye.minus(center).normalized();
	var xaxis = zaxis.cross(up).normalized();
	var yaxis = xaxis.cross(zaxis);

	
	
	var mat = new M44f();
	mat.M00 = xaxis.X;
	mat.M01 = xaxis.Y;
	mat.M02 = xaxis.Z;
	mat.M03 = -xaxis.dot(eye);
	
	mat.M10 = yaxis.X;
	mat.M11 = yaxis.Y;
	mat.M12 = yaxis.Z;
	mat.M13 = -yaxis.dot(eye);
	
	mat.M20 = zaxis.X;
	mat.M21 = zaxis.Y;
	mat.M22 = zaxis.Z;
	mat.M23 = -zaxis.dot(eye);
	
	mat.M30 = 0;
	mat.M31 = 0;
	mat.M32 = 0;
	mat.M33 = 1;
	
	return mat;
};

frustum = function(l,r,b,t,n,f) {

	var mat = new M44f();
	
	/*
	new M44d(
                    (2 * n) / (r - l),                     0,     (r + l) / (r - l),                     0,
                                    0,     (2 * n) / (t - b),     (t + b) / (t - b),                     0,
                                    0,                     0,           f / (n - f),     (f * n) / (n - f),
                                    0,                     0,                    -1,                     0
                    ),   
	*/
	
    mat.M00 = 2 * n / (r - l);
    mat.M01 = 0;
	mat.M02 = (r + l) / (r - l);
	mat.M03 = 0;
	
	mat.M10 = 0;
    mat.M11 = (2 * n) / (t - b);
    mat.M12 = (t + b) / (t - b);
    mat.M13 = 0;
	
	mat.M20 = 0;
	mat.M21 = 0;
	mat.M22 = f / (n - f);
    mat.M23 = (f * n) / (n - f);
    
	mat.M30 = 0;
	mat.M31 = 0;
	mat.M32 = -1;
    mat.M33 = 0;
	
	return mat;
};

perspective = function(horizontalFieldOfView, aspect, near, far) {

	var wh = Math.tan(horizontalFieldOfView*0.5)*near;
	var hh = wh / aspect;
	
	return frustum(-wh, wh, -hh, hh, near, far);
	
};

translation = function(pos) {
	var mat = new M44f();
	mat.M00 = 1;
	mat.M01 = 0;
	mat.M02 = 0;
	mat.M03 = pos.X
	
	mat.M10 = 0;
	mat.M11 = 1;
	mat.M12 = 0;
	mat.M13 = pos.Y;
	
	mat.M20 = 0;
	mat.M21 = 0;
	mat.M22 = 1;
	mat.M23 = pos.Z;
	
	mat.M30 = 0;
	mat.M31 = 0;
	mat.M32 = 0;
	mat.M33 = 1;
	
	return mat;
};


rotationTrafo = function(angle, axis) {

	return new Trafo3d(rotation(angle, axis), rotation(-angle, axis));
};

lookAtTrafo = function(eye, center, up) {
	var zaxis = eye.minus(center).normalized();
	var xaxis = up.cross(zaxis).normalized();
	var yaxis = zaxis.cross(xaxis);

	var forward = new M44f();
	forward.M00 = xaxis.X;
	forward.M01 = xaxis.Y;
	forward.M02 = xaxis.Z;
	forward.M03 = -xaxis.dot(eye);
	
	forward.M10 = yaxis.X;
	forward.M11 = yaxis.Y;
	forward.M12 = yaxis.Z;
	forward.M13 = -yaxis.dot(eye);
	
	forward.M20 = zaxis.X;
	forward.M21 = zaxis.Y;
	forward.M22 = zaxis.Z;
	forward.M23 = -zaxis.dot(eye);
	
	forward.M30 = 0;
	forward.M31 = 0;
	forward.M32 = 0;
	forward.M33 = 1;
	
	
	var backward = new M44f();
	backward.M00 = xaxis.X;
	backward.M01 = yaxis.X;
	backward.M02 = zaxis.X;
	backward.M03 = eye.X;
	
	backward.M10 = xaxis.Y;
	backward.M11 = yaxis.Y;
	backward.M12 = zaxis.Y;
	backward.M13 = eye.Y;
	
	backward.M20 = xaxis.Z;
	backward.M21 = yaxis.Z;
	backward.M22 = zaxis.Z;
	backward.M23 = eye.Z;
	
	backward.M30 = 0;
	backward.M31 = 0;
	backward.M32 = 0;
	backward.M33 = 1;
	
	
	return new Trafo3d(forward, backward);
};

frustumTrafo = function(l,r,b,t,n,f) {
	var forward = new M44f();
	var backward = new M44f();
	
	/*
	m_trafo = new Trafo3d(
                new M44d(
                    (2 * n) / (r - l),                     0,     (r + l) / (r - l),                     0,
                                    0,     (2 * n) / (t - b),     (t + b) / (t - b),                     0,
                                    0,                     0,           f / (n - f),     (f * n) / (n - f),
                                    0,                     0,                    -1,                     0
                    ),                                                     
                                                                      
	*/
	//alert(l + ", " + r + ", " + b + ", " + t + ", " + n + ", " + f);
    forward.M00 = (2 * n) / (r - l); 	forward.M01 = 0.0; 					forward.M02 = (r + l) / (r - l); 	forward.M03 = 0.0;
	forward.M10 = 0.0; 					forward.M11 = (2 * n) / (t - b);	forward.M12 = (t + b) / (t - b);	forward.M13 = 0.0;
	forward.M20 = 0.0;					forward.M21 = 0.0;					forward.M22 = f / (n - f);			forward.M23 = (f * n) / (n - f);
    forward.M30 = 0.0;					forward.M31 = 0.0;					forward.M32 = -1.0;					forward.M33 = 0.0;
	
	//alert(forward);
	
	
	
	/*new M44d(                                      
                    (r - l) / (2 * n),                     0,                     0,     (r + l) / (2 * n),
                                    0,     (t - b) / (2 * n),                     0,     (t + b) / (2 * n),
                                    0,                     0,                     0,                    -1,
                                    0,                     0,     (n - f) / (f * n),                 1 / n
                    )
                );*/
	
	
    backward.M00 = (r - l) / (2 * n);
    backward.M01 = 0;
	backward.M02 = 0;
	backward.M03 = (r + l) / (2 * n);
	
	backward.M10 = 0;
    backward.M11 = (t - b) / (2 * n);
    backward.M12 = 0;
    backward.M13 = (t + b) / (2 * n);
	
	backward.M20 = 0;
	backward.M21 = 0;
	backward.M22 = 0;
    backward.M23 = -1;
    
	backward.M30 = 0;
	backward.M31 = 0;
	backward.M32 = (n - f) / (f * n);
    backward.M33 = 1 / n;
	
	
	//alert(forward);
	
	return new Trafo3d(forward, backward);
	
};

orthoTrafo = function(l,r,b,t,n,f) {

    var fw = new M44f();
	var bw = new M44f();
	
	fw.M00 = 2 / (r - l); 	fw.M01 = 0; 			fw.M02 = 0; 			fw.M03 = (l + r) / (l - r);
	fw.M10 = 0; 			fw.M11 = 2 / (t - b); 	fw.M12 = 0; 			fw.M13 = (b + t) / (b - t);
	fw.M20 = 0;				fw.M21 = 0; 			fw.M22 = 1 / (n - f);   fw.M23 = n / (n - f);
	fw.M30 = 0;				fw.M31 = 0;             fw.M32 = 0;             fw.M33 = 1;
	
	
	bw.M00 = (r - l) / 2; 	bw.M01 = 0; 			bw.M02 = 0; 			bw.M03 = (l + r) / 2;
	bw.M10 = 0; 			bw.M11 = (t - b) / 2; 	bw.M12 = 0; 			bw.M13 = (b + t) / 2;
	bw.M20 = 0;				bw.M21 = 0; 			bw.M22 = n - f;   		bw.M23 = -n;
	bw.M30 = 0;				bw.M31 = 0;             bw.M32 = 0;             bw.M33 = 1;
	

	return new Trafo3d(fw, bw);

}


scaleTrafo = function(s) {

    var fw = new M44f();
	var bw = new M44f();
	
	fw.M00 = s; 			fw.M01 = 0; 			fw.M02 = 0; 			fw.M03 = 0;
	fw.M10 = 0; 			fw.M11 = s; 			fw.M12 = 0; 			fw.M13 = 0;
	fw.M20 = 0;				fw.M21 = 0; 			fw.M22 = s;   			fw.M23 = 0;
	fw.M30 = 0;				fw.M31 = 0;             fw.M32 = 0;             fw.M33 = 1;
	
	
	bw.M00 = 1.0/s;		 	bw.M01 = 0; 			bw.M02 = 0; 			bw.M03 = 0;
	bw.M10 = 0; 			bw.M11 = 1.0/s; 		bw.M12 = 0; 			bw.M13 = 0;
	bw.M20 = 0;				bw.M21 = 0; 			bw.M22 = 1.0/s;   		bw.M23 = 0;
	bw.M30 = 0;				bw.M31 = 0;             bw.M32 = 0;             bw.M33 = 1;
	

	return new Trafo3d(fw, bw);

};

perspectiveTrafo = function(horizontalFieldOfView, aspect, near, far) {
	var wh = Math.tan(horizontalFieldOfView*0.5)*near;
	var hh = wh / aspect;
	//alert(1 + "x" + hh/wh);
	return frustumTrafo(-wh, wh, -hh, hh, near, far);

};

translationTrafo = function(pos) {
	return new Trafo3d(translation(pos), translation(new V3f(0,0,0).minus(pos)));
};


getElementType = function(array) {
	var element = array[0];
	
	if(element.Type != undefined) {
		return element.Type;
	}
	//rather complicated check for int/float
	if(typeof element == 'number') {
		var fv = parseFloat(element);
		var iv = parseInt(element, 10);
		
		if(iv == fv && !isNaN(element)) {
			return Type.Int;
		}
		else return Type.Float;
	}
	else if(element instanceof V2f) return Type.Float2;
	else if(element instanceof V3f) return Type.Float3;
	else if(element instanceof V4f) return Type.Float4;
	else if(element instanceof V2i) return Type.Int2;
	else if(element instanceof V3i) return Type.Int3;
	else if(element instanceof V4i) return Type.Int4;
	else if(element instanceof C3b) return Type.Byte3;
	else if(element instanceof C4b) return Type.Byte4;
	//else if(element instanceof M33f) return Type.Float3x3;
	else if(element instanceof M44f) return Type.Float4x4;

	throw "unknown type";
};


toFloatArray = function(array) {
	var e0 = array[0];
	
	var result = new Array();
	var type = getElementType(array);
	
	if(e0 % 1 === 0) {
		return new Uint16Array(array);
	}
	else if(typeof e0 == 'number') {
		return new Float32Array(array);
	}
	else if(type == Type.Float) {
		new Float32Array(array);
	}
	else if(type == Type.Float2) {
		for(i = 0; i < array.length; i++) {
			var e = array[i];
			
			result[2*i+0] = e.X;
			result[2*i+1] = e.Y;
		}
	}
	else if(type == Type.Float3) {
		for(i = 0; i < array.length; i++) {
			var e = array[i];
			try {
			result[3*i+0] = e.X;
			result[3*i+1] = e.Y;
			result[3*i+2] = e.Z;
			}
			catch(x) {
				alert(i + ": " + e);
			}
		}
	}
	else if(type == Type.Float4) {
		for(i = 0; i < array.length; i++) {
			var e = array[i];
			result[4*i+0] = e.X;
			result[4*i+1] = e.Y;
			result[4*i+2] = e.Z;
			result[4*i+3] = e.W;
		}
	}
	else throw "cannot flatten array: "+ type;

	return new Float32Array(result);

};
