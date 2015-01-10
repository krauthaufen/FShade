function CameraController(mouse, keyboard, view, proj) {
	this.autorotate = false;
	this.explore = false;
	this.moveForward = false;
	this.moveBackward = false;
	this.rotate = false;
	this.drag = false;
	this.currentZoom = 0.0;
	this.zoomTarget = 0.0;
	this.lastMouse = new V2f(0,0);
	this.shift = false;
	this.control = false;
	this.alt = false;
	this.resetToExplore = false;
	this.targetRotationX = 0.0;
	this.currentRotationX = 0.0;
	this.targetRotationY = 0.0;
	this.currentRotationY = 0.0;
	this.MoveSpeed = 4.0;
	
	this.AutorotateSpeed = 0.2;
	
	
	this.ViewTrafo = view;
	this.ProjTrafo = proj;
	
	this.Mouse = mouse;
	this.Keyboard = keyboard;
	
	this.install();
}


CameraController.prototype.install = function() {

	this.Mouse.addListener(this);
	this.Keyboard.addListener(this);
	var me = this;
};

CameraController.prototype.idle = function(dt) {

	if(this.autorotate) {
		this.ViewTrafo = rotationTrafo(-this.AutorotateSpeed*dt , new V3f(0,1,0)).mul(this.ViewTrafo);
	}
	
	
	if(this.resetToExplore) {
		var diffX = this.targetRotationX - this.currentRotationX;
		var diffY = this.targetRotationY - this.currentRotationY;
		if(Math.abs(diffX) > 0.0005 || Math.abs(diffY) > 0.0005) {
			var rstep = Math.min(4.0*dt, 1.0);

			var dx = diffX * rstep;
			var dy = diffY * rstep;
			
			var up = this.ViewTrafo.Forward.transformDir(new V3f(0,1,0)).normalized();
			
			this.ViewTrafo = this.ViewTrafo.mul(rotationTrafo(dx, up)).mul(rotationTrafo(dy, new V3f(1,0,0)));
			this.currentRotationX = this.currentRotationX + dx;
			this.currentRotationY = this.currentRotationY + dy;
		}
		else {
			var up = this.ViewTrafo.Forward.transformDir(new V3f(0,1,0)).normalized();
			
			this.ViewTrafo = this.ViewTrafo.mul(rotationTrafo(diffX, up)).mul(rotationTrafo(diffY, new V3f(1,0,0)));
			this.resetToExplore = false;
		}
	}
	
	if(this.explore) {
		var moveDelta = new V3f(0,0,0);
		if(this.moveForward) moveDelta.Z =  moveDelta.Z + this.MoveSpeed*dt;
		if(this.moveBackward) moveDelta.Z = moveDelta.Z - this.MoveSpeed*dt;
		if(this.moveLeft) moveDelta.X =  moveDelta.X + this.MoveSpeed*dt;
		if(this.moveRight) moveDelta.X = moveDelta.X - this.MoveSpeed*dt;

	
		var fw = this.ViewTrafo.Backward.transformDir(new V3f(0,0,1));
		var r = this.ViewTrafo.Backward.transformDir(new V3f(1,0,0));
		
		fw = new V3f(fw.X*moveDelta.Z, fw.Y*moveDelta.Z, fw.Z*moveDelta.Z);
		r = new V3f(r.X*moveDelta.X, r.Y*moveDelta.X, r.Z*moveDelta.X);
		
		this.ViewTrafo = translationTrafo(fw.plus(r)).mul(this.ViewTrafo);
	}
	
	
	var deltaZoom = this.zoomTarget - this.currentZoom;
		
	if(Math.abs(deltaZoom) > 0.01) {
	
		var forward = this.ViewTrafo.Backward.transformDir(new V3f(0,0,1));
		var dz = deltaZoom * 0.1;
		this.ViewTrafo = translationTrafo(new V3f(forward.X*dz, forward.Y*dz, forward.Z*dz)).mul(this.ViewTrafo);

		this.currentZoom = this.currentZoom + dz;
	}
	
};



CameraController.prototype.mouseDown = function(event) {

	if(event.button == 0) {
		this.rotate = true;
	}
	else if(event.button == 1) {
		this.drag = true;
	}
    this.lastMouse = new V2f(event.clientX, event.clientY);
}

CameraController.prototype.mouseUp = function(event) {
	this.rotate = false;
	this.drag = false;
	//alert("sadsa");
}

CameraController.prototype.mouseScroll = function(delta) {
	this.zoomTarget = this.zoomTarget + delta;
};

CameraController.prototype.mouseMoved = function(event) {
	var current = new V2f(event.clientX, event.clientY);;
	var delta = current.minus(this.lastMouse);

	try {
	if(this.rotate) {
		//alert(this.ProjTrafo);
		if(this.explore) {
		
			
			var up = this.ViewTrafo.Forward.transformDir(new V3f(0,1,0));
			this.ViewTrafo = this.ViewTrafo.mul(rotationTrafo(delta.X / 200.0, up));
			this.ViewTrafo = this.ViewTrafo.mul(rotationTrafo(delta.Y / 200.0, new V3f(1,0,0)));
		
		}
		else {
		
			
			
			this.ViewTrafo = rotationTrafo(delta.X / 200.0, new V3f(0,1,0)).mul(this.ViewTrafo);
			
			var right = this.ViewTrafo.Backward.transformDir(new V3f(1,0,0));
			this.ViewTrafo = rotationTrafo(delta.Y / 200.0, right).mul(this.ViewTrafo);
		}
	}
	else if(this.drag) {
		//alert(this.ViewTrafo);
		var right = this.ViewTrafo.Backward.transformDir(new V3f(1,0,0));
		var up = this.ViewTrafo.Backward.transformDir(new V3f(0,1,0));
		
		var dx = delta.X / 200.0;
		var dy = -delta.Y / 200.0;
		if(this.explore) {
			dy = -dy;
			dx = -dx;
		}
		this.ViewTrafo = translationTrafo(new V3f(up.X*dy, up.Y*dy, up.Z*dy)).mul(this.ViewTrafo);
		this.ViewTrafo = translationTrafo(new V3f(right.X*dx, right.Y*dx, right.Z*dx)).mul(this.ViewTrafo);
		
	}
	}
	catch(x){alert(x);}
	
	this.lastMouse = current;
};

CameraController.prototype.keyDown = function(event) {
	//alert(event.keyCode);
	if(event.keyCode == 82 && !this.control && !this.shift &&!this.alt) {
		this.autorotate = !this.autorotate;
	}
	else if(event.keyCode == 32) {
		this.explore = !this.explore;
		
		if(!this.explore) {
			//viewTrafo = lookAtTrafo(new V3f(6,3,2), new V3f(0,0,0), new V3f(0,1,0));
			
			var fw = new V3f(0,0,-1);
			var p = this.ViewTrafo.Backward.transformPos(new V3f(0,0,0));
			var target = this.ViewTrafo.Forward.transformDir(new V3f(0,0,0).minus(p)).normalized();
			var up = this.ViewTrafo.Forward.transformDir(new V3f(0,1,0)).normalized();
			var right = new V3f(1,0,0);

			var dtu = target.dot(up);
			var dfu = fw.dot(up);
			var dtr = target.dot(right);
			var dfr = fw.dot(right);
			
			var tx = target.minus(new V3f(dtu*up.X, dtu*up.Y, dtu*up.Z)).normalized();
			var fx = fw.minus(new V3f(dfu*up.X, dfu*up.Y, dfu*up.Z)).normalized();
			
			var ty = target.minus(new V3f(dtr*right.X, dtr*right.Y, dtr*right.Z)).normalized();
			var fy = fw.minus(new V3f(dfr*right.X, dfr*right.Y, dfr*right.Z)).normalized();

			this.currentRotationX = 0.0;
			this.currentRotationY = 0.0;
			this.targetRotationX = Math.atan2(tx.dot(right), tx.dot(fx));//0.0;//-Math.acos(tx.dot(fx));
			
			
			var angle1 = Math.acos(fw.dot(up));
			var angle2 = Math.acos(target.dot(up));
			this.targetRotationY = angle2 - angle1;
			
			this.resetToExplore = true;
			//alert(targetRotation);
		}
	}
	else if(event.keyCode == 18)this.alt = true;
	else if(event.keyCode == 17)this.control = true;
	else if(event.keyCode == 16)this.shift = true;
	
	else if(event.keyCode == 87 || event.keyCode ==38)this.moveForward = true;
	else if(event.keyCode == 83 || event.keyCode ==40)this.moveBackward = true;
	else if(event.keyCode == 65 || event.keyCode ==37)this.moveLeft = true;
	else if(event.keyCode == 68 || event.keyCode ==39)this.moveRight = true;

	else if(event.keyCode == Keys.PageUp) this.MoveSpeed = this.MoveSpeed * 1.5;
	else if(event.keyCode == Keys.PageDown) this.MoveSpeed = this.MoveSpeed / 1.5;
	
	//else alert(event.keyCode); //w=87 s=83 a=65 d=68 left=37 right=39 up=38 down=40
	return true;
};

CameraController.prototype.keyUp = function(event) {

	if(event.keyCode == 18)this.alt = false;
	else if(event.keyCode == 17)this.control = false;
	else if(event.keyCode == 16)this.shift = false;
	
	else if(event.keyCode == 87 || event.keyCode ==38)this.moveForward = false;
	else if(event.keyCode == 83 || event.keyCode ==40)this.moveBackward = false;
	else if(event.keyCode == 65 || event.keyCode ==37)this.moveLeft = false;
	else if(event.keyCode == 68 || event.keyCode ==39)this.moveRight = false;
	
	
	return true;
};

