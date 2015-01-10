function VertexGeometry(name, positions, texCoords, indices, bounds) {
	this.Name = name;
	this.Indices = indices;
	this.HasIndices = indices != null;
	this.Positions = positions;
	this.TexCoords = texCoords;
	this.BoundingBox = bounds;
	this.Material = new Material();
};

function Material() {
	this.AmbientColor = new C3b(0,0,0);
	this.DiffuseColor = new C3b(255,255,255);
	this.DiffuseTexture = null;
	this.NormalMap = null;
};


String.prototype.startsWith = function (str){
	return this.indexOf(str) == 0;
};


createTangents = function(positions, coordinates, vertexIndices, coordinateIndices, tangentIndices, normals, uta, vta) {
	var via = vertexIndices;
	var pa = positions;
	var cia = coordinateIndices;
	var ca = coordinates;
	var tia = tangentIndices;
	
	try {
	var fi = 0;
	for(var fvi = 0; fvi < via.length;)
	{
		var p0 = pa[via[fvi]]; var ci0 = cia[fvi]; var c0 = ca[ci0]; var ti0 = tia[fvi++];
		var p1 = pa[via[fvi]]; var ci1 = cia[fvi]; var c1 = ca[ci1]; var ti1 = tia[fvi++];
		var p2 = pa[via[fvi]]; var ci2 = cia[fvi]; var c2 = ca[ci2]; var ti2 = tia[fvi++];
		
		
		//alert(c2 + ", " + c1 + ", " + c0);
		var v10 = p1.minus(p0); var c10 = c1.minus(c0); var v10len = v10.length();
		var v20 = p2.minus(p0); var c20 = c2.minus(c0); var v20len = v20.length();

		
		var t0 = new V3f(0,0,0); var t1 = new V3f(0,0,0);
		var alpha0 = 0.0;
		var	alpha1 = 0.0;
		var alpha2 = 0.0;
		
		var d = c10.X * c20.Y - c20.X * c10.Y;
		if (d != 0.0)
		{
			
			d = 1.0 / d;
			t0 = ((v10.mul(c20.Y).minus(v20.mul(c10.Y))).mul(d)).normalized();
			t1 = ((v20.mul(c10.X).minus(v10.mul(c20.X))).mul(d)).normalized();
			
			
			var v21 = p2.minus(p1); var v21len = v21.length();
			if (v10len > 0.00000000001 && v20len > 0.00000000001 && v21len > 0.00000000001)
			{
				
				alpha0 = Math.acos(v10.dot(v20) / (v10len * v20len));
				alpha1 = Math.acos(-v10.dot(v21) / (v10len * v21len));
				alpha2 = Math.acos(v20.dot(v21) / (v20len * v21len));
			}
			
			
		}
			
		if(uta[ti0] == undefined) uta[ti0] = new V3f(0,0,0);
		if(vta[ti0] == undefined) vta[ti0] = new V3f(0,0,0);
		if(uta[ti1] == undefined) uta[ti1] = new V3f(0,0,0);
		if(vta[ti1] == undefined) vta[ti1] = new V3f(0,0,0);
		if(uta[ti2] == undefined) uta[ti2] = new V3f(0,0,0);
		if(vta[ti2] == undefined) vta[ti2] = new V3f(0,0,0);
			
			
		uta[ti0] = uta[ti0].plus(t0.mul(alpha0)); 
		vta[ti0] = vta[ti0].plus(t1.mul(alpha0));
		uta[ti1] = uta[ti1].plus(t0.mul(alpha1)); 
		vta[ti1] = vta[ti1].plus(t1.mul(alpha1));
		uta[ti2] = uta[ti2].plus(t0.mul(alpha2)); 
		vta[ti2] = vta[ti2].plus(t1.mul(alpha2));

		
		
		p1 = p2; ci1 = ci2; c1 = c2; ti1 = ti2;
		v10 = v20; c10 = c20; v10len = v20len;
	
		fi++;
	}
	
	
	
	for(var ti = 0; ti < uta.length; ti++) {
	
		if(uta[ti] == undefined) uta[ti] = new V3f(0,1,0);
		if(vta[ti] == undefined) vta[ti] = new V3f(1,0,0);
		
		
		var n = normals[ti];
		
		//uta[ti] = uta[ti].minus(n.mul(n.dot(uta[ti])));
		//vta[ti] = vta[ti].minus(n.mul(n.dot(vta[ti])));
		//vta[ti] = vta[ti].minus(uta[ti].mul(uta[ti].dot(vta[ti])));
		
		uta[ti] = uta[ti].normalized();
		vta[ti] = vta[ti].normalized();
	}
	
	}
	catch(x) {
		alert(x); }
};


function parseObj(path) {
	var txtFile = new XMLHttpRequest();
	txtFile.open("GET", "../gltest/data/" + path, false);
	
	var realPositions = new Array();
	var realTexCoords = new Array();
	var realNormals = new Array();
	var realTangents = new Array();
	var realBiNormals = new Array();
	
	var geometries = new Array();
	var geometryIndex = 0;
	
	var maxX = -666.66E6;
	var maxY = -666.66E6;
	var maxZ = -666.66E6;
	
	var minX = 666.66E6;
	var minY = 666.66E6;
	var minZ = 666.66E6;
	
	var mtlFiles = new Array();
	
	txtFile.onreadystatechange = function () {
		
		if (txtFile.status!=200) alert("Error executing XMLHttpRequest call!");
		
		var positions = new Array();
		var texCoords = new Array();
		var normals = new Array();
		var vertexIndex = 0;
		var tcCount = 0;
		var normalCount = 0;
		var pFaces = new Array();
		var tFaces = new Array();
		var nFaces = new Array();
		var pFaceCount = 0;
		var tFaceCount = 0;
		var nFaceCount = 0;
		var mtlname = "global";
		
		lines = txtFile.responseText.split("\n");
		var currentName = "global";
		
		for(var l = 0; l < lines.length; l++) {
			var line = lines[l].trim();
			if(line.startsWith("mtllib")) {
				var mtlFile = line.split(/[ \t\r\n]+/)[1];
				mtlFiles[mtlFiles.length] = mtlFile;
			}
			else if(line.startsWith("usemtl")) {
				mtlname = line.split(/[ \t\r\n]+/)[1];
			}
			else if(line.startsWith("g")) {
				//alert(pFaceCount);
				
				
				if(pFaceCount > 0) {
				
					if(!currentName.startsWith("FcF") && !currentName.startsWith("Bip") && !currentName.startsWith("CpRig")) {
				
					var tangents = null;
					var binormals = null;
					if(texCoords.length > 0) {
						tangents = new Array();
						binormals = new Array();
						createTangents(positions, texCoords, pFaces, nFaces, tFaces, normals, tangents, binormals);
					}
					for(var i = 0; i < pFaces.length; i++) {
						if(pFaces[i] >= 0 && pFaces[i] < positions.length) {
							var p = positions[pFaces[i]];
					
							realPositions[i] = p;//new V3f(s*(p.X - avgX), s*(p.Y - avgY), s*(p.Z - avgZ));
							if(texCoords.length > 0 && nFaces.length > 0) realTexCoords[i] = texCoords[nFaces[i]];
							if(normals.length > 0 && tFaces.length > 0) {
								realNormals[i] = normals[tFaces[i]];
								if(texCoords.length > 0) {
									realTangents[i] = tangents[tFaces[i]];
									realBiNormals[i] = binormals[tFaces[i]];
								}
							}
						}	
						else alert(i + ": " + pFaces[i] + "<" + positions.length);
					}
					var vg = new VertexGeometry(currentName, realPositions, realTexCoords, null, new Box3f(new V3f(minX, minY, minZ), new V3f(maxX, maxY, maxZ)));
					vg.Normals = realNormals;
					vg.Tangents = realTangents;
					vg.BiNormals = realBiNormals;
					vg.MaterialName = mtlname;
					
					geometries[geometryIndex] = vg;
					geometryIndex++;
					
					}
					currentName = line.split(/[ \t\r\n]+/)[1];
	
		
					pFaceCount = 0;
					tFaceCount = 0;
					nFaceCount = 0;
					pFaces = new Array();
					tFaces = new Array();
					nFaces = new Array();
					
					
					
					maxX = -666.66E6;
					maxY = -666.66E6;
					maxZ = -666.66E6;
					
					minX = 666.66E6;
					minY = 666.66E6;
					minZ = 666.66E6;
					
					realPositions = new Array();
					realTexCoords = new Array();
					realNormals = new Array();
					realTangents = new Array();
					realBiNormals = new Array();
				}
				
			}
			else if (line.startsWith("vt")) {
			//alert(line);
				var components = line.split(/[ \t\r\n]+/);
				if(components.length >= 3) {
					var x = parseFloat(components[1]);
					var y = parseFloat(components[2]);
					texCoords[tcCount] = new V2f(x,y);
					tcCount = tcCount + 1;
				}
			}
			else if (line.startsWith("vn")) {
			//alert(line);
				var components = line.split(/[ \t\r\n]+/);
				if(components.length >= 4) {
					var x = parseFloat(components[1]);
					var y = parseFloat(components[2]);
					var z = parseFloat(components[3]);
					normals[normalCount] = new V3f(x,y,z);
					normalCount = normalCount + 1;
				}
			}
			else if (line.startsWith("v")) {
				var components = line.split(/[ \t\r\n]+/);
				if(components.length >= 4) {
					var x = parseFloat(components[1]);
					var y = parseFloat(components[2]);
					var z = parseFloat(components[3]);
					
					if(x < minX)minX = x;
					if(y < minY)minY = y;
					if(z < minZ)minZ = z;
					
					if(x > maxX)maxX = x;
					if(y > maxY)maxY = y;
					if(z > maxZ)maxZ = z;
					
					positions[vertexIndex] = new V3f(x,y,z);
					vertexIndex = vertexIndex + 1;
				}
			}
			else if(line.startsWith("f")) {
				var components = line.split(/[ \t]+/);
				//alert(components);
				if(components.length == 5) {
					//quad
					
					var f0 = components[1].split("/");
					var f1 = components[2].split("/");
					var f2 = components[3].split("/");
					var f3 = components[4].split("/");
					
					//f0/f1/f2
					//f2/f1/f3
					
					if(f0.length > 0 && f0[0].length > 0) {
						pFaces[pFaceCount + 0] = parseInt(f0[0])-1;
						pFaces[pFaceCount + 1] = parseInt(f1[0])-1;
						pFaces[pFaceCount + 2] = parseInt(f2[0])-1;
						
						pFaces[pFaceCount + 3] = parseInt(f0[0])-1;
						pFaces[pFaceCount + 4] = parseInt(f2[0])-1;
						pFaces[pFaceCount + 5] = parseInt(f3[0])-1;

						pFaceCount += 6;
					}
					
					if(f0.length > 1 && f0[1].length > 0) {
						nFaces[nFaceCount + 0] = parseInt(f0[1])-1;
						nFaces[nFaceCount + 1] = parseInt(f1[1])-1;
						nFaces[nFaceCount + 2] = parseInt(f2[1])-1;
						
						nFaces[nFaceCount + 3] = parseInt(f0[1])-1;
						nFaces[nFaceCount + 4] = parseInt(f2[1])-1;
						nFaces[nFaceCount + 5] = parseInt(f3[1])-1;
						
			
						nFaceCount += 6;
					}
					
					if(f0.length > 2 && f0[2].length > 0) {
						tFaces[tFaceCount + 0] = parseInt(f0[2])-1;
						tFaces[tFaceCount + 1] = parseInt(f1[2])-1;
						tFaces[tFaceCount + 2] = parseInt(f2[2])-1;
						
						tFaces[tFaceCount + 3] = parseInt(f0[2])-1;
						tFaces[tFaceCount + 4] = parseInt(f2[2])-1;
						tFaces[tFaceCount + 5] = parseInt(f3[2])-1;
				
						
						tFaceCount += 6;
					}
				}
				else if(components.length == 4) {
					var f0 = components[1].split("/");
					var f1 = components[2].split("/");
					var f2 = components[3].split("/");
					
					if(f0.length > 0 && f0[0].length > 0) {
						pFaces[pFaceCount + 0] = parseInt(f0[0])-1;
						pFaces[pFaceCount + 1] = parseInt(f1[0])-1;
						pFaces[pFaceCount + 2] = parseInt(f2[0])-1;

						pFaceCount += 3;
					}
					
					if(f0.length > 1 && f0[1].length > 0) {
						nFaces[nFaceCount + 0] = parseInt(f0[1])-1;
						nFaces[nFaceCount + 1] = parseInt(f1[1])-1;
						nFaces[nFaceCount + 2] = parseInt(f2[1])-1;
						nFaceCount += 3;
						

					}
					
					if(f0.length > 2 && f0[2].length > 0) {
						tFaces[tFaceCount + 0] = parseInt(f0[2])-1;
						tFaces[tFaceCount + 1] = parseInt(f1[2])-1;
						tFaces[tFaceCount + 2] = parseInt(f2[2])-1;

						tFaceCount += 3;
					}
					
				}
			
			}
	  
		}
	  
	 
		
		if(pFaceCount > 0) {
		
		
			var tangents = new Array();
			var binormals = new Array();
			createTangents(positions, texCoords, pFaces, nFaces, tFaces, normals, tangents, binormals);
		
			realPositions = new Array();
			realTexCoords = new Array();
			realNormals = new Array();
			realTangents = new Array();
			realBiNormals = new Array();
			
			for(var i = 0; i < pFaces.length; i++) {
				if(pFaces[i] >= 0 && pFaces[i] < positions.length) {
					var p = positions[pFaces[i]];
			
					realPositions[i] = p;//new V3f(s*(p.X - avgX), s*(p.Y - avgY), s*(p.Z - avgZ));
					if(nFaces.length > 0) realTexCoords[i] = texCoords[nFaces[i]];
					if(tFaces.length > 0) {
						realNormals[i] = normals[tFaces[i]];
						realTangents[i] = tangents[tFaces[i]];
						realBiNormals[i] = binormals[tFaces[i]];
					}
					
				}	
				else alert(i + ": " + pFaces[i] + "<" + positions.length);
			}
			
			
			
			var vg = new VertexGeometry(currentName, realPositions, realTexCoords, null, new Box3f(new V3f(minX, minY, minZ), new V3f(maxX, maxY, maxZ)));
			vg.Normals = realNormals;
			vg.Tangents = realTangents;
			vg.BiNormals = realBiNormals;
			vg.MaterialName = mtlname;
			
			geometries[geometryIndex] = vg;
			geometryIndex++;
		}
  
	
	};
	
	txtFile.send(null);
	
	var materials = new Object();
	var currentMaterial = null;
	var currentName = "global";
	//alert(mtlFiles.length);
	
	for(var i = 0; i < mtlFiles.length; i++) {
		var mtFile = new XMLHttpRequest();
		mtFile.open("GET", "../gltest/data/" + mtlFiles[i], false);
		
		mtFile.onreadystatechange = function() {
			if (mtFile.status!=200) alert("Error executing XMLHttpRequest call!");
			lines = mtFile.responseText.split("\n");
			for(var l = 0; l < lines.length; l++) {
				var line = lines[l].trim();
				
				if(line.startsWith("newmtl")) {
					if(currentMaterial != null) {
						materials[currentName] = currentMaterial;
					}
					currentName = line.split(/[ \t]+/)[1];
					currentMaterial = new Material();
				}
				else if(line.startsWith("map_Kd")) {
					currentMaterial.DiffuseTexture = line.split(/[ \t]+/)[1];
				}
				else if(line.startsWith("map_bump")) {
					currentMaterial.NormalMap = line.split(/[ \t]+/)[1];
				}
				
			}
			
			if(currentMaterial != null) {
				materials[currentName] = currentMaterial;
			}
		};
		
		mtFile.send(null);
	}
	
	
	for(var i = 0; i < geometries.length; i++) {
		var vg = geometries[i];
		if(vg.MaterialName != undefined && vg.MaterialName != null && materials[vg.MaterialName] != undefined) {
			vg.Material = materials[vg.MaterialName];
			//alert("assigned material to " + vg.MaterialName);
		}
	}
	
	
	
	
	//alert(geometries.length);
	return geometries;
	
};


function parseV2f(str) {
	var components = str.split(/[ \t]+/);
	var x = parseFloat(components[1]);
	var y = parseFloat(components[2]);
	
	return new V2f(x,y);
}

function parseV3f(str) {
	var components = str.split(/[ \t]+/);
	var x = parseFloat(components[1]);
	var y = parseFloat(components[2]);
	var z = parseFloat(components[3]);
	
	return new V3f(x,y,z);
}

function createGeometry(name, matName, positions,normals,texCoords,posIndex,normalIndex,tcIndex) {

	var canBeIndexed = true;
	
	var tcIndexTrafo = new Array();
	var nIndexTrafo = new Array();
	for(var i = 0; i < posIndex.length; i++) {
		var vi = posIndex[i];
		var ti = tcIndex[i];
		var ni = normalIndex[i];
		
		if(tcIndexTrafo[vi] == undefined || tcIndexTrafo[vi] == ti) {
			tcIndexTrafo[vi] = ti;
		}
		else {
			canBeIndexed = false;
			break;
		}
		
		if(nIndexTrafo[vi] == undefined || nIndexTrafo[vi] == ni) {
			nIndexTrafo[vi] = ni;
		}
		else {
			canBeIndexed = false;
			break;
		}
		
	}
	
	if(canBeIndexed) {
	
		var newCoords = new Array();
		var newNormals = new Array();
		
		for(var i = 0; i < posIndex.length; i++) {
		
			newCoords[i] = texCoords[tcIndexTrafo[i]];
			newNormals[i] = normals[nIndexTrafo[i]];
		}
		
		texCoords = newCoords;
		normals = newNormals;
		normalIndex = null;
		tcIndex = null;
	}
	
	//createTangents = function(positions, coordinates, vertexIndices, coordinateIndices, tangentIndices, normals, uta, vta)
	
	
	var tangents = new Array();
	var binormals = new Array();
	createTangents(positions, texCoords, posIndex, canBeIndexed ? posIndex : tcIndex, canBeIndexed ? posIndex : normalIndex, normals, tangents. binormals);
	
	
	var minX = 666.666;
	var minY = 666.666;
	var minZ = 666.666;
	var maxX = -666.666;
	var maxY = -666.666;
	var maxZ = -666.666;
	
	for(var i = 0; i < positions.length; i++) {
		var pi = positions[i];
		
		if(pi.X > maxX)maxX = pi.X;
		if(pi.Y > maxY)maxY = pi.Y;
		if(pi.Z > maxZ)maxZ = pi.Z;
		if(pi.X < minX)minX = pi.X;
		if(pi.Y < minY)minY = pi.Y;
		if(pi.Z < minZ)minZ = pi.Z;
	}
	
	var box = new Box3f(new V3f(minX, minY, minZ), new V3f(maxX, maxY, maxZ));
	
	if(!canBeIndexed || true) {
		//flatten
		var realPos = new Array();
		var realNorm = new Array();
		var realCoords = new Array();
		var realTangents = new Array();
		var realBinormals = new Array();

		for(var i = 0; i < posIndex.length; i++) {
			realPos[i] = positions[posIndex[i]];
			realNorm[i] = normals[normalIndex[i]];
			realTangents[i] = tangents[normalIndex[i]];
			realBinormals[i] = binormals[normalIndex[i]];
			realCoords[i] = texCoords[tcIndex[i]];
		}
		
		//function VertexGeometry(name, positions, texCoords, indices, bounds) {
		var geometry = new VertexGeometry(name, realPos, realCoords, null, box);
		geometry.Normals = realNorm;
		geometry.Tangents = realTangents;
		geometry.BiNormals = realBinormals;
		geometry.MaterialName = matName;
		
		return geometry;
	}
	else {
		var geometry = new VertexGeometry(name, positions, texCoords, posIndex, box);
		geometry.Normals = normals;
		geometry.Tangents = tangents;
		geometry.BiNormals = binormals;
		geometry.MaterialName = matName;
		
		return geometry;
	}
	

}

function parseObj2(content) {

	var lines = content.split("\n");
	
	var mtllibs = new Array();
	
	var currentMaterial = null;
	var geometryName = null;
	
	var vertexPositions = new Array();
	var vertexNormals = new Array();
	var vertexCoordinates = new Array();
	
	var pIndices = new Array();
	var nIndices = new Array();
	var tIndices = new Array();
	
	var geometries = new Array();
	
	for(var l = 0; l < lines.length; l++) {
		var line = lines[l].trim();
		
		if(line.startsWith("vn")) {
			vertexNormals.push(parseV3f(line));
		}
		else if(line.startsWith("vt")) {
			vertexCoordinates.push(parseV2f(line));
		}
		else if(line.startsWith("v")) {
			vertexPositions.push(parseV3f(line));
		}
		else if(line.startsWith("usemtl")) {
			currentMaterial = line.split(/[ \t]+/)[1];
		}
		else if(line.startsWith("mtllib")) {
			mtllibs.push(line.split(/[ \t]+/)[1]);
		}
		else if(line.startsWith("g") || line.startsWith("o")) {
			//TODO: create objects
			
			if(pIndices.length > 0) {
				geometries.push(createGeometry(geometryName, currentMaterial, vertexPositions,vertexNormals,vertexCoordinates, pIndices, nIndices, tIndices));
			}
			
			
			geometryName = line.split(/[ \t]+/)[1];
			pIndices = new Array();
			nIndices = new Array();
			tIndices = new Array();
		}
		else if(line.startsWith("f")) {
			var indices = line.split(/[ \t]+/);
			var faces = new Array();
			for(var i = 1; i < indices.length; i++) {
				faces.push(indices[i].split("/"));
			}
			
			if(faces.length >=  3) {
				//triangle
				
				pIndices.push(parseInt(faces[0][0]) - 1);
				pIndices.push(parseInt(faces[1][0]) - 1);
				pIndices.push(parseInt(faces[2][0]) - 1);
				
				nIndices.push(parseInt(faces[0][1]) - 1);
				nIndices.push(parseInt(faces[1][1]) - 1);
				nIndices.push(parseInt(faces[2][1]) - 1);
				
				tIndices.push(parseInt(faces[0][2]) - 1);
				tIndices.push(parseInt(faces[1][2]) - 1);
				tIndices.push(parseInt(faces[2][2]) - 1);
			}
			
			if(faces.length >= 4) {
				//quad
			
				pIndices.push(parseInt(faces[0][0]) - 1);
				pIndices.push(parseInt(faces[2][0]) - 1);
				pIndices.push(parseInt(faces[3][0]) - 1);
				
				nIndices.push(parseInt(faces[0][1]) - 1);
				nIndices.push(parseInt(faces[2][1]) - 1);
				nIndices.push(parseInt(faces[3][1]) - 1);
				
				tIndices.push(parseInt(faces[0][2]) - 1);
				tIndices.push(parseInt(faces[2][2]) - 1);
				tIndices.push(parseInt(faces[3][2]) - 1);
			}
			
			
			
		}
	
	}
	
	if(pIndices.length > 0) {
		geometries.push(createGeometry(geometryName, currentMaterial, vertexPositions,vertexNormals,vertexCoordinates, pIndices, nIndices, tIndices));
	}

}





