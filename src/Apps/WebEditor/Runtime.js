Type = {

	Byte : 1,
	Int : 2,
	Float : 3,
	
	Float2 : 4,
	Float3 : 5,
	Float4 : 6,
	
	Int2 : 7,
	Int3 : 8,
	Int4 : 9,
	
	Byte2 : 10,
	Byte3 : 11,
	Byte4 : 12,
	
	Float2x2 : 13,
	Float3x3 : 14,
	Float4x4 : 15,
	
	Texture2D : 16,
}

getTypeSize = function(type) {
	switch(type) {
		case Type.Byte: return 1;
		case Type.Int: return 4;
		case Type.Float: return 4;
		case Type.Float2: return 8;
		case Type.Float3: return 12;
		case Type.Float4: return 16;
		case Type.Int2: return 8;
		case Type.Int3: return 12;
		case Type.Int4: return 16;
		case Type.Byte2: return 2;
		case Type.Byte3: return 3;
		case Type.Byte4: return 4;
		
		case Type.Float2x2: return 16;
		case Type.Float3x3: return 48;
		case Type.Float4x4: return 64;
		
		case Type.Texture2D: return 4;
		
		default: throw "invalid: " + type;
	}
};
