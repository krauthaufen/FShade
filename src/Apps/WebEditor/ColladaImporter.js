


parseCollada = function(path) {
	var txtFile = new XMLHttpRequest();
	txtFile.open("GET", "data/" + path, false);
    txtFile.overrideMimeType("text/xml");

	txtFile.onreadystatechange = function () {
		
		if (txtFile.status!=200) alert("Error executing XMLHttpRequest call!");
		
		var xml = txtFile.responseXML;
		var mesh = xml.querySelector("mesh");
		
	};
};