

function compile(code, composition, action)
{
	var request = new XmlHttpRequest();
	request.onload = function() {
		if(request.status == 200)
		{
			action(request.responseText);
		}
		else action("asdasd")
	};
	
	request.open("POST", "http://localhost:1462/", true);
	request.setRequestHeader("Content-Type", "text/plain;charset=UTF-8");
}


function updateGLSLCode()
{
	var code = document.getElementById("code").value;
	var comp = document.getElementById("comp").value;
	var target = document.getElementById("glsl");
	alert("asdasd");
	compile(code, comp, function(code) {
		target.value = code;
	});
	
}