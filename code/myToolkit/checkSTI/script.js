var onFor = 350; //How long should each node light up?
var baseColor = "0xB3B3B3";
var blockedColor = "0x000000";
var activeColor = "0xFFD000";

function showing(){
	var file = document.getElementById('datacsv').files[0];
	var reader = new FileReader();
	reader.onload= function (e){
		console.log(e)
		console.log(e.target.result)
		var csv2obj=new Object();
		csv2arr=e.target.result.split('\n');
		csvcol=csv2arr[0].split(',');
		for (var j = 0; j < csvcol.length; j++) {
			csv2obj[csvcol[j]]=[];
		}
		for (var i = 1; i < csv2arr.length-1; i++) {
			csvline=csv2arr[i].split(',');
			for (var j = 0; j <csvcol.length; j++) {
				csv2obj[csvcol[j]][i-1]=csvline[j];
			}
		}
		var dataset=csv2obj;
		console.log(dataset)
		//setTimeout(turnOnProcess,0,"E");///caustion
		for (var i = 0; i <dataset.time.length; i++) {
			setTimeout(turnOnProcess,Number(dataset.time[i])*1000,dataset.obj[i]);
		}
	}
	reader.readAsText(file);
}

function turnOnProcess(dot){
	document.getElementById("dot_"+dot).style.backgroundColor="yellow";
	setTimeout(turnOffProcess,onFor,dot)
}

function turnOffProcess(dot){
	document.getElementById("dot_"+dot).style.backgroundColor="gray";

}

function saving(){
	var file = document.getElementById('datacsv').files[0];
	var reader = new FileReader();
	reader.onload= function (e){
		console.log(e.target.result)
		console.log(e)
		var csv2obj=new Object();
		csv2arr=e.target.result.split('\n');
		csvcol=csv2arr[0].split(',');
		for (var j = 0; j < csvcol.length; j++) {
			csv2obj[csvcol[j]]=[];
		}
		for (var i = 1; i < csv2arr.length-1; i++) {
			csvline=csv2arr[i].split(',');
			for (var j = 0; j <csvcol.length; j++) {
				csv2obj[csvcol[j]][i-1]=csvline[j];
			}
		}
		var dataset=csv2obj;
		console.log(dataset)
		//setTimeout(turnOnProcess,0,"E");///caustion
		jsonData=JSON.stringify(dataset);
		download('var sti ='+jsonData, 'stimulus.js', '"text/javascript"');
	}
	reader.readAsText(file);
}


function download(content, fileName, contentType) {
    var a = document.createElement("a");
    var file = new Blob([content], {type: contentType});
    a.href = URL.createObjectURL(file);
    a.download = fileName;
    a.click();
}
