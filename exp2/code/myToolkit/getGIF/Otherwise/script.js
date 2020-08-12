//generative-regular
// var trial_sqc ={"obj":["E","A","E","E","A","E","E","A","E","E","A","E","E"],"time":["0","2.772784","3.769044","4.559107","6.565542","8.826321","9.358511","11.078088","12.30697","15.188192","17.297149","18.376911","19.558342"]}
//generative-irregular
// var trial_sqc ={"obj":["E","E","A","E","A","E","E","A","E","A","E","E","E"],"time":["0","0.6265659","2.7727843","3.7690436","6.5655424","7.4971295","8.8263213","11.0780882","12.3069697","17.2971489","18.3769111","19.6991604","22.2068996"]}

// //prevention-regular
// var preFlag=1;
// var trial_sqc ={"obj":["E","A","E","A","A","E","A"],"time":["0","3.850083","9.696661","11.49685","13.57551","18.692764","19.919253"]}

// //prevention-irregular
// var preFlag=1;
// var trial_sqc ={"obj":["E","A","E","A","A","E","E","A"],"time":["0","3.850083","8.145244","11.49685","13.57551","17.412453","18.365184","19.919253"]}

// //noncausal-regular
// var trial_sqc ={"obj":["E","E","A","E","A","E","A","E","A"],"time":["0","4.765785","8.596248","10.123235","12.877074","15.108361","16.205193","19.18244","23.263884"]}

// //noncausal-irregular
var trial_sqc ={"obj":["E","E","A","E","E","A","A","E","A"],"time":["0","6.876077","8.596248","10.389125","12.182999","12.877074","16.205193","19.536083","23.263884"]}


var block_time=["3.0","5.08","3.0","3.0"];

var  preAct=0;
var trialNumber=0;
var onFor=350;
var clipLength=24*1000;
var onColor="#FFD966";
var offColor="#D9D9D9";
var clickACount=0;
var clickBCount=0;
var connect_color=["#ededed","black","red"];
var connect_state=["N","G","P"];
var pracFlag=1;
var baserateFlag=0;
var typeFlag=0;
var data=new Array;
var beginTime=0;
var myOrder=undefined;

var connect_hint_color=["#ababab","black","red"];
var connect_state=["N","G","P"];
var connect_name=["Non-causal","Generative","Preventative"];



document.getElementById("frame_hand_A").style.visibility="hidden";

function TrialStart(){
	document.getElementById("exp_start").disabled=true;
	document.getElementById("hint").innerHTML="Trial ongoing..."
	document.getElementById('frame_line_A').style.pointerEvents = 'auto';
	beginTime=Date.now();

	for (var i = 0; i <trial_sqc.time.length; i++) {
		setTimeout(TurnOnProcess,Number(trial_sqc.time[i])*1000,trial_sqc.obj[i]);
	}
	setTimeout(ClipEnd,clipLength);
}

function TurnOnProcess(dot){
	if (dot=="E"){
		document.getElementById("frame_bulb").style.backgroundColor=onColor;
	}else {
		document.getElementById("light_button_"+dot).style.backgroundColor=onColor;
		document.getElementById("frame_hand_"+dot).style.visibility = "visible";
	}
	setTimeout(TurnOffProcess,onFor,dot);

	if (preFlag==1 && dot=="A"){
		ShowHint();
	}
}

function ShowHint(){
	document.getElementById("inner_hint").innerHTML = "(Period of Prevention...)";
	setTimeout(HideHint,block_time[preAct]*1000);
	preAct=preAct+1;
}

function HideHint(){
	document.getElementById("inner_hint").innerHTML = "";
}

function TurnOffProcess(dot){
	if (dot=="E"){
		document.getElementById("frame_bulb").style.backgroundColor=offColor;
	}else{
		document.getElementById("light_button_"+dot).style.backgroundColor=offColor;
		document.getElementById("frame_hand_"+dot).style.visibility = "hidden";
	}
}

function ClipEnd(){
	document.getElementById("hint").innerHTML="Decide your answer and click <b>Next</b> to move onto the next device."
	document.getElementById("exp_next").disabled=false;
}

function ClickA(){
	clickACount=clickACount+1;
	document.getElementById("arrow_line_A").style.background=connect_color[clickACount % 3];
	document.getElementById("arrow_point_A").style.borderLeftColor=connect_color[clickACount % 3];
	document.getElementById("arrow_hint_A").innerHTML=connect_name[clickACount % 3];
	document.getElementById("arrow_hint_A").style.color=connect_hint_color[clickACount % 3];

	data[data.length]={order:trialNumber,
		trial_type:trial_sqc.type,
		id:trial_sqc.id,
		A_pro:trial_sqc.a,
		B_pro:trial_sqc.b};
	data[data.length-1].begintime=beginTime;
	data[data.length-1].unixtime=Date.now();
	data[data.length-1].A_state=connect_state[clickACount % 3];
	data[data.length-1].B_state=connect_state[clickBCount % 3];
	data[data.length-1].data_type="online";
}


