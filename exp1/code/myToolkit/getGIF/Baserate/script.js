var trial_sqc ={"obj":["E","E","E","E","E","E","E"],"time":["0","4.5","6.4","10.4","19.0","22.9"]};

var trialNumber=0;
var onFor=350;
var clipLength=20*1000;
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
document.getElementById("frame_hand_A").style.visibility="hidden";
document.getElementById("frame_hand_B").style.visibility="hidden";

function TrialStart(){
	document.getElementById("exp_start").disabled=true;
	document.getElementById("hint").innerHTML="Trial ongoing..."
	document.getElementById('frame_line_A').style.pointerEvents = 'auto';
	document.getElementById('frame_line_B').style.pointerEvents = 'auto';
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

function ClickB(){
	clickBCount=clickBCount+1;
	document.getElementById("arrow_line_B").style.background=connect_color[clickBCount % 3];
	document.getElementById("arrow_point_B").style.borderLeftColor=connect_color[clickBCount % 3];
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
