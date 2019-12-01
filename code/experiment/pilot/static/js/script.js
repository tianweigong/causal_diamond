var ins_pages=["ins_pre","ins1","ins_baserate","ins_baserate_over","ins_type_generative","ins_type_preventative","ins_type_noncausal","bonus_reminder","check","checkagain","welldone"];
var check_keys=[true,false,false,true,true,true];
var train_types=["baserate","generative","preventative","noncausal"]
var check_ans=new Array(check_keys.length);
var insPage=0;
var checkFlag=0;
var trialNumber=0;
var onFor=500;
var clipLength=20;
var onColor="#FFD966";
var offColor="#D9D9D9";
var clickACount=0;
var clickBCount=0;
var connect_color=["#ededed","black","red"];
var connect_state=["N","G","P"];
var pracFlag=1;
var baserateFlag=0;
var gFlag=0;
var pFlag=0;
var nFlag=0;
var typeFlag=0;
var data=new Array;
var beginTime=0;
var myOrder=undefined;
var myPosition=0;
var myID=0;

document.getElementById("instruction").style.display="none";
document.getElementById("experiment").style.display="none";
document.getElementById("survey").style.display="none";
document.getElementById("experiment_finished").style.display="none";
document.getElementById("ins_backward").disabled=true;
document.getElementById("frame_hand_B").style.visibility = "hidden";
document.getElementById("frame_hand_A").style.visibility = "hidden";
//document.getElementById("clip_end").style.visibility="hidden"

function ConsentClick(){
	document.getElementById("instruction").style.display="block";
	document.getElementById("consent").style.display="none";
	for(var i=1;i<ins_pages.length;i++){
		document.getElementById(ins_pages[i]).style.display="none";
	}
}

function InsPrevClick(){
	for(var i=0;i<ins_pages.length;i++){
		document.getElementById(ins_pages[i]).style.display="none";
	}
	insPage=insPage-1;
	document.getElementById(ins_pages[insPage]).style.display="block";
	document.getElementById("ins_forward").disabled=false;
	document.getElementById("ins_backward").disabled=false;
	if (insPage==0){
		document.getElementById("ins_backward").disabled=true;
	}
}

function InsNextClick(){
	if (ins_pages[insPage]=="welldone"){ //begin the experiment
		ShowExp();
		return;
	}
	if (ins_pages[insPage]=="check"){
		CheckReturn();
		return;
	}
	for(var i=0;i<ins_pages.length;i++){
		document.getElementById(ins_pages[i]).style.display="none";
	}
	insPage=insPage+1;
	document.getElementById(ins_pages[insPage]).style.display="block";
	if (insPage>0){
		document.getElementById("ins_backward").disabled=false;
	}
	document.getElementById("ins_forward").disabled=false;
	document.getElementById("ins_backward").disabled=false;
	if (ins_pages[insPage]=="check" && checkFlag==0){
		document.getElementById("ins_forward").disabled=true;
	}
	if (ins_pages[insPage]=="ins_baserate" && baserateFlag==0){
		document.getElementById("ins_forward").disabled=true;
	}
	if (ins_pages[insPage]=="ins_type_generative" && (!gFlag)){
		document.getElementById("ins_forward").disabled=true;
	}
	if (ins_pages[insPage]=="ins_type_preventative" && (!pFlag)){
		document.getElementById("ins_forward").disabled=true;
	}
	if (ins_pages[insPage]=="ins_type_noncausal" && (!nFlag)){
		document.getElementById("ins_forward").disabled=true;
	}
}

function Training(train,trainLength){
	document.getElementById("button_training_"+train).disabled=true;
	document.getElementById("ins_forward").disabled=true;
	document.getElementById("ins_backward").disabled=true;

	document.getElementById("frame_training_"+train).innerHTML="<img class='example_img' src='static/images/"+train+".gif'>"
	setTimeout(TrainingEnd,trainLength*1000,train);
}

function TrainingEnd(train){
	document.getElementById("frame_training_"+train).innerHTML="";
	for (var i=0; i<train_types.length;i++){
		document.getElementById("button_training_"+train_types[i]).disabled=false;
	}
	if (train=="baserate"){
		baserateFlag=1
	}
	if (train=="generative"){
		gFlag=1;
	}else if (train=="preventative"){
		pFlag=1;
	}else if (train=="noncausal"){
		nFlag=1;
	}
	document.getElementById("ins_forward").disabled=false;
	document.getElementById("ins_backward").disabled=false;
}

function CheckClick(){
	checkCount=0
	for (var i=0;i<check_ans.length;i++){
		var p="check"+Number(i+1)+"_yes"
		var q="check"+Number(i+1)+"_no"
		if (document.getElementById(p).checked || document.getElementById(q).checked){
			checkCount=checkCount+1
		}
	}
	if (checkCount>=check_ans.length){
		checkFlag=1;
		document.getElementById("ins_forward").disabled=false;
	}
}

function CheckReturn(){
	for (var i=0;i<check_keys.length;i++){
		var p="check"+Number(i+1)+"_"+["no","yes"][Number(check_keys[i])]
		check_ans[i]=document.getElementById(p).checked
	}
	if (check_ans.reduce((a, b) => a + b)==check_keys.length){
		for(var i=0;i<ins_pages.length;i++){
			document.getElementById(ins_pages[i]).style.display="none";
		}
		insPage=insPage+2;
		document.getElementById("welldone").style.display="block";
		document.getElementById("ins_backward").disabled=true;
	}else{
		for(var i=0;i<ins_pages.length;i++){
			document.getElementById(ins_pages[i]).style.display="none";
		}
		insPage=insPage+1;
		document.getElementById("checkagain").style.display="block";
		document.getElementById("ins_forward").disabled=true;
	}
}

function ShowExp(){
	myID=Math.random().toString(36).substring(2, 11);
	while (1){
		myOrder=randperm(sti_list.length);
		if (myOrder.slice(1).map(function(n, i) {return n - myOrder[i];}).indexOf(1)==-1){
			//console.log(myOrder);
			break;
		}
	}
	myPosition=Number(Math.random()>0.5)
	// console.log(myPosition);
	// console.log(myID);
	if (myPosition) {
		document.getElementById("frame_button_A").style.marginTop="260px";
		document.getElementById("frame_button_B").style.marginTop="35px";

		document.getElementById("frame_line_A").style.marginTop="208px";
		document.getElementById("frame_line_B").style.marginTop="98px";

		document.getElementById("frame_line_A").style.transform = "rotate(-25deg)";
		document.getElementById("frame_line_B").style.transform = "rotate(25deg)";

	}
	//prepare a ramdon seed for each participant
	document.getElementById("instruction").style.display="none";
	document.getElementById("experiment").style.display="block";
	document.getElementById("hint").innerHTML="Click <b>Start</b> to watch the clip for this device."
}

function TrialStart(){
	document.getElementById("exp_start").disabled=true;
	document.getElementById("hint").innerHTML="Trial ongoing..."
	//document.getElementById('frame_line_A').style.pointerEvents = 'auto';
	//document.getElementById('frame_line_B').style.pointerEvents = 'auto';
	beginTime=Date.now();
	if (trialNumber==0){
		trial_sqc=prac;
	}else if (trialNumber>sti_list.length){
		trial_sqc=undetect;
	}else{
		trial_sqc=sti_list[myOrder[trialNumber-1]];
	}

	for (var i = 0; i <trial_sqc.time.length; i++) {
		setTimeout(TurnOnProcess,Number(trial_sqc.time[i])*1000,trial_sqc.obj[i]);
	}
	setTimeout(ClipEnd,clipLength*1000);
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
	//document.getElementById("clip_end").style.visibility="visible";
	document.getElementById("hint").innerHTML="Decide your answer and click <b>Next</b> to move onto the next device."
	document.getElementById("exp_next").disabled=false;
	//setTimeout(ClipEndHint,1000);
}

// function ClipEndHint(){
// 	document.getElementById("clip_end").style.visibility="hidden";
// }

function ClickA(){
	clickACount=clickACount+1;
	document.getElementById("arrow_line_A").style.background=connect_color[clickACount % 3];
	document.getElementById("arrow_point_A").style.borderLeftColor=connect_color[clickACount % 3];
	data[data.length]={sub_id:myID,
		position:myPosition,
		trial_order:trialNumber,
		trial_type:trial_sqc.type,
		trial_id:trial_sqc.id,
		A_pro:trial_sqc.a,
		B_pro:trial_sqc.b};
	data[data.length-1].begintime=beginTime;
	data[data.length-1].unixtime=Date.now();
	data[data.length-1].A_state=connect_state[clickACount % 3];
	data[data.length-1].B_state=connect_state[clickBCount % 3];
	data[data.length-1].data_type="online";
	data[data.length-1].acc=Number(connect_state[clickACount % 3]==trial_sqc.a && connect_state[clickBCount % 3]==trial_sqc.b);
	data[data.length-1].acc_a=Number(connect_state[clickACount % 3]==trial_sqc.a);
	data[data.length-1].acc_b=Number(connect_state[clickBCount % 3]==trial_sqc.b);
	data[data.length-1].acc_connection=Number(connect_state[clickACount % 3]==trial_sqc.a)+Number(connect_state[clickBCount % 3]==trial_sqc.b);
}

function ClickB(){
	clickBCount=clickBCount+1;
	document.getElementById("arrow_line_B").style.background=connect_color[clickBCount % 3];
	document.getElementById("arrow_point_B").style.borderLeftColor=connect_color[clickBCount % 3];
	data[data.length]={sub_id:myID,
		position:myPosition,
		trial_order:trialNumber,
		trial_type:trial_sqc.type,
		trial_id:trial_sqc.id,
		A_pro:trial_sqc.a,
		B_pro:trial_sqc.b};
	data[data.length-1].begintime=beginTime;
	data[data.length-1].unixtime=Date.now();
	data[data.length-1].A_state=connect_state[clickACount % 3];
	data[data.length-1].B_state=connect_state[clickBCount % 3];
	data[data.length-1].data_type="online";
	data[data.length-1].acc=Number(connect_state[clickACount % 3]==trial_sqc.a && connect_state[clickBCount % 3]==trial_sqc.b);
	data[data.length-1].acc_a=Number(connect_state[clickACount % 3]==trial_sqc.a);
	data[data.length-1].acc_b=Number(connect_state[clickBCount % 3]==trial_sqc.b);
	data[data.length-1].acc_connection=Number(connect_state[clickACount % 3]==trial_sqc.a)+Number(connect_state[clickBCount % 3]==trial_sqc.b);

}

function TrialNext(){
	//document.getElementById('frame_line_A').style.pointerEvents = 'none';
	//document.getElementById('frame_line_B').style.pointerEvents = 'none';
	if (trialNumber==0){
		if (pracFlag==1){
			PracFeedback();
			return;
		}else{
			document.getElementById("frame_line_A").style.background=connect_color[0];
			document.getElementById("frame_line_B").style.background=connect_color[0];
		}
	}
	DataRecord();
}

function PracFeedback(){
	if (myPosition){
		document.getElementById("hint").innerHTML="The correct answer of this device is <b>Generative (above)</b> and <b>Non-causal (below)</b>, click <b>Next</b> to move onto the formal experiment.There will be no feedback in the formal experiment." 
	}else{
		document.getElementById("hint").innerHTML="The correct answer of this device is <b>Non-causal (above)</b> and <b>Generative (below)</b>, click <b>Next</b> to move onto the formal experiment.There will be no feedback in the formal experiment." 
	}

	if (clickACount % 3 ==0){
		document.getElementById("frame_line_A").style.background="#9ee89f";
	}else{
		document.getElementById("frame_line_A").style.background="#e89e9e";
	}
	if (clickBCount % 3 ==1){
		document.getElementById("frame_line_B").style.background="#9ee89f";
	}else{
		document.getElementById("frame_line_B").style.background="#e89e9e";
	}
	pracFlag=0;
}

function DataRecord(){
	data[data.length]={sub_id:myID,
		position:myPosition,
		trial_order:trialNumber,
		trial_type:trial_sqc.type,
		trial_id:trial_sqc.id,
		A_pro:trial_sqc.a,
		B_pro:trial_sqc.b};
	data[data.length-1].begintime=beginTime;
	data[data.length-1].unixtime=Date.now();
	data[data.length-1].A_state=connect_state[clickACount % 3];
	data[data.length-1].B_state=connect_state[clickBCount % 3];
	data[data.length-1].data_type="final";
	data[data.length-1].acc=Number(connect_state[clickACount % 3]==trial_sqc.a && connect_state[clickBCount % 3]==trial_sqc.b);
	data[data.length-1].acc_a=Number(connect_state[clickACount % 3]==trial_sqc.a);
	data[data.length-1].acc_b=Number(connect_state[clickBCount % 3]==trial_sqc.b);
	data[data.length-1].acc_connection=Number(connect_state[clickACount % 3]==trial_sqc.a)+Number(connect_state[clickBCount % 3]==trial_sqc.b);
	ShowNextTrial();
	//console.log(data);
}

function ShowNextTrial(){
	if (trialNumber>=sti_list.length+1){//debug:>=sti_list.length+1
		document.getElementById("experiment").style.display="none";
		ShowSurvey();
		return;
	}
	clickACount=0;
	clickBCount=0;
	document.getElementById("arrow_line_A").style.background=connect_color[0];
	document.getElementById("arrow_point_A").style.borderLeftColor=connect_color[0];
	document.getElementById("arrow_line_B").style.background=connect_color[0];
	document.getElementById("arrow_point_B").style.borderLeftColor=connect_color[0];
	trialNumber=trialNumber+1;
	document.getElementById("progress_indicator").style.width=Number(45*(trialNumber+1))+"px";
	document.getElementById("trial_title").innerHTML="Device "+trialNumber+" of 19";
	document.getElementById("hint").innerHTML="click <b>Start</b> to watch the clip for this device, when you are ready,"
	document.getElementById("exp_next").disabled=true;
	document.getElementById("exp_start").disabled=false;
}


function ShowSurvey(){
	document.getElementById("experiment").style.display="none";
	document.getElementById("survey").style.display="block";
	document.getElementById("survey_submit").disabled=true;
}

function SurveyClick(){
	if (document.getElementById("gender_unselected").selected|document.getElementById("engaging_unselected").selected|document.getElementById("difficult_unselected").selected){
	}else{
		document.getElementById("survey_submit").disabled=false;
	}
}

function SaveData(){
	var subject=myID;
	var position=myPosition;
	var age=document.getElementById("age").value;
	var feedback=document.getElementById("text_feedback").value;

	var e = document.getElementById("gender");
	var gender=e.options[e.selectedIndex].value;
	var e = document.getElementById("engaging");
	var engaging=e.options[e.selectedIndex].value;
	var e = document.getElementById("difficult");
	var difficult=e.options[e.selectedIndex].value;
	
	var trial_order=data[0].trial_order;
	var trial_type=data[0].trial_type;
	var trial_id=data[0].trial_id;
	var A_pro=data[0].A_pro;
	var B_pro=data[0].B_pro;
	var begintime=data[0].begintime;
	var unixtime=data[0].unixtime;
	var A_state=data[0].A_state;
	var B_state=data[0].B_state;
	var data_type=data[0].data_type;

	for (var i = 1;i < data.length;i++){
		trial_order=trial_order+","+data[i].trial_order;
		trial_type=trial_type+","+data[i].trial_type;
		trial_id=trial_id+","+data[i].trial_id;
		A_pro=A_pro+","+data[i].A_pro;
		B_pro=B_pro+","+data[i].B_pro;
		begintime=begintime+","+data[i].begintime;
		unixtime=unixtime+","+data[i].unixtime;
		A_state=A_state+","+data[i].A_state;
		B_state=B_state+","+data[i].B_state;
		data_type=data_type+","+data[i].data_type;
	}


	jQuery.ajax({
		url: 'static/php/save_data.php',
		type:'POST',
		data:{subject:subject,
			position:position,
			age:age,
			feedback:feedback,
			gender:gender,
			engaging:engaging,
			difficult:difficult,
			trial_order:trial_order,//using trial_order for order 
			trial_type:trial_type,
			trial_id:trial_id,
			A_pro:A_pro,
			B_pro:B_pro,
			begintime:begintime,
			unixtime:unixtime,
			A_state:A_state,
			B_state:B_state,
			data_type:data_type},
		success:function(data)
		{
			console.log('Sent data to database');
			FinishExp();
		},
		error:function(xhr, status, error)
		{
			//Just print out what comes back if it doesn't work
			console.log(xhr, status, error);
			FinishError();
		}
	})
}


function FinishExp(){
	document.getElementById("survey").style.display="none";
	document.getElementById("experiment_finished").style.display="block";
	document.getElementById("give_ID").innerHTML="Your reference code is:  "+myID;
	document.getElementById("finish_info").innerHTML="Please write your reference code in Mturk and then close this window. Thanks so much!"
}

function FinishError(){
	document.getElementById("give_ID").innerHTML="Your reference code is:  "+myID;
	document.getElementById("finish_info").innerHTML="<span style='color: red'>Error: the data might not upload to database successfully. Please contact the experimenter.</span>"
}

function randperm(maxValue){
	// first generate number sequence
	var permArray = new Array(maxValue);
	for(var i = 0; i < maxValue; i++){
	   permArray[i] = i;
	}
	// draw out of the number sequence
	for (var i = (maxValue - 1); i >= 0; --i){
	   var randPos = Math.floor(i * Math.random());
	   var tmpStore = permArray[i];
	   permArray[i] = permArray[randPos];
	   permArray[randPos] = tmpStore;
	}
	return permArray;
}

// For offline running
// download(JSON.stringify(data), 'data.txt', '"text/csv"');
// function download(content, fileName, contentType) {
//   var a = document.createElement("a");
//   var file = new Blob([content], {type: contentType});
//   a.href = URL.createObjectURL(file);
//   a.download = fileName;
//   a.click();
// }