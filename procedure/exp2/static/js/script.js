var ins_pages=["ins_pre","ins1","ins_baserate","ins_baserate_over","ins_type_generative","ins_type_preventative","ins_type_noncausal","bonus_reminder","check","checkagain","welldone"];
var check_keys=[true,false,true,true,true,true,false];
var train_types=["baserate","generative","preventative","noncausal"]
var check_ans=new Array(check_keys.length);
var insPage=0;
var checkFlag=0;
var trialNumber=0;//begin with click training(0), and then no practice trial(0)
var onFor=350;
var clipLength=20+0.5;
var onColor="#FFD966";
var offColor="#D9D9D9";
var clickACount=0;
var clickBCount=0;
var connect_color=["#ededed","black","red"];
var connect_hint_color=["#ababab","black","red"];
var connect_state=["N","G","P"];
var connect_name=["Non-causal","Generative","Preventative"];
var pracFlag=1;
var baserateFlag=0;
var gFlag=0;
var pFlag=0;
var nFlag=0;
var typeFlag=0;
var endFlag=0;
var trainFlag=1;
var data=new Array;
var beginTime=0;
var myOrder=undefined;
var myPosition=undefined;
var myABI=Math.random().toString(36).substring(2, 4)+["a","6"][Number(Math.random()>0.5)]+Math.random().toString(36).substring(2, 6)+["p","m"][Number(Math.random()>0.5)]+Math.random().toString(36).substring(2, 3);


var cnumber=0;
var anumber=0;

var connect_array=[0,1,2];
var trial_array=[1,2,3,4,5,6,
				7,8,9,10,11,12,
				13,14];
var myClick=shuffle(connect_array);

document.getElementById("instruction").style.display="none";
document.getElementById("experiment").style.display="none";
document.getElementById("survey").style.display="none";
document.getElementById("experiment_finished").style.display="none";
document.getElementById("ins_backward").disabled=true;
document.getElementById("frame_hand_B").style.visibility = "hidden";
document.getElementById("frame_hand_A").style.visibility = "hidden";
//document.getElementById("clip_end").style.visibility="hidden"

// ipcheck();

function ConsentClick(){
	document.getElementById("instruction").style.display="block";
	document.getElementById("consent").style.display="none";
	for(var i=1;i<ins_pages.length;i++){
		document.getElementById(ins_pages[i]).style.display="none";
	}
}

function ConsentFinish(){
	if (document.getElementById("proid").value.length){
		document.getElementById("i_agree").disabled=false;
	}else{
		document.getElementById("i_agree").disabled=true;
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
	// if (ins_pages[insPage]=="ins_baserate" && baserateFlag==0){
	// 	document.getElementById("ins_forward").disabled=true;
	// }
	// if (ins_pages[insPage]=="ins_type_generative" && (!gFlag)){
	// 	document.getElementById("ins_forward").disabled=true;
	// }
	// if (ins_pages[insPage]=="ins_type_preventative" && (!pFlag)){
	// 	document.getElementById("ins_forward").disabled=true;
	// }
	// if (ins_pages[insPage]=="ins_type_noncausal" && (!nFlag)){
	// 	document.getElementById("ins_forward").disabled=true;
	// }
}

function Training(train,trainLength){
	document.getElementById("button_training_"+train).disabled=true;
	document.getElementById("ins_forward").disabled=true;
	document.getElementById("ins_backward").disabled=true;

	document.getElementById("frame_training_"+train).innerHTML="<p align='center'><img class='example_img' src='static/images/"+train+".gif'></p>"
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
		cnumber=cnumber+1;
		if (!check_ans[6]){
			anumber=anumber+1
		}
		for(var i=0;i<ins_pages.length;i++){
			document.getElementById(ins_pages[i]).style.display="none";
		}
		insPage=insPage+1;
		document.getElementById("checkagain").style.display="block";
		document.getElementById("ins_forward").disabled=true;
	}
}

function ShowExp(){
	// while (1){
		// myOrder=shuffle([1,2,3,4,5,6,7,8,9,10,11,12]);
		// if (myOrder.slice(1).map(function(n, i) {return n - myOrder[i];}).indexOf(1)==-1){
		// 	if (myOrder.slice(1).map(function(n, i) {return n - myOrder[i];}).indexOf(-1)==-1){
				// console.log(myOrder);
				// break;
		// 	}
		// }
	// }
	myOrder=shuffle([1,2,3,4,5,6]).concat(shuffle([7,8,9,10,11,12,13,14]));
	console.log(myOrder);
	myPosition=shuffle([0,0,0,0,0,0,0,1,1,1,1,1,1,1]);
	//prepare a ramdon seed for each participant
	document.getElementById("instruction").style.display="none";
	document.getElementById("experiment").style.display="block";
	document.getElementById("hint").innerHTML="Click <b>Start</b> to watch the clip for this device."
	ClickTraining()
}

function ClickTraining(){
	document.getElementById("exp_start").disabled=true;
	document.getElementById("arrow_hint_A").innerHTML="<span style='font-size:15px;font-weight:bold;color:black'>Click Here</span>"
	document.getElementById("arrow_hint_B").innerHTML="<span style='font-size:15px;font-weight:bold;color:black'>Click Here</span>"
	document.getElementById("frame_progress").style.display="none";
	document.getElementById("frame_reminder").style.display="none";
	document.getElementById("hint").innerHTML="Please click each connection more than 6 times to continue."
}

function TrialStart(){
	document.getElementById("exp_start").disabled=true;
	document.getElementById("hint").innerHTML="Trial ongoing..."
	document.getElementById("arrow_hint_A").innerHTML="<span style='font-size:30px;font-weight:bold'>?</span>"
	document.getElementById("arrow_hint_B").innerHTML="<span style='font-size:30px;font-weight:bold'>?</span>"
	document.getElementById('frame_line_A').style.pointerEvents = 'auto';
	document.getElementById('frame_line_B').style.pointerEvents = 'auto';
	beginTime=Date.now();
	console.log(myOrder[trialNumber-1])
	if (trialNumber==0){
		trial_sqc=prac;
	}else{
		trial_sqc=sti_list[myOrder[trialNumber-1]-1];
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
	document.getElementById("hint").innerHTML="<b>Decide your answer</b> by clicking on two connections and click <b>Next</b> to move onto the next device."
	//setTimeout(ClipEndHint,1000);
	endFlag=1;
	if (clickACount && clickBCount){
		document.getElementById("exp_next").disabled=false;
	}
}

// function ClipEndHint(){
// 	document.getElementById("clip_end").style.visibility="hidden";
// }

function ClickA(){
	clickACount=clickACount+1;

	if (endFlag==1 && clickBCount){
		document.getElementById("exp_next").disabled=false;
	}

	document.getElementById("arrow_line_A").style.background=connect_color[myClick[clickACount % 3]];
	document.getElementById("arrow_point_A").style.borderLeftColor=connect_color[myClick[clickACount % 3]];
	document.getElementById("arrow_hint_A").innerHTML=connect_name[myClick[clickACount % 3]];
	document.getElementById("arrow_hint_A").style.color=connect_hint_color[myClick[clickACount % 3]];
	
	if (trainFlag){
		if (clickACount>=6 & clickBCount>=6){
			document.getElementById("exp_next").disabled=false;
		}
		return;
	}

	data[data.length]={
		position:myPosition,
		trial_order:trialNumber,
		trial_type:trial_sqc.type,
		trial_id:trial_sqc.id,
		A_pro:trial_sqc.a,
		B_pro:trial_sqc.b};
	data[data.length-1].begintime=beginTime;
	data[data.length-1].unixtime=Date.now();
	data[data.length-1].A_state=connect_state[myClick[clickACount % 3]];
	if (clickBCount){
		data[data.length-1].B_state=connect_state[myClick[clickBCount % 3]];
	}else{
		data[data.length-1].B_state="T";
	}
	data[data.length-1].data_type="online";
	data[data.length-1].acc=Number(connect_state[myClick[clickACount % 3]]==trial_sqc.a && connect_state[myClick[clickBCount % 3]]==trial_sqc.b);
	data[data.length-1].acc_a=Number(connect_state[myClick[clickACount % 3]]==trial_sqc.a);
	data[data.length-1].acc_b=Number(connect_state[myClick[clickBCount % 3]]==trial_sqc.b);
	data[data.length-1].acc_connection=Number(connect_state[myClick[clickACount % 3]]==trial_sqc.a)+Number(connect_state[myClick[clickBCount % 3]]==trial_sqc.b);
}

function ClickB(){
	clickBCount=clickBCount+1;

	if (endFlag==1 && clickACount){
		document.getElementById("exp_next").disabled=false;
	}

	document.getElementById("arrow_line_B").style.background=connect_color[myClick[clickBCount % 3]];
	document.getElementById("arrow_point_B").style.borderLeftColor=connect_color[myClick[clickBCount % 3]];
	document.getElementById("arrow_hint_B").innerHTML=connect_name[myClick[clickBCount % 3]];
	document.getElementById("arrow_hint_B").style.color=connect_hint_color[myClick[clickBCount % 3]];
	
	if (trainFlag){
		if (clickACount>=6 & clickBCount>=6){
			document.getElementById("exp_next").disabled=false;
		}
		return;
	}

	data[data.length]={
		position:myPosition,
		trial_order:trialNumber,
		trial_type:trial_sqc.type,
		trial_id:trial_sqc.id,
		A_pro:trial_sqc.a,
		B_pro:trial_sqc.b};
	data[data.length-1].begintime=beginTime;
	data[data.length-1].unixtime=Date.now();
	if (clickACount){
		data[data.length-1].A_state=connect_state[myClick[clickACount % 3]];
	}else{
		data[data.length-1].A_state="T";
	}
	data[data.length-1].B_state=connect_state[myClick[clickBCount % 3]];
	data[data.length-1].data_type="online";
	data[data.length-1].acc=Number(connect_state[myClick[clickACount % 3]]==trial_sqc.a && connect_state[myClick[clickBCount % 3]]==trial_sqc.b);
	data[data.length-1].acc_a=Number(connect_state[myClick[clickACount % 3]]==trial_sqc.a);
	data[data.length-1].acc_b=Number(connect_state[myClick[clickBCount % 3]]==trial_sqc.b);
	data[data.length-1].acc_connection=Number(connect_state[myClick[clickACount % 3]]==trial_sqc.a)+Number(connect_state[myClick[clickBCount % 3]]==trial_sqc.b);

}

function TrialNext(){
	if (myPosition[trialNumber]) {
		document.getElementById("frame_button_A").style.marginTop="260px";
		document.getElementById("frame_button_B").style.marginTop="35px";

		document.getElementById("frame_line_A").style.marginTop="208px";
		document.getElementById("frame_line_B").style.marginTop="98px";

		document.getElementById("frame_line_A").style.transform = "rotate(-25deg)";
		document.getElementById("frame_line_B").style.transform = "rotate(25deg)";

	}else{
		document.getElementById("frame_button_B").style.marginTop="260px";
		document.getElementById("frame_button_A").style.marginTop="35px";

		document.getElementById("frame_line_B").style.marginTop="208px";
		document.getElementById("frame_line_A").style.marginTop="98px";

		document.getElementById("frame_line_B").style.transform = "rotate(-25deg)";
		document.getElementById("frame_line_A").style.transform = "rotate(25deg)";
	}

	document.getElementById('frame_line_A').style.pointerEvents = 'none';
	document.getElementById('frame_line_B').style.pointerEvents = 'none';

	if (trainFlag){
		ShowNextTrial();
		document.getElementById("exp_start").disabled=false;
		document.getElementById("frame_progress").style.display="block";
		document.getElementById("frame_reminder").style.display="block";
		document.getElementById("click_training").style.display="none";
		trainFlag=0;
		return;
	}

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
		document.getElementById("hint").innerHTML="The correct answer of this device is <b>Generative (above)</b> and <b>Non-causal (below)</b>.  Click <b>Next</b> to move onto the formal experiment when you are ready.<b>There will be no feedback in the formal experiment</b>." 
	}else{
		document.getElementById("hint").innerHTML="The correct answer of this device is <b>Non-causal (above)</b> and <b>Generative (below)</b>.  Click <b>Next</b> to move onto the formal experiment when you are ready.<b>There will be no feedback in the formal experiment</b>." 
	}

	if (myClick[clickACount % 3] ==0){
		document.getElementById("frame_line_A").style.background="#9ee89f";
	}else{
		document.getElementById("frame_line_A").style.background="#e89e9e";
	}
	if (myClick[clickBCount % 3] ==1){
		document.getElementById("frame_line_B").style.background="#9ee89f";
	}else{
		document.getElementById("frame_line_B").style.background="#e89e9e";
	}
	pracFlag=0;
}

function DataRecord(){
	data[data.length]={
		trial_order:trialNumber,
		trial_type:trial_sqc.type,
		trial_id:trial_sqc.id,
		A_pro:trial_sqc.a,
		B_pro:trial_sqc.b};
	data[data.length-1].begintime=beginTime;
	data[data.length-1].unixtime=Date.now();
	data[data.length-1].A_state=connect_state[myClick[clickACount % 3]];
	data[data.length-1].B_state=connect_state[myClick[clickBCount % 3]];
	data[data.length-1].data_type="final";
	data[data.length-1].acc=Number(connect_state[myClick[clickACount % 3]]==trial_sqc.a && connect_state[myClick[clickBCount % 3]]==trial_sqc.b);
	data[data.length-1].acc_a=Number(connect_state[myClick[clickACount % 3]]==trial_sqc.a);
	data[data.length-1].acc_b=Number(connect_state[myClick[clickBCount % 3]]==trial_sqc.b);
	data[data.length-1].acc_connection=Number(connect_state[myClick[clickACount % 3]]==trial_sqc.a)+Number(connect_state[myClick[clickBCount % 3]]==trial_sqc.b);
	ShowNextTrial();
}

function ShowNextTrial(){
	if (trialNumber>=sti_list.length){//debug:>=sti_list.length+1
		document.getElementById("experiment").style.display="none";
		ShowSurvey();
		return;
	}
	clickACount=0;
	clickBCount=0;
	endFlag=0;
	document.getElementById("arrow_hint_A").innerHTML="";
	document.getElementById("arrow_hint_B").innerHTML="";
	document.getElementById("arrow_hint_A").style.color="black";
	document.getElementById("arrow_hint_B").style.color="black";
	document.getElementById("arrow_line_A").style.background=connect_color[0];
	document.getElementById("arrow_point_A").style.borderLeftColor=connect_color[0];
	document.getElementById("arrow_line_B").style.background=connect_color[0];
	document.getElementById("arrow_point_B").style.borderLeftColor=connect_color[0];
	document.getElementById("exp_next").disabled=true;
	document.getElementById("exp_start").disabled=false;
	document.getElementById("hint").innerHTML="When you are ready, click <b>Start</b> to watch the clip for this device &#x1f44d"
	trialNumber=trialNumber+1;
	document.getElementById("progress_indicator").style.width=Number((855/14)*(trialNumber-1))+"px";
	if (trialNumber==0){
		document.getElementById("trial_title").innerHTML="Device "+trialNumber+" of 14 (practice)";
	}else{
		document.getElementById("trial_title").innerHTML="Device "+trialNumber+" of 14";
	}	
}


function ShowSurvey(){
	document.getElementById("experiment").style.display="none";
	document.getElementById("survey").style.display="block";
	document.getElementById("survey_submit").disabled=true;
}

function SurveyClick(){
	if (document.getElementById("gender_unselected").selected|document.getElementById("engaging_unselected").selected|document.getElementById("difficult_unselected").selected|document.getElementById("acheck_unselected").selected|document.getElementById("age").value==""){
	}else{
		document.getElementById("survey_submit").disabled=false;
	}
}

function SaveData(){
	var subject=myABI+"_"+document.getElementById("proid").value;
	var position=myPosition.toString();
	var clickpattern=JSON.stringify(connect_state[myClick[1]]+connect_state[myClick[2]]+connect_state[myClick[0]]);
	var exp_id="eco";
	var age=document.getElementById("age").value;
	var feedback=document.getElementById("text_feedback").value;
	feedback=feedback.replace(/'/g,"\\'");
	feedback=feedback.replace(/"/g, '\\"');

	var mycondition=con_list_num
	var mystiset=sti_list_num

	var e = document.getElementById("gender");
	var gender=e.options[e.selectedIndex].value;
	var e = document.getElementById("engaging");
	var engaging=e.options[e.selectedIndex].value;
	var e = document.getElementById("difficult");
	var difficult=e.options[e.selectedIndex].value;

	var e = document.getElementById("acheck");
	var acheck=JSON.stringify([cnumber,anumber,e.options[e.selectedIndex].value]);

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

	// var a = document.createElement("a");
	// var file = new Blob([JSON.stringify({subject:subject,
	// 		position:position,
	// 		clickpattern:clickpattern,
	// 		exp_id:exp_id,
	// 		mycondition:mycondition,
	// 		mystiset:mystiset,
	// 		age:age,
	// 		feedback:feedback,
	// 		gender:gender,
	// 		engaging:engaging,
	// 		difficult:difficult,
	// 		acheck:acheck,
	// 		trial_order:trial_order,//using trial_order for order 
	// 		trial_type:trial_type,
	// 		trial_id:trial_id,
	// 		A_pro:A_pro,
	// 		B_pro:B_pro,
	// 		begintime:begintime,
	// 		unixtime:unixtime,
	// 		A_state:A_state,
	// 		B_state:B_state,
	// 		data_type:data_type})], 
	// 	{type: "application/json"});
	// a.href = URL.createObjectURL(file);
	// a.download = 'Inference.json';
	// a.click();



	// jQuery.ajax({
	// 	url: 'static/php/save_data.php',
	// 	type:'POST',
	// 	data:{subject:subject,
	// 		position:position,
	// 		clickpattern:clickpattern,
	// 		exp_id:exp_id,
	// 		mycondition:mycondition,
	// 		mystiset:mystiset,
	// 		age:age,
	// 		feedback:feedback,
	// 		gender:gender,
	// 		engaging:engaging,
	// 		difficult:difficult,
	// 		acheck:acheck,
	// 		trial_order:trial_order,//using trial_order for order 
	// 		trial_type:trial_type,
	// 		trial_id:trial_id,
	// 		A_pro:A_pro,
	// 		B_pro:B_pro,
	// 		begintime:begintime,
	// 		unixtime:unixtime,
	// 		A_state:A_state,
	// 		B_state:B_state,
	// 		data_type:data_type},
	// 	success:function(data)
	// 	{
	// 		console.log('Sent data to database');
	// 		FinishExp();
	// 	},
	// 	error:function(xhr, status, error)
	// 	{
	// 		//Just print out what comes back if it doesn't work
	// 		console.log(xhr, status, error);
	// 		FinishError();

	// 	}
	// })
}


function FinishExp(){
	document.getElementById("survey").style.display="none";
	document.getElementById("experiment_finished").style.display="block";
	document.getElementById("give_ID").innerHTML="Your finish code is:  C13PGXX0";
	document.getElementById("finish_info").innerHTML="Please write your finish code in Prolific and then close this window. Thanks so much!"
}

function FinishError(){
	document.getElementById("give_ID").innerHTML="Your reference code is:  "+myABI;
	document.getElementById("finish_info").innerHTML="<span style='color: red'>Error: the data might not upload to database successfully. Please contact the experimenter.</span>"
}

function shuffle(a) {
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}


function ipcheck () {
	console.log('test');

	jQuery.ajax({
		url: 'static/php/check_id.php',
		type:'POST',
		success:function(data)
		{
			if (data==1)
			{
				Oops();
			} else if (data==0)
			{
			} else {
				alert('answer was not 1 or 0!');
			}
			
		},
		error:function()
		{
			alert('failed to connect to ip')
		}
		//var result_str:String = event.target.data.toString();
 		//id_check = Number(result_str);		
 		//trace(id_check);

	})
}

function Oops(){
	document.getElementById('frame_consent').innerHTML="Unfortunately, you cannot do this experiment because you (or someone in your household) have participated in this experiment or a similar experiment before. Please close the window. Thank you!"
	document.getElementById('i_agree').style.display="none";
	document.getElementById('title_consent').innerHTML="Oops:("
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