<?php

	// Create a database connection
	$mysqli = mysqli_connect("chost4.is.ed.ac.uk","wwwbramleylabppl_neil","M2B(BKwEYa5.RXQ9", "wwwbramleylabppl_expdata");

	if (mysqli_connect_errno($mysqli)) {
		echo "Failed to connect to MySQL: " . mysqli_connect_error();
	}

	// Get values passed from JS
	$ip = $_SERVER['REMOTE_ADDR'];
	$date = date('Y-m-d');
	$subject = $_POST['subject'];
	$position = $_POST['position'];
	$clickpattern=$_POST['clickpattern'];
	$exp_id=$_POST['exp_id'];
	$gender=$_POST['gender'];
	$age = $_POST['age'];
	$engaging=$_POST['engaging'];
	$difficult=$_POST['difficult'];
	$feedback = $_POST['feedback'];
	$trial_order=$_POST['trial_order'];
 	$trial_type=$_POST['trial_type'];
	$trial_id=$_POST['trial_id'];
	$A_pro=$_POST['A_pro'];
	$B_pro=$_POST['B_pro'];
	$begintime=$_POST['begintime'];
	$unixtime=$_POST['unixtime'];
	$A_state=$_POST['A_state'];
	$B_state=$_POST['B_state'];
	$data_type=$_POST['data_type'];

	//Create a query

	$query = "INSERT INTO tia_prevent_pilot (ip, date, subject,position,clickpattern,exp_id,gender,age,engaging,difficult,feedback,trial_order,trial_type,trial_id,A_pro,B_pro,begintime,unixtime,A_state,B_state,data_type) VALUES ('{$ip}', '{$date}','{$subject}','{$position}','{$clickpattern}','{$exp_id}','{$gender}','{$age}','{$engaging}','{$difficult}','{$feedback}','{$trial_order}','{$trial_type}','{$trial_id}','{$A_pro}','{$B_pro}','{$begintime}','{$unixtime}','{$A_state}','{$B_state}','{$data_type}')";
	
	//Do it
	mysqli_query($mysqli, $query);

	//Close connection
	mysqli_close($mysqli);

?>