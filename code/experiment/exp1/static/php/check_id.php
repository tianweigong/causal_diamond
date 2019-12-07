<?php
$mysqli = mysqli_connect("chost4.is.ed.ac.uk","wwwbramleylabppl_neil","M2B(BKwEYa5.RXQ9", "wwwbramleylabppl_expdata");
if (mysqli_connect_errno($mysqli)) {
    echo "Failed to connect to MySQL: " . mysqli_connect_error();
}

$ip_address = gethostbyname($_SERVER['REMOTE_ADDR']);
// $ip_address = "18.111.62.18";
//slashes added for escaping
$ip_address = addslashes($ip_address);

$query = "(SELECT 'ip' FROM tia_prevent_pilot WHERE tia_prevent_pilot.ip ='$ip_address')";
//$query = "(SELECT 'ip' FROM neil_time2 WHERE neil_time2.ip ='$ip_address')";

$result = mysqli_query($mysqli, $query) or die ("Something went wrong!");

$num_result = mysqli_num_rows($result);

if ($num_result > 0){
	// echo("You've done this before") ;
	echo(1);
}else{
	// echo("you can do it!");
	echo(0);
}
?>

