let lock = false;

check_buttons = function(){
	//console.log("Checking buttons...");
	var buttons = document.getElementsByTagName('button');
	for (button of buttons){
		button.addEventListener("click", button_disabler);
	}

	buttons = document.getElementsByTagName('button');

	for (button of buttons){
		if (lock){
			button.disabled = true;
		} else {
			button.disabled = false;
		}
	}
}

button_disabler = function(){
	lock = true;
	setTimeout(function() {
		console.log("Disabling lock");
		lock = false;
	}, 1000);
}

$(document).ready(function() {
	console.log("Starting button checking...")
	setInterval(check_buttons, 5);
});
