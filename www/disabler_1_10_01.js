let lock = false;

get_buttons = function(){
	var btns = []
	for (let button of document.getElementsByTagName('button')){
		if  (["backBtn", "nextBtn", "confBtn"].includes(button.id)){
			btns.push(button);
		}
	}
	return btns;
}

check_buttons = function(){
	//console.log("Checking buttons...");
	buttons = get_buttons();
	for (let button of buttons){
		button.addEventListener("click", button_disabler);
	}

	buttons = get_buttons();
	for (let button of buttons){
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
