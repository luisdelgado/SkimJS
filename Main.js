//Programa 1 - If e Comentário

var a = 1;
if (a == 1) {
	var b = 1;
}


//Programa 2 - If-else com If dentro de If
/*
var a = 1;
if (a == 1) {
	if (a == 1) {
		var b = 1;
	};
} else {
	var c = 1;
}
*/

//Programa 3 - If-else com If dentro de else
/*
var a = 1;
if (a == 0) {
	var b = 1;
} else {	
	if (a == 1) {
		var c = 1;
	};
}
*/

//Programa 4 - For normal e Break
/*
var a = 1;
for (var b = 0; b < a; b = b+1) {
	var c = 1;
	break;
	var d = 1;
}
*/

//Programa 5 - For sem declaração e sem incremento
/*
var a = 1;
var b = 0;
for (; b < a;) {
	var c = 1;
	var b = b+1;
}
*/

//Programa 6 - For sem verificação e com Break
/*
var a = 1;
for (var b = 0; ; b = b+1) {
	var c = 1;
	break;
	var d = 1;
}
*/

//Programa 7 - If interno ao For e com declaração de variável automaticamente global e incremento i++ 
/*
var a = 5;
for (var i = 2; i <= a; i++) {
	if(i == 5){
		b = 100;
	}
};
if (b == 100) {
	a = 200;
};
*/

// Programa 8 - IF e Else com for dentro do Else, decremento i-- e declaração de váriavei automaticamente globais 

var a = 3;
if (a > 4) {
	break;
}else{
	b = 1;
	for (var i = 5; i >= b; i--) {
		var c = i;
	};
}
