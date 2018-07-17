// The root URL for the RESTful services
var rootURL = "/dmvc/wines";

var currentWine;

$(document).ready(function () {
	//pupulate years combo
	for (i = new Date().getFullYear(); i > 1940; i--) {
		$('#year').append($('<option />').val(i).html(i));
	}

	// Retrieve wine list when application starts
	findAll();
	// Nothing to delete in initial application state
	$('#btnDelete').hide();

	// Register listeners
	$('#btnSearch').click(function () {
		search($('#searchKey').val());
		return false;
	});

	// Trigger search when pressing 'Return' on search key input field
	$('#searchKey').keypress(function (e) {
		if (e.which == 13) {
			search($('#searchKey').val());
			e.preventDefault();
			return false;
		}
	});

	$('#btnAdd').click(function () {
		newWine();
		return false;
	});

	$('#btnSave').click(function () {
		if ($('#wineId').val() == '') {
			addWine();
		}
		else
			updateWine();
		return false;
	});

	$('#btnDelete').click(function () {
		deleteWine();
		return false;
	});

	$('#wineList a').live('click', function () {
		findById($(this).data('identity'));
	});

	// Replace broken images with generic wine bottle
	$("img").error(function () {
		$(this).attr("src", "pics/generic.jpg");

	});

});


function search(searchKey) {
	if (searchKey == '')
		findAll();
	else
		findByName(searchKey);
}

function newWine() {
	$('#btnDelete').hide();
	currentWine = {};
	renderDetails(currentWine); // Display empty form
}

function findAll() {
	console.log('findAll');
	$.ajax({
		type: 'GET',
		url: rootURL,
		dataType: "json", // data type of response
		success: renderList
	});
}

function findByName(searchKey) {
	console.log('findByName: ' + searchKey);
	$.ajax({
		type: 'GET',
		url: rootURL + '/search/' + searchKey,
		dataType: "json",
		success: renderList
	});
}

function findById(id) {
	console.log('findById: ' + id);
	$.ajax({
		type: 'GET',
		url: rootURL + '/' + id,
		dataType: "json",
		success: function (data) {
			$('#btnDelete').show();
			if (data instanceof Array) {
				alert('ERROR! Expected "object" got "array"');
				return;
			}
			console.log('findById success: ' + data.name);
			currentWine = data;
			renderDetails(currentWine);
		},
		error: function (jqXHR, textStatus, errorThrown) {
			alert('findById error: ' + textStatus);
		}
	});
}

function addWine() {
	console.log('addWine');
	$.ajax({
		type: 'POST',
		contentType: 'application/json',
		url: rootURL,
		dataType: "json",
		data: formToJSON(),
		success: function (data, textStatus, jqXHR) {
			alert('Wine created successfully');
			$('#btnDelete').show();
			$('#wineId').val(data.id);
			findById(data.id);
		},
		error: function (jqXHR, textStatus, errorThrown) {
			alert('addWine error: ' + textStatus);
		}
	});
}

function updateWine() {
	console.log('updateWine');
	$.ajax({
		type: 'PUT',
		contentType: 'application/json',
		url: rootURL + '/' + $('#wineId').val(),
		dataType: "json",
		data: formToJSON(),
		processData: false,
		success: function (data, textStatus, jqXHR) {
			debugger;
			alert('Wine updated successfully');
		},
		error: function (jqXHR, textStatus, errorThrown) {
			alert('updateWine error: ' + textStatus);
		}
	});
}

function deleteWine() {
	console.log('deleteWine');
	$.ajax({
		type: 'DELETE',
		url: rootURL + '/' + $('#wineId').val(),
		success: function (data, textStatus, jqXHR) {
			alert('Wine deleted successfully');
			findAll();
		},
		error: function (jqXHR, textStatus, errorThrown) {
			alert('deleteWine error');
		}
	});
}

function renderList(data) {
	// JAX-RS serializes an empty list as null, and a 'collection of one' as an object (not an 'array of one')
	//var list = data == null ? [] : (data.wine instanceof Array ? data.wine : [data.wine]);
	//debugger;
	var list = data;
	$('#wineList li').remove();
	$.each(list, function (index, wine) {
		$('#wineList').append('<li><a href="#" data-identity="' + wine.id + '">' + wine.name + '</a></li>');
	});
}

function renderDetails(wine) {
	console.log("rendering details for wine: ", wine);
	$('#wineId').val(wine.id);
	$('#name').val(wine.name);
	$('#grapes').val(wine.grapes);
	$('#country').val(wine.country);
	$('#region').val(wine.region);
	$('#year').val(wine.year);
	$('#pic').attr('src', 'pics/' + wine.picture);
	$('#description').val(wine.description);
}

// Helper function to serialize all the form fields into a JSON string
function formToJSON() {
	return JSON.stringify({
		"id": $('#wineId').val(),
		"name": $('#name').val(),
		"grapes": $('#grapes').val(),
		"country": $('#country').val(),
		"region": $('#region').val(),
		"year": $('#year').val(),
		"picture": currentWine.picture,
		"description": $('#description').val()
	});
}
