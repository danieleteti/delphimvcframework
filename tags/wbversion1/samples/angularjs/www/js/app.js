'use strict';


var myAppConfig = function($routeProvider) {
    $routeProvider.when('/home', {templateUrl: 'partials/home.html', controller: 'HomeCtrl'})
                  .otherwise({redirectTo: '/home'});
};
var app = angular.module('myApp',['ngResource']).config(myAppConfig);


