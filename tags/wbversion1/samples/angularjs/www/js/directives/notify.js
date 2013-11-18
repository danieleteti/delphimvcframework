"use strict";

app.directive("bootstrapNotify", function() {
    return {
        restrict: "E",
        replace: true,
        transclude: true,
        templateUrl: "js/directives/notify.html",
        scope: {
            viewLogin: '=',
            username: '=username' // set the name on the directive's scope,
//      password: '=password'
            // to the name attribute on the directive element.
        },
        link: function(scope, elem, attr) {
            // scope is the directive's scope,
            // elem is a jquery lite (or jquery full) object for the directive root element.
            // attr is a dictionary of attributes on the directive element.            
        },
        //the controller for the directive
        controller: function($scope, $routeParams) {

        },
        compile: function(scope, element, attrs) {


        }
    };
});


