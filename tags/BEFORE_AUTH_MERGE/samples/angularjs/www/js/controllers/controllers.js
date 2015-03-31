'use strict';

/* Controllers */



app.controller('HomeCtrl', function($scope,NotifyModel, PXTODO) {

PXTODO.get(function(data){
    $scope.list = data.todos;
},function(error){
    NotifyModel.showMessageError({message:"error"});
});


$scope.edit = function(todo){
    $scope.viewedit = true;
    $scope.viewadd = false;
    $scope.obj_todo = angular.copy(todo);
}

$scope.update = function(todo) {
    $scope.call(todo, 0, "Updated");
		$scope.viewedit = false;
}

$scope.delete = function(todo) {
    $scope.call(todo, 3, "Deleted");
}

$scope.add = function() {
    $scope.description = "";
    $scope.viewadd = true;
    $scope.viewedit = false;
}

$scope.insert = function() {
    var obj = {};
    obj.todos = [{description:$scope.description,
                  objstatus:0,
                  datetime: new Date()}];

    var p = PXTODO.put(obj,function(data){
        p.$get(function(data){
            $scope.list = data.todos;
						$scope.viewadd = false;
						$scope.viewedit = false;
        });
        NotifyModel.showMessageSuccess({message:"Added"});
    },function(error){
        NotifyModel.showMessageError({message:"error"});
    });
}

$scope.call = function(todo, statusobj, message) {
    todo.objstatus = statusobj;
    var obj = {};
    obj.todos = [todo];

    var p = PXTODO.put(obj,function(data){
        p.$get(function(data){
            $scope.list = data.todos;
        });
        NotifyModel.showMessageSuccess({message:message});
    },function(error){
        NotifyModel.showMessageError({message:"error"});
    });
};


});

