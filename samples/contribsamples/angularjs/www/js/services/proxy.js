//PROXY RESTFULL

//{ 'get':    {method:'GET'},
//  'save':   {method:'POST'},
//  'query':  {method:'GET', isArray:true},
//  'remove': {method:'DELETE'},
//  'delete': {method:'DELETE'} };


/**
 * GET
 * POST (insert new movie)
 * PUT (update movie ) @id params
 */
app.factory('PXTODO', ['$resource', function($resource) {
    var resource = $resource('todo' ,{},
        {
            put: {method: 'PUT' }
        }
    );
    return resource;
}]);

