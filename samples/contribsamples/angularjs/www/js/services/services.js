'use strict';

/* Services */


// Demonstrate how to register services
// In this case it is a simple value service.

app.service('NotifyModel', function( ) {

    this.showMessageSuccess = function(result) {
        $('.top-right').notify({message: {text: result.message}, type: 'success',fadeOut: {enabled: true, delay: 1000}}).show();
    };

    this.showMessageError = function(result) {
        $('.top-right').notify({message: {text: result.message}, type: 'danger'}).show();
    };
});