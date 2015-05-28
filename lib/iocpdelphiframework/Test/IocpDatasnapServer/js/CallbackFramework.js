//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

/*
 * Event item which gets passed to a registered 'onChannelStateChange' event on a ClientChannel.
 * This contains the eventType (0 - channel start, 1 - channel stop, 2 - callback added, 3 - callback removed),
 * as well as the channel being acted on and the callback. Callback will be null if the event is a channel closing.
 */
function ClientChannelEventItem(eventType, channel, callback)
{
  this.eventType = eventType; //value as set in the EVENT_ variables of the ClientChannel class.
  this.channel = channel; //Instance of ClientChannel
  this.callback = callback; //Instance of ClientCallback
}

/*
 * Class for registering a callback which will update callbacks on the client when invoked from 
 * the server. A channel on the server is a manager for a specific object on the source which can
 * send out notifications. Registering a client callback hooks that callback in to be notified
 * of changes by the server.
 * @param channelId the unique identifier for this client channel (callback)
 * @param serverChannelName the unique name of the server channel
 * @param connectionInfo map (JSON Object) containing connection info. See ServerFunctionExecutor for more info
 * @param securityToken optional parameter used to prove ownership of your channel. If not specified one will be generated.
 */
function ClientChannel(channelId, serverChannelName, connectionInfo, securityToken)
{
  //constant values describing the possible event types passed to the onChannelStateChange event
  this.EVENT_CHANNEL_START = 0;
  this.EVENT_CHANNEL_STOP = 1;
  this.EVENT_SERVER_DISCONNECT = 2;
  this.EVENT_CALLBACK_ADDED = 3;
  this.EVENT_CALLBACK_REMOVED = 4;

  /*
   * This holds all of the registered callbacks for this client channel
   */
  this.callbacks = new Array();

  this.connectionInfo = connectionInfo;
  this.securityToken = (securityToken == null) ? (((new Date()).getTime()) + "") : securityToken;
  this.admin = new DSAdmin(connectionInfo);

  /*
   * Procedure to be notified when the state of the client channel or the callbacks it holds changes.
   */
  this.onChannelStateChange = null;

  /*
   * The name of the channel on the server this callback will 'listen to' for updates.
   * This is optional, but if left out all ClientCallbacks held by this channel need to specify their own channel names.
   */
  this.serverChannelName = (serverChannelName == null) ? "" : serverChannelName;
  
  /*
   * The wrapper of a continually issued XMLHTTPRequest which is used as the client callback on the server.
   */
  this.callbackLoop = null;

  /*
   * Unique ID of the client channel (callback.) Not to be confused with the server's Channel Name, which
   * uniquely identifies the channel type on the server.
   */
  this.channelId = channelId != null ? channelId : "client" + ((new Date()).getTime()) + Math.round(Math.random()*10000);

  /*
   * Registers a client callback with the server.
   * @param clientCallback an instance of ClientCallback
   */
  this.registerCallback = function(clientCallback) {
    if (this.callbackLoop != null && !this.callbackLoop.stopped)
    {
      this.callbacks[this.callbacks.length] = clientCallback;
      this.callbackLoop.registerCallback(clientCallback);
    }
  };

  /*
   * Unregisters a client callback from the server.
   * @param clientCallback the callback to unregister
   */
  this.unregisterCallback = function(clientCallback) {
    if (this.callbackLoop != null)
    {
      removeItemFromArray(this.callbacks, clientCallback);
      if (!this.callbackLoop.stopped)
      {
        this.callbackLoop.unregisterCallback(clientCallback);
      }
      if (this.callbacks.length == 0)
      {
        this.callbackLoop = null;
      }
    }
  };

  /*
   * Connects the client channel, registering a callback, as the channel can only
   * be open if at least one callback is registered.
   * @param firstCallback an instance of ClientCallback
   */
  this.connect = function(firstCallback) {
    if (this.callbackLoop != null)
    {
      this.callbackLoop.stop();
    }
    this.callbacks.length = 0; //clear the array
    this.callbacks[this.callbacks.length] = firstCallback;
    this.callbackLoop = new CallbackLoop(this, this.connectionInfo, this.securityToken);
    this.callbackLoop.start(firstCallback);
  };

  /*
   * Unregisters all callbacks and disconnect the channel.
   */
  this.disconnect = function() {
    this.callbacks.length = 0;
    if (this.callbackLoop != null)
    {
      this.callbackLoop.stop(); //this will unregister all callbacks
      this.callbackLoop = null;
    }
  };
  
  /*
   * Calls into the closeSession function of the ServerFunctionExecutor,
   * closing the current session with the server and indirectly
   * terminating the client channel.
   */
  this.closeSession = function() {
    if ( this.admin != null )
    {
      this.admin.executor.closeSession();
    }
  };
  
  /*
   * Broadcasts a message to all registered callbacks of the server channel
   * this ClientChannel is registered with.
   * @param message the message to broadcast
   * @param channelName optional name of channel to broadcast to. If left blank, serverChannelName will be used.
   * @return true if the message was sent successfully to the server (but not neccessarally all callbacks,)
   *         false otherwise (or a JSON value/object in special cases, such as errors or session timeouts)
   */
  this.broadcast = function(message, channelName) {
    var response = null;
    if (message != null)
    {
       var toBroadcastTo = (channelName == null || channelName == "" || !isString(channelName)) ? this.serverChannelName : channelName;

       if (toBroadcastTo != "") {
         response = this.admin.BroadcastToChannel(toBroadcastTo, message);
       }
    }
    
    //if the response isn't a formatted result then it is probably an error, so pass it back
    if (response != null && response.result == null)
    {
      return response;
    }
    
    return response != null && response.result != null && response.result;
  };
  
  /*
   * Sends a notification message to a single callback of a specific ClientChannel.
   * Note that if you try to notify a callback of this ClientChannel no trip to the
   * server will be made to perform this.
   * @param clientId the unique ID of ClientChannel the callback to notify is in
   * @param callbackId the unique ID of the callback to notify
   * @param message the message to notify the callback with
   * @return true (or possibly a response message) if notification was successful,
   *         false otherwise  (or a JSON value/object in special cases, such as errors or session timeouts)
   */
  this.notify = function(clientId, callbackId, message) {
    if (clientId == null || callbackId == null || message == null)
    {
      return false;
    }
    
    //optimization to prevent unrequired trip to the server.
    //Also done because of open connection limits in browsers which can be as low as 2 or 3
    if (clientId == this.channelId)
    {
      for(var i = 0; i < this.callbacks.length; i++)
      {
        var currCallback = this.callbacks[i];
        if (currCallback.callbackId == callbackId)
        {
          var cbResult = currCallback.notifyCallback(message);
          return cbResult != null ? cbResult : false;
        }
      }
      
      //callback with given ID doesn't exist
      return false;
    }
    
    //the callback isn't in this ClientChannel, so request for the server to notify it
    return this.admin.NotifyCallback(clientId, callbackId, message);
  };
}

/*
 * Class representing a client callback with a single function which will be invoked from
 * the server to update the callback, and then return a result which will be passed back to the server.
 * @param clientChannel an instance of ClientChannel, which will contain this callback
 * @param callbackId a unique id for this callback
 * @param clientFunction a function to call which will do the update of the physical callback and return a result.
 *                  This function takes a JSON Value as a parameter and returns a JSON Value as a result.
 * @param serverChannelNames a single string representing a server channel this callback cares about, or an array of channel names.
 *                  This supplements the serverChannelName specified by the ClientChannel this callback gets added to. This is optional
 *                  unless the ClientChannel doesn't specify a serverChannelName. In that case, this becomes a required property.
 */
function ClientCallback(clientChannel, callbackId, clientFunction, serverChannelNames)
{
  /*
   * An instance of the ClientChannel class which is intended to contain the callback. It isn't
   * guaranteed this containment will actually exist. To do so, you will need to call either the
   * "connect" or "registerCallback" function of the ClientChannel instance with this callback
   * as a parameter to create the containment relationship.
   */
  this.clientChannel = clientChannel;

  /*
   * The unique ID of this callback. This only needs to be unique on this particular client,
   * in this instance of ClientChannel and not across all instances of ClientChannel or all clients.
   */
  this.callbackId = callbackId != null ? callbackId :  "cb" + ((new Date()).getTime()) + Math.round(Math.random()*10000);

  /*
   * The function to call for updating the callback and returning a JSON Value response back to the server.
   * @param value a JSON Value instance containing the formatted JSON properties required for updating the callback
   * @return a JSON value response to deliver to the server. This response will reflect the current 
   *         state of the callback after the update.
   */
  this.notifyCallback = clientFunction;

  /*
   * An array of server channel names to listen to. The array can be empty, as long as clientChannel.serverChannelName
   * is not null or empty string. The parameter passed into 'ClientCallback' is assigned here, but if it is null, empty string,
   * or not a string or array, then an empty array is assigned. Otherwise, if it is an array it is directly assigned. It it is
   * a string, then the string is wrapped in an array and assigned here.
   */
  this.serverChannelNames = (serverChannelNames == null) ? [] : (serverChannelNames == "") ? [] : (isString(serverChannelNames)) ? [serverChannelNames] : (isArray(serverChannelNames)) ? serverChannelNames : [];
}

/*
 * Class which wraps an XMLHTTPRequest. When the request returns, the appropriate callback(s) are notified
 * and then this callback loop returns the result of the invocation back to the server.
 *
 * This long-term callback is achieved by issuing an HTTP request which will not return anything until
 * a server channel this client is interested in is updated. When the response is returned, 
 * it is handled and another request (the client's response) is sent.
 *
 * @param clientChannel the client channel to have registered on the server
 * @param connectionInfo map (JSON Object) containing connection info. See ServerFunctionExecutor for more info
 * @param securityToken SecurityToken to use, for callback ownership. (required: won't be generated)
 */
function CallbackLoop(clientChannel, connectionInfo, securityToken)
{
  this.executor = new ServerFunctionExecutor("DSAdmin", connectionInfo, this);
  this.clientChannel = clientChannel;
  this.stopped = true;
  this.securityToken = securityToken;

  /*
   * The callback which will handle a value passed in from the server and then pass
   * back a response to the server as long as the channel is active.
   */
  this.callback = function(responseObject, requestStatus, callbackLoop) {
    if (callbackLoop != null && !callbackLoop.stopped && responseObject != null)
    {
      //resolve the true response object
      responseObject = (responseObject.result != null) ? responseObject.result : responseObject;
      responseObject = isArray(responseObject) ? responseObject[0] : responseObject;
      
      //if the session this callback was created on has sense expired then stop the callback loop,
      //preventing any calls to callbackLoop.sendResponse from executing
      var sessId = getSessionID();
      if (sessId == null)
      {
        callbackLoop.stopped = true;
      }
      
      //session expired, so notify local callbacks and then stop the loop
      if (responseObject.SessionExpired != null)
      {
        callbackLoop.stopped = true; 
        for(var i = 0; i < clientChannel.callbacks.length; i++)
        {
          clientChannel.callbacks[i].notifyCallback(responseObject);
        }
        //notify that the channel has been closed
        if (isReferenceAFunction(clientChannel.onChannelStateChange)) {
          clientChannel.onChannelStateChange(new ClientChannelEventItem(clientChannel.EVENT_SERVER_DISCONNECT,
                                                                        clientChannel, null));
        }
      }
      //broadcast to all of the callbacks listening on the given channel
      else if (responseObject.broadcast != null)
      {
        var paramArray = responseObject.broadcast;
        var paramValue = paramArray[0];
 
        //used to determine if the paramValue is (on the server) a JSONValue or a TObject
        var dataType = paramArray[1];

        var broadcastChannel = responseObject.channel == null ? clientChannel.serverChannelName : responseObject.channel;
        var doForAll = clientChannel.serverChannelName == broadcastChannel;

        for(var i = 0; i < clientChannel.callbacks.length; i++)
        {
          var currentCallback = clientChannel.callbacks[i];

          //Broadcast to the callback if the channel being broadcast to is the one specified in the ClientChannel,
          //or if it appears in the array of channels this specific callback cares about.
          if (doForAll || arrayIndexOf(currentCallback.serverChannelNames, broadcastChannel) > -1) {
            currentCallback.notifyCallback(paramValue, dataType);
          }
        }
        callbackLoop.sendResponse(true, callbackLoop);
      }
      //Invoke the specified callback
      else if (responseObject.invoke != null)
      {
        var paramArray = responseObject.invoke;
        var callbackKey = paramArray[0];
        var paramValue = paramArray[1];
 
        //used to determine if the paramValue is (on the server) a JSONValue or a TObject
        var dataType = paramArray[2];

        var currCallback;
        for(var i = 0; i < clientChannel.callbacks.length; i++)
        {
          currCallback = clientChannel.callbacks[i];
 
          if (currCallback.callbackId == callbackKey)
          {
            callbackLoop.sendResponse(currCallback.notifyCallback(paramValue, dataType), callbackLoop);
            break;
          }
        }
      }
      //if an error has occured notify the callbacks and stop the loop
      else if (responseObject.error != null)
      {
        callbackLoop.stopped = true;
        for(var i = 0; i < clientChannel.callbacks.length; i++)
        {
          clientChannel.callbacks[i].notifyCallback(responseObject, "error");
        }
        //notify that the channel has been closed by
        if (isReferenceAFunction(clientChannel.onChannelStateChange)) {
          clientChannel.onChannelStateChange(new ClientChannelEventItem(this.clientChannel.EVENT_SERVER_DISCONNECT,
                                                                             this.clientChannel, null));
        }
      }
      //If the result key is 'close' or 'closeChannel' then no response should be sent, which means
      //the recursion of this loop will end. Otherwise, send a response to the server with
      //a value of false so the loop will continue and the server will know the invocation failed
      else if (responseObject.closeChannel == null && responseObject.close == null)
      {
        callbackLoop.sendResponse(false, callbackLoop);
      }
      else
      {
        callbackLoop.stopped = true;
        
        //notify each callback that it has been closed
        for(var i = 0; i < clientChannel.callbacks.length; i++)
        {
          clientChannel.callbacks[i].notifyCallback(responseObject, "closed");
        }
        
        //notify that the channel has been closed
        if (isReferenceAFunction(clientChannel.onChannelStateChange)) {
          clientChannel.onChannelStateChange(new ClientChannelEventItem(clientChannel.EVENT_CHANNEL_STOP,
                                                                        clientChannel, null));
        }
      }
    }
    else
    {
      if (callbackLoop != null) {
        if (!callbackLoop.stopped && isReferenceAFunction(clientChannel.onChannelStateChange)) {
          //notify that the channel has been closed by the server
          clientChannel.onChannelStateChange(new ClientChannelEventItem(clientChannel.EVENT_SERVER_DISCONNECT,
                                                                        clientChannel, null));
        }
        callbackLoop.stopped = true;
      }
    }
  };

  /*
   * Starts the loop, registering the client callback on the server and the initial client callback specified
   * @param firstCallback the first callback to register, as you can't register a client with the server without specifying the first callback
   */
  this.start = function(firstCallback) {
    if (this.stopped && (!nullOrEmptyStr(this.clientChannel) || firstCallback.serverChannelNames.length > 0))
    {
      this.stopped = false;
    
      //passes empty string for the ConsumeClientChannel last parameter, since this is initiating the channel, and has no value
      //passes true after the callback to say a response from the server is expected
      this.executor.executeMethod("ConsumeClientChannel", "GET", 
                             [this.clientChannel.serverChannelName, this.clientChannel.channelId, 
                              firstCallback.callbackId, arrayToCSV(firstCallback.serverChannelNames), this.securityToken, ""],
                              this.callback, true);

      if (isReferenceAFunction(this.clientChannel.onChannelStateChange)) {
        this.clientChannel.onChannelStateChange(new ClientChannelEventItem(this.clientChannel.EVENT_CHANNEL_START,
                                                                           this.clientChannel, firstCallback));
      }
    }
  };

  /*
   * Tells the loop to terminate. The loop will still be hung waiting for a response from the server, but
   * once it gets that it will disregard the server command and not send another http request. This also
   * sends a command to the server, telling it that the client wishes to be unregistered.
   */
  this.stop = function() {
    if(!this.stopped && this.clientChannel != null)
    {
      this.stopped = true;
      this.executor.executeMethod("CloseClientChannel", "GET", 
                         [this.clientChannel.channelId, this.securityToken], null);

      if (isReferenceAFunction(this.clientChannel.onChannelStateChange)) {
        this.clientChannel.onChannelStateChange(new ClientChannelEventItem(this.clientChannel.EVENT_CHANNEL_STOP,
                                                                           this.clientChannel, null));
      }
    }
  };

  /*
   * Function which should only be called internally from the callback inside this CallbackLoop instance.
   * This function is used to return a given response to the server, reinitializing the callback HTTP request in the process.
   */
  this.sendResponse = function(responseObject,callbackLoop) {
    if (!this.stopped)
    {
      if (callbackLoop == null)
      {
        callbackLoop = this;
      }
      //passes empty string for server channel name, as this is only needed when registering the client channel
      //passes empty string for the CallbackId parameter of ConsumeClientChannel to say this is a response to a client invocation
      //passes empty string for the array of callback's server channel names, as this call isn't registering a callback
      //passes the response JSON Value as the last parameter of the ConsumeclientChannel call
      this.executor.executeMethod("ConsumeClientChannel", "POST", 
                             ["", callbackLoop.clientChannel.channelId, "", "", this.securityToken, responseObject],
                              callbackLoop.callback, true);
    }
  };

  /*
   * This function registers the given callback. This should only be called after the
   * start function has been called and the loop is active. The call is not issued through
   * the loop; it instead uses a separate http request to do this.
   */
  this.registerCallback = function(clientCallback) {
    if(!this.stopped && clientCallback != null && (!nullOrEmptyStr(this.clientChannel) || clientCallback.serverChannelNames.length > 0))
    {
      this.executor.executeMethod("RegisterClientCallbackServer", "GET", 
                             [this.clientChannel.channelId, 
                              clientCallback.callbackId, arrayToCSV(clientCallback.serverChannelNames), this.securityToken], null);

      if (isReferenceAFunction(this.clientChannel.onChannelStateChange)) {
        this.clientChannel.onChannelStateChange(new ClientChannelEventItem(this.clientChannel.EVENT_CALLBACK_ADDED,
                                                                           this.clientChannel, clientCallback));
      }
    }
  };

  /*
   * This function unregisters the given callback
   */
  this.unregisterCallback = function(clientCallback) {
    if(!this.stopped && this.clientChannel != null && clientCallback != null)
    {
      this.executor.executeMethod("UnregisterClientCallback", "GET", 
                             [this.clientChannel.channelId,
                              clientCallback.callbackId, this.securityToken], null);

      if (isReferenceAFunction(this.clientChannel.onChannelStateChange)) {
        this.clientChannel.onChannelStateChange(new ClientChannelEventItem(this.clientChannel.EVENT_CALLBACK_REMOVED,
                                                                           this.clientChannel, clientCallback));
      }
    }
  };
}

function removeItemFromArray(anArray, arrayElement)
{
  if (isArray(anArray))
  {
    for (var i = 0; i < anArray.length; i++)
    {
      if (anArray[i] == arrayElement)
      {
        anArray.splice(i, 1);
      }
    }
  }
}
