#DMVCFramework features
  * RESTful (RMM Level 3) compliant
  * Can be used in load balanced environment using Redis (http://Redis.io) [dev]
  * Fancy URL with parameter mappings
  * Specialied renders to generate text, html, JSON
  * Powerful mapper to map json to objects and datasets to objects
  * Can be packaged as stand alone server, apache module (XE6, XE7, XE8) and ISAPI dll
  * Integrated RESTClient
  * Works with XE3, XE4, XE5, XE6, XE7 and XE8
  * Completely unit tested
  * There is a sample for each functionlities
  * There is a complete set of trainings about it, but the samples are included in the project
  * Experimental support for IOCP [dev]
  * Server side generated pages using eLua (Embedded Lua)
  * Specific trainings are available (ask me for a date and a place)
  * Messaging extension using STOMP (beta)
  * Community driven (Facebook group https://www.facebook.com/groups/delphimvcframework)
  * Simple and [documented](https://github.com/danieleteti/delphimvcframework/blob/master/docs/ITDevCON%202013%20-%20Introduction%20to%20DelphiMVCFramework.pdf)
  

DelphiMVCFramework contains also a lot of indipendent code that can be used in other kind of project. 

These are the most notable:

  * Mapper (convert JSON in Object and back, ObjectList in JSONArray and back, DataSets in JSONArray or ObjectList and back)
  * LuaDelphiBinding (integrate Lua script into Delphi native code)
  * eLua (convert eLua into plain Lua executable script just like PHP or JSP)

##Samples and documentation
DMVCFramework is provided with a lot of examples focused on specific functionality.
All samples are in [Samples](https://github.com/danieleteti/delphimvcframework/tree/master/samples) folder


#Sample Controller
Below a basic sample of a DMVCFramework controller with 2 action

```delphi
	unit UsersControllerU;
	  
	interface
	  
	uses 
	  MVCFramework;
	 
	type 
	   [MVCPath('/users')]
	   TUsersController = class(TMVCController)
	   public
	    
	    //The following action will be with a GET request like the following
	    //http://myserver.com/users/3
	    [MVCPath('/($id)')]
	    [MVCProduce('application/json')]
	    [MVCHTTPMethod([httpGET])]
	    procedure GetUsers(CTX: TWebContext);
	
	    //The following action will be with a POST or PUT request like the following
	    //http://myserver.com/users/3
	    //and in the request body there should be a serialized TUser
	    [MVCPath('/($id)')]
	    [MVCProduce('application/json')]
	    [MVCHTTPMethod([httPOST, httpPUT])]
	    procedure UpdateOrCreateUser(CTX: TWebContext);
	
	  end;
	 
	implementation
	
	uses
	  MyTransactionScript; //contains actual data access code
	  
	{ TUsersController }
	 
	procedure TUsersController.GetUsers(CTX: TWebContext);
	var
	  User: TUser;
	begin
	  User := GetUserById(CTX.Request.Parameters['id'].ToInteger);
	  Render(User);
	end;
	
	procedure TUsersController.UpdateOrCreateUser(CTX: TWebContext);
	var
	  User: TUser;
	begin
	  User := CTX.Request.BodyAs<TUser>;
	  SaveUser(User);
	  Render(User);
	end;	
	  
	end.
```

###Quick Creation of DelphiMVCFramework Server

Create a new server is a simple task:

```delphi
	uses
	  MVCFramework.Server;
	
	var
	  ServerInfo: IMVCServerInfo;
	  Server: IMVCServer;
	begin
	  ServerInfo := TMVCServerInfoFactory.Build;
	  ServerInfo.ServerName := 'MVCServer';
	  ServerInfo.Port := 4000;
	  ServerInfo.MaxConnections := 1000;
	  //You must reference your TWebModuleClass  
	  ServerInfo.WebModuleClass := YourServerWebModuleClass;
	
	  Server := TMVCServerFactory.Build(ServerInfo);
	  Server.Start;
	  Server.Stop;
	end;
```

If you want to add a layer of security:

```delphi
        uses
	  MVCFramework.Server;
	
	var
	  ServerInfo: IMVCServerInfo;
	  Server: IMVCServer;
          OnAuthentication: TMVCAuthenticationDelegate;
	begin
	  ServerInfo := TMVCServerInfoFactory.Build;
	  ServerInfo.ServerName := 'MVCServer';
	  ServerInfo.Port := 4000;
	  ServerInfo.MaxConnections := 1000;
	  //You must reference your TWebModuleClass  
	  ServerInfo.WebModuleClass := YourServerWebModuleClass;

      OnAuthentication := procedure(const pUserName, pPassword: string;
        pUserRoles: TList<string>; var pIsValid: Boolean)
        begin
           pIsValid := pUserName.Equals('dmvc') and pPassword.Equals('123');
        end;

	  ServerInfo.Security := TMVCDefaultSecurity.Create(OnAuthentication, nil);
	
	  Server := TMVCServerFactory.Build(ServerInfo);
	  Server.Start;
	end;

      //And in his WebModule you should add the security middleware
     uses
        MVCFramework.Middleware.Authentication;
    
     procedure TTestWebModule.WebModuleCreate(Sender: TObject);
     begin
       MVCEngine := TMVCEngine.Create(Self);
	
       // Add Yours Controllers
       MVCEngine.AddController(TYourController);
	
       // Add Security Middleware
       MVCEngine.AddMiddleware(
	     TMVCBasicAuthenticationMiddleware.Create(Server.Info.Security)
       );
	end;  
```

You can work with a container of DelphiMVCFramework servers:

```delphi
	uses 
	  MVCFramework.Server;
	
	var
	  ServerOneInfo: IMVCServerInfo;
	  ServerTwoInfo: IMVCServerInfo;
	  Container: IMVCServerContainer;
	begin
	  Container := TMVCServerContainerFactory.Build();
	
	  ServerOneInfo := TMVCServerInfoFactory.Build;
	  ServerOneInfo.ServerName := 'MVCServer1';
	  ServerOneInfo.Port := 4000;
	  ServerOneInfo.MaxConnections := 1000;
	  ServerOneInfo.WebModuleClass := ServerOneWebModuleClass;
	
	  Container.CreateServer(ServerOneInfo);
	
	  ServerTwoInfo := TMVCServerInfoFactory.Build;
	  ServerTwoInfo.ServerName := 'MVCServer2';
	  ServerTwoInfo.Port := 5000;
	  ServerTwoInfo.MaxConnections := 1000;
	  ServerTwoInfo.WebModuleClass := ServerTwoWebModuleClass;
	
	  Container.CreateServer(ServerTwoInfo);
      
          Container.StartServers();
	end;  
```

###Links
Feel free to ask questions on the "Delphi MVC Framework" facebook group (https://www.facebook.com/groups/delphimvcframework).

http://www.danieleteti.it/2013/04/18/sneak-peek-to-simple-integration-between-dmvcframework-and-dorm/
