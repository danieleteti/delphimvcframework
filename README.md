#DMVCFramework features
  * RESTful (RMM Level 3) compliant
  * Can be used in load balanced environment using Redis (http://Redis.io) [dev]
  * Fancy URL with parameter mappings
  * Specialied renders to generate text, html, JSON
  * Powerful mapper to map json to objects and datasets to objects
  * Can be packaged as stand alone server, apache module (XE6 or better) and ISAPI dll
  * Integrated RESTClient
  * Works with XE3, XE4, XE5, XE6, XE7, XE8 and Delphi 10 Seattle
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


#Sample Server
Below a basic sample of a DMVCFramework server wich can be deployed as standa-alone application, as an Apache module or as ISAPI dll. This flexibility is provided by the Delphi WebBroker framework (built-in in Delphi since Delphi 4).

To create this server, you have to create a new Delphi Projects -> WebBroker -> WebServerApplication. Then add the following changes to the webmodule.
```delphi
unit WebModuleUnit1;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework {this unit contains TMVCEngine class};

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);

  private
    MVC: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses UsersControllerU; //this is the unit where is defined the controller

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self);
  MVC.Config['document_root'] := 'public_html'; //if you need some static html, javascript, etc (optional)
  MVC.AddController(TUsersController); //see next section to know how to create a controller
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  MVC.Free;
end;

end.
```

Remember that the files inside the redist folder *must* be in the executable path or in the system path. If starting the server whithin the IDE doesn't works, try to run the executable outside the IDE and check the dependencies.
That's it! You have just created your first DelphiMVCFramework. Now you have to add a controller to respond to the http request.

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

    //The following action will be with a PUT request like the following
    //http://myserver.com/users/3
    //and in the request body there should be a serialized TUser
    [MVCPath('/($id)')]
    [MVCProduce('application/json')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateUser(CTX: TWebContext);

    //The following action will respond to a POST request like the following
    //http://myserver.com/users
    //and in the request body there should be the new user to create as json
    [MVCPath]
    [MVCProduce('application/json')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateUser(CTX: TWebContext);

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

procedure TUsersController.UpdateUser(CTX: TWebContext);
var
  User: TUser;
begin
  User := CTX.Request.BodyAs<TUser>;
  SaveUser(User);
  Render(User);
end;	
  
procedure TUsersController.CreateUser(CTX: TWebContext);
var
  User: TUser;
begin
  User := CTX.Request.BodyAs<TUser>;
  CreateUser(User);
  Render(User);
end;	
  
end.
```

Now you have a performant RESTful server wich respond to the following URLs:
- GET /users/($id)		(eg. /users/1, /users/45 etc)
- PUT /users/($id)		(eg. /users/1, /users/45 etc with the JSON data in the request body)
- POST /users			(the JSON data must be in the request body)

###Quick Creation of DelphiMVCFramework Server

If you dont plan to deploy your DMVCFramework server behind a webserver (apache or IIS) you can also pack more than one server into one single executable. In this case, the process is a bit different and involves the creation of a server container. However, create a new server is a simple task:

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

	OnAuthentication := procedure(const pUserName, pPassword: string; pUserRoles: TList<string>; var pIsValid: Boolean)
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
	MVCEngine.AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(Server.Info.Security));
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
