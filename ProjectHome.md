# DMVCFramework features #
  * RESTful (RMM Level 3) compliant
  * Can be used in load balanced environment using Redis (http://Redis.io) [dev](dev.md)
  * Fancy URL with parameter mappings
  * Specialied renders to generate text, html, JSON
  * Powerful mapper to map json to objects and datasets to objects
  * Can be packaged as stand alone server, apache module (XE6, XE7) and ISAPI dll
  * Integrated RESTClient
  * Works with XE3, XE4, XE5, XE6 and XE7
  * For each functionalities there is at least a test case
  * There is a sample for each functionlities
  * There is a complete set of trainings about it, but the samples are included in the project
  * Experimental support for IOCP [dev](dev.md)
  * Server side generated pages using eLua (Embedded Lua)
  * Specific trainings are available (ask me for a date and a place)
  * Messaging extension using STOMP (beta)
  * Community driven (Facebook group https://www.facebook.com/groups/delphimvcframework)
  * Simple and [documented](https://delphimvcframework.googlecode.com/svn/trunk/docs/ITDevCON%202013%20-%20Introduction%20to%20DelphiMVCFramework.pdf) (right click "Save as...")
  * Completely unit tested

The way to get DMVCFramework is a SVN checkout as explained in the "Source" tab of this page.

DelphiMVCFramework contains also a lot of indipendent code that can be used in other kind of project.

These are the most notable:

  * Mapper (convert JSON in Object and back, ObjectList in !JSONArray and back, DataSets in JSONArray or ObjectList and back)
  * LuaDelphiBinding (integrate Lua script into Delphi native code)
  * [eLua](eLuaTextFilter.md) (convert eLua into plain Lua executable script just like PHP or JSP)

## Samples and documentation ##
DMVCFramework is provided with a lot of examples focused on specific functionality.
All samples are in `Samples` folder


# Sample Controller #
Below a basic sample of a DMVCFramework controller with 2 action

```

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


Feel free to ask questions on the "Delphi MVC Framework" facebook group (https://www.facebook.com/groups/delphimvcframework).

### How to get the source ###
  * Download and install TortoiseSVN from http://tortoisesvn.net/downloads.html
  * Create an empty folder named DMVCFramework (or as you wish)
  * Open the folder, right-click on a blank area, and select SVN Checkout...
  * In the URL of Repository, enter "http://delphimvcframework.googlecode.com/svn/trunk/" and press OK
  * Wait untill all the files are downloaded and you will have all the Source Codes and the samples

### Links ###
http://www.danieleteti.it/2013/04/18/sneak-peek-to-simple-integration-between-dmvcframework-and-dorm/