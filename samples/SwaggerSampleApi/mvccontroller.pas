unit mvccontroller;

interface


uses
    MVCFramework
  , MVCFramework.Commons
  , MVCFramework.Logger
  , MVCFramework.JWT
  , Generics.Collections
  , Swag.Common.Types
  ;


(*
Title: Swagger Petstore
Description: This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.
License: Apache 2.0
*)

type
  TApiResponse = class
    [MVCFormat('int32')]
    code : integer;

    &type : string;

    message : string;

  end;

  TCategory = class
    [MVCFormat('int64')]
    id : Int64;

    name : string;

  end;

  TOrder = class
    [MVCFormat('int64')]
    id : Int64;

    [MVCFormat('int64')]
    petId : Int64;

    [MVCFormat('int32')]
    quantity : integer;

    [MVCFormat('date-time')]
    shipDate : string;

    [MVCDoc('Order Status')]
    status : string;

    complete : boolean;

  end;

  TTag = class
    [MVCFormat('int64')]
    id : Int64;

    name : string;

  end;

  TPet = class
    [MVCFormat('int64')]
    id : Int64;

    category : TCategory;

    name : string;

    photoUrls : array of string;

    tags : array of TTag;

    [MVCDoc('pet status in the store')]
    status : string;

  end;



  TUser = class
    [MVCFormat('int64')]
    id : Int64;

    username : string;

    firstName : string;

    lastName : string;

    email : string;

    password : string;

    phone : string;

    [MVCDoc('User Status')]
    [MVCFormat('int32')]
    userStatus : integer;

  end;

  [MVCPath('/v2')]
  TMyMVCController = class(TMVCController)
    [MVCPath('/pet')]
    [MVCHTTPMethod([httpPOST])]
[MVCParam('body', rpiBody, TPet)]
    [MVCResponse(HTTP_STATUS.MethodNotAllowed, 'Invalid input')]
    procedure AddPet;

    [MVCPath('/pet')]
    [MVCHTTPMethod([httpPUT])]
[MVCParam('body', rpiBody, TPet)]
    [MVCResponse(HTTP_STATUS.MethodNotAllowed, 'Validation exception')]
    [MVCResponse(HTTP_STATUS.NotFound, 'Pet not found')]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid ID supplied')]
    procedure UpdatePet;

    [MVCDoc('Multiple status values can be provided with comma separated strings')]
    [MVCPath('/pet/findByStatus')]
    [MVCHTTPMethod([httpGET])]
[MVCParam('status', rpiQuery, stpArray)]
    [MVCResponseList(HTTP_STATUS.OK, 'successful operation', TPet)]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid status value')]
    procedure FindPetsByStatus;

    [MVCDoc('Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.')]
    [MVCPath('/pet/findByTags')]
    [MVCHTTPMethod([httpGET])]
[MVCParam('tags', rpiQuery, stpArray)]
    [MVCResponseList(HTTP_STATUS.OK, 'successful operation', TPet)]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid tag value')]
    procedure FindPetsByTags;

    [MVCDoc('Returns a single pet')]
    [MVCPath('/pet/($petId)')]
    [MVCHTTPMethod([httpGET])]
    [MVCResponse(HTTP_STATUS.OK, 'successful operation', TPet)]
    [MVCResponse(HTTP_STATUS.NotFound, 'Pet not found')]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid ID supplied')]
    procedure GetPetById([MVCDoc('ID of pet to return')] [MVCPathParam(stpInteger, '', 'int64')] petId: Int64);

    [MVCPath('/pet/($petId)')]
    [MVCHTTPMethod([httpPOST])]
[MVCParam('name', rpiFormData, stpString)]
[MVCParam('status', rpiFormData, stpString)]
    [MVCResponse(HTTP_STATUS.MethodNotAllowed, 'Invalid input')]
    procedure UpdatePetWithForm([MVCDoc('ID of pet that needs to be updated')] [MVCPathParam(stpInteger, '', 'int64')] petId: Int64);

    [MVCPath('/pet/($petId)')]
    [MVCHTTPMethod([httpDELETE])]
[MVCParam('api_key', rpiHeader, stpString)]
    [MVCResponse(HTTP_STATUS.NotFound, 'Pet not found')]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid ID supplied')]
    procedure DeletePet([MVCDoc('Pet id to delete')] [MVCPathParam(stpInteger, '', 'int64')] petId: Int64);

    [MVCPath('/pet/($petId)/uploadImage')]
    [MVCHTTPMethod([httpPOST])]
[MVCParam('additionalMetadata', rpiFormData, stpString)]
[MVCParam('file', rpiFormData, stpFile)]
    [MVCResponse(HTTP_STATUS.OK, 'successful operation', TApiResponse)]
    procedure UploadFile([MVCDoc('ID of pet to update')] [MVCPathParam(stpInteger, '', 'int64')] petId: Int64);

    [MVCDoc('Returns a map of status codes to quantities')]
    [MVCPath('/store/inventory')]
    [MVCHTTPMethod([httpGET])]
    procedure GetInventory;

    [MVCPath('/store/order')]
    [MVCHTTPMethod([httpPOST])]
[MVCParam('body', rpiBody, TOrder)]
    [MVCResponse(HTTP_STATUS.OK, 'successful operation', TOrder)]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid Order')]
    procedure PlaceOrder;

    [MVCDoc('For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions')]
    [MVCPath('/store/order/($orderId)')]
    [MVCHTTPMethod([httpGET])]
    [MVCResponse(HTTP_STATUS.OK, 'successful operation', TOrder)]
    [MVCResponse(HTTP_STATUS.NotFound, 'Order not found')]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid ID supplied')]
    procedure GetOrderById([MVCDoc('ID of pet that needs to be fetched')] [MVCPathParam(stpInteger, '', 'int64')] orderId: Int64);

    [MVCDoc('For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors')]
    [MVCPath('/store/order/($orderId)')]
    [MVCHTTPMethod([httpDELETE])]
    [MVCResponse(HTTP_STATUS.NotFound, 'Order not found')]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid ID supplied')]
    procedure DeleteOrder([MVCDoc('ID of the order that needs to be deleted')] [MVCPathParam(stpInteger, '', 'int64')] orderId: Int64);

    [MVCDoc('This can only be done by the logged in user.')]
    [MVCPath('/user')]
    [MVCHTTPMethod([httpPOST])]
[MVCParam('body', rpiBody, TUser)]
    [MVCResponse(HTTP_STATUS.OK, 'successful operation')]
    procedure CreateUser;

    [MVCPath('/user/createWithArray')]
    [MVCHTTPMethod([httpPOST])]
[MVCParam('body', rpiBody, stpNotDefined)]
    [MVCResponse(HTTP_STATUS.OK, 'successful operation')]
    procedure CreateUsersWithArrayInput;

    [MVCPath('/user/createWithList')]
    [MVCHTTPMethod([httpPOST])]
[MVCParam('body', rpiBody, stpNotDefined)]
    [MVCResponse(HTTP_STATUS.OK, 'successful operation')]
    procedure CreateUsersWithListInput;

    [MVCPath('/user/login')]
    [MVCHTTPMethod([httpGET])]
[MVCParam('username', rpiQuery, stpString)]
[MVCParam('password', rpiQuery, stpString)]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid username/password supplied')]
    procedure LoginUser;

    [MVCPath('/user/logout')]
    [MVCHTTPMethod([httpGET])]
    [MVCResponse(HTTP_STATUS.OK, 'successful operation')]
    procedure LogoutUser;

    [MVCPath('/user/($username)')]
    [MVCHTTPMethod([httpGET])]
    [MVCResponse(HTTP_STATUS.OK, 'successful operation', TUser)]
    [MVCResponse(HTTP_STATUS.NotFound, 'User not found')]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid username supplied')]
    procedure GetUserByName([MVCDoc('The name that needs to be fetched. Use user1 for testing. ')] [MVCPathParam(stpString)] username: String);

    [MVCDoc('This can only be done by the logged in user.')]
    [MVCPath('/user/($username)')]
    [MVCHTTPMethod([httpPUT])]
[MVCParam('body', rpiBody, TUser)]
    [MVCResponse(HTTP_STATUS.NotFound, 'User not found')]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid user supplied')]
    procedure UpdateUser([MVCDoc('name that need to be updated')] [MVCPathParam(stpString)] username: String);

    [MVCDoc('This can only be done by the logged in user.')]
    [MVCPath('/user/($username)')]
    [MVCHTTPMethod([httpDELETE])]
    [MVCResponse(HTTP_STATUS.NotFound, 'User not found')]
    [MVCResponse(HTTP_STATUS.BadRequest, 'Invalid username supplied')]
    procedure DeleteUser([MVCDoc('The name that needs to be deleted')] [MVCPathParam(stpString)] username: String);

  end;


implementation


uses
    Swag.Doc
  ;



procedure TMyMVCController.AddPet;
var
  paramBody : TPet;
begin
  paramBody := Context.Request.BodyAs<TPet>;
  // 405 Invalid input

end;

procedure TMyMVCController.UpdatePet;
var
  paramBody : TPet;
begin
  paramBody := Context.Request.BodyAs<TPet>;
  // 405 Validation exception
  // 404 Pet not found
  // 400 Invalid ID supplied

end;

procedure TMyMVCController.FindPetsByStatus;
var
  paramStatus : String;
  Pet : TObjectList<TPet>;
begin
  paramStatus := Context.Request.Params['status'];
  Pet := Context.Request.BodyAsListOf<TPet>;

  {TODO: Implement filling Pet }

  Render(HTTP_STATUS.OK, Pet);
  // 400 Invalid status value

end;

procedure TMyMVCController.FindPetsByTags;
var
  paramTags : String;
  Pet : TObjectList<TPet>;
begin
  paramTags := Context.Request.Params['tags'];
  Pet := Context.Request.BodyAsListOf<TPet>;

  {TODO: Implement filling Pet }

  Render(HTTP_STATUS.OK, Pet);
  // 400 Invalid tag value

end;

procedure TMyMVCController.GetPetById(petId: Int64);
var
  Pet : TPet;
begin
  Pet := TPet.Create;

  {TODO: Implement filling Pet }
  Render(HTTP_STATUS.OK, Pet);
  // 404 Pet not found
  // 400 Invalid ID supplied

end;

procedure TMyMVCController.UpdatePetWithForm(petId: Int64);
var
  paramName : String;
  paramStatus : String;
begin
  paramName := Context.Request.Params['name'];
  paramStatus := Context.Request.Params['status'];
  // 405 Invalid input

end;

procedure TMyMVCController.DeletePet(petId: Int64);
var
  paramApi_key : String;
begin
  paramApi_key := Context.Request.Params['api_key'];
  // 404 Pet not found
  // 400 Invalid ID supplied

end;

procedure TMyMVCController.UploadFile(petId: Int64);
var
  paramAdditionalMetadata : String;
  paramFile : String;
  ApiResponse : TApiResponse;
begin
  paramAdditionalMetadata := Context.Request.Params['additionalMetadata'];
  paramFile := Context.Request.Params['file'];
  ApiResponse := TApiResponse.Create;

  {TODO: Implement filling ApiResponse }
  Render(HTTP_STATUS.OK, ApiResponse);

end;

procedure TMyMVCController.GetInventory;
begin

end;

procedure TMyMVCController.PlaceOrder;
var
  paramBody : TOrder;
  Order : TOrder;
begin
  paramBody := Context.Request.BodyAs<TOrder>;
  Order := TOrder.Create;

  {TODO: Implement filling Order }
  Render(HTTP_STATUS.OK, Order);
  // 400 Invalid Order

end;

procedure TMyMVCController.GetOrderById(orderId: Int64);
var
  Order : TOrder;
begin
  Order := TOrder.Create;

  {TODO: Implement filling Order }
  Render(HTTP_STATUS.OK, Order);
  // 404 Order not found
  // 400 Invalid ID supplied

end;

procedure TMyMVCController.DeleteOrder(orderId: Int64);
begin
  // 404 Order not found
  // 400 Invalid ID supplied

end;

procedure TMyMVCController.CreateUser;
var
  paramBody : TUser;
begin
  paramBody := Context.Request.BodyAs<TUser>;
  // default successful operation

end;

procedure TMyMVCController.CreateUsersWithArrayInput;
var
  paramBody : TObjectList<TUser>;
begin
  paramBody := Context.Request.BodyAsListOf<TUser>;
  // default successful operation

end;

procedure TMyMVCController.CreateUsersWithListInput;
var
  paramBody : TObjectList<TUser>;
begin
  paramBody := Context.Request.BodyAsListOf<TUser>;
  // default successful operation

end;

procedure TMyMVCController.LoginUser;
var
  paramUsername : String;
  paramPassword : String;
begin
  paramUsername := Context.Request.Params['username'];
  paramPassword := Context.Request.Params['password'];
  // 400 Invalid username/password supplied

end;

procedure TMyMVCController.LogoutUser;
begin
  // default successful operation

end;

procedure TMyMVCController.GetUserByName(username: String);
var
  User : TUser;
begin
  User := TUser.Create;

  {TODO: Implement filling User }
  Render(HTTP_STATUS.OK, User);
  // 404 User not found
  // 400 Invalid username supplied

end;

procedure TMyMVCController.UpdateUser(username: String);
var
  paramBody : TUser;
begin
  paramBody := Context.Request.BodyAs<TUser>;
  // 404 User not found
  // 400 Invalid user supplied

end;

procedure TMyMVCController.DeleteUser(username: String);
begin
  // 404 User not found
  // 400 Invalid username supplied

end;

end.
