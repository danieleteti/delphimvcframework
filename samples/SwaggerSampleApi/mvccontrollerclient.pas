unit mvccontrollerclient;

interface


uses
    IPPeerClient
  , REST.Client
  , REST.Authenticator.OAuth
  , REST.Types
  , MVCFramework
  , MVCFramework.Commons
  ;


(*
Title: Swagger Petstore
Description: This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.
License: Apache 2.0
*)

type
  notdefined = string;

  [MVCPath('/v2')]
  TMyMVCControllerClient = class(TObject)
    RESTClient : TRESTClient;
    RESTRequest : TRESTRequest;
    RESTResponse : TRESTResponse;

    [MVCDoc('')]
    [MVCPath('/pet')]
    [MVCHTTPMethod([httppost])]
    procedure addPet(paramBody: notdefined);

    [MVCDoc('')]
    [MVCPath('/pet')]
    [MVCHTTPMethod([httpput])]
    procedure updatePet(paramBody: notdefined);

    [MVCDoc('Multiple status values can be provided with comma separated strings')]
    [MVCPath('/pet/findByStatus')]
    [MVCHTTPMethod([httpget])]
    procedure findPetsByStatus(paramStatus: Array of string);

    [MVCDoc('Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.')]
    [MVCPath('/pet/findByTags')]
    [MVCHTTPMethod([httpget])]
    procedure findPetsByTags(paramTags: Array of string);

    [MVCDoc('Returns a single pet')]
    [MVCPath('/pet/{petId}')]
    [MVCHTTPMethod([httpget])]
    procedure getPetById(paramPetId: Integer);

    [MVCDoc('')]
    [MVCPath('/pet/{petId}')]
    [MVCHTTPMethod([httppost])]
    procedure updatePetWithForm(paramPetId: Integer; paramName: String; paramStatus: String);

    [MVCDoc('')]
    [MVCPath('/pet/{petId}')]
    [MVCHTTPMethod([httpdelete])]
    procedure deletePet(paramApi_key: String; paramPetId: Integer);

    [MVCDoc('')]
    [MVCPath('/pet/{petId}/uploadImage')]
    [MVCHTTPMethod([httppost])]
    procedure uploadFile(paramPetId: Integer; paramAdditionalMetadata: String; paramFile: string);

    [MVCDoc('Returns a map of status codes to quantities')]
    [MVCPath('/store/inventory')]
    [MVCHTTPMethod([httpget])]
    procedure getInventory;

    [MVCDoc('')]
    [MVCPath('/store/order')]
    [MVCHTTPMethod([httppost])]
    procedure placeOrder(paramBody: notdefined);

    [MVCDoc('For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions')]
    [MVCPath('/store/order/{orderId}')]
    [MVCHTTPMethod([httpget])]
    procedure getOrderById(paramOrderId: Integer);

    [MVCDoc('For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors')]
    [MVCPath('/store/order/{orderId}')]
    [MVCHTTPMethod([httpdelete])]
    procedure deleteOrder(paramOrderId: Integer);

    [MVCDoc('This can only be done by the logged in user.')]
    [MVCPath('/user')]
    [MVCHTTPMethod([httppost])]
    procedure createUser(paramBody: notdefined);

    [MVCDoc('')]
    [MVCPath('/user/createWithArray')]
    [MVCHTTPMethod([httppost])]
    procedure createUsersWithArrayInput(paramBody: notdefined);

    [MVCDoc('')]
    [MVCPath('/user/createWithList')]
    [MVCHTTPMethod([httppost])]
    procedure createUsersWithListInput(paramBody: notdefined);

    [MVCDoc('')]
    [MVCPath('/user/login')]
    [MVCHTTPMethod([httpget])]
    procedure loginUser(paramUsername: String; paramPassword: String);

    [MVCDoc('')]
    [MVCPath('/user/logout')]
    [MVCHTTPMethod([httpget])]
    procedure logoutUser;

    [MVCDoc('')]
    [MVCPath('/user/{username}')]
    [MVCHTTPMethod([httpget])]
    procedure getUserByName(paramUsername: String);

    [MVCDoc('This can only be done by the logged in user.')]
    [MVCPath('/user/{username}')]
    [MVCHTTPMethod([httpput])]
    procedure updateUser(paramUsername: String; paramBody: notdefined);

    [MVCDoc('This can only be done by the logged in user.')]
    [MVCPath('/user/{username}')]
    [MVCHTTPMethod([httpdelete])]
    procedure deleteUser(paramUsername: String);

  end;


implementation


uses
    Swag.Doc
  ;



procedure TMyMVCControllerClient.addPet(paramBody: notdefined);
begin

end;

procedure TMyMVCControllerClient.updatePet(paramBody: notdefined);
begin

end;

procedure TMyMVCControllerClient.findPetsByStatus(paramStatus: Array of string);
begin

end;

procedure TMyMVCControllerClient.findPetsByTags(paramTags: Array of string);
begin

end;

procedure TMyMVCControllerClient.getPetById(paramPetId: Integer);
begin

end;

procedure TMyMVCControllerClient.updatePetWithForm(paramPetId: Integer; paramName: String; paramStatus: String);
begin

end;

procedure TMyMVCControllerClient.deletePet(paramApi_key: String; paramPetId: Integer);
begin

end;

procedure TMyMVCControllerClient.uploadFile(paramPetId: Integer; paramAdditionalMetadata: String; paramFile: string);
begin

end;

procedure TMyMVCControllerClient.getInventory;
begin

end;

procedure TMyMVCControllerClient.placeOrder(paramBody: notdefined);
begin

end;

procedure TMyMVCControllerClient.getOrderById(paramOrderId: Integer);
begin

end;

procedure TMyMVCControllerClient.deleteOrder(paramOrderId: Integer);
begin

end;

procedure TMyMVCControllerClient.createUser(paramBody: notdefined);
begin

end;

procedure TMyMVCControllerClient.createUsersWithArrayInput(paramBody: notdefined);
begin

end;

procedure TMyMVCControllerClient.createUsersWithListInput(paramBody: notdefined);
begin

end;

procedure TMyMVCControllerClient.loginUser(paramUsername: String; paramPassword: String);
begin

end;

procedure TMyMVCControllerClient.logoutUser;
begin

end;

procedure TMyMVCControllerClient.getUserByName(paramUsername: String);
begin

end;

procedure TMyMVCControllerClient.updateUser(paramUsername: String; paramBody: notdefined);
begin

end;

procedure TMyMVCControllerClient.deleteUser(paramUsername: String);
begin

end;

end.
