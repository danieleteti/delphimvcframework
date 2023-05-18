// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }


unit MVCFramework.Tests.AppController;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server;

type

  TAppUser = class
  strict private
    FCod: Integer;
    FName: string;
    FPass: string;
  public
    property Cod: Integer read FCod write FCod;
    property Name: string read FName write FName;
    property Pass: string read FPass write FPass;
  end;

  [MVCPath('/')]
  TAppController = class(TMVCController)
  public
    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloWorld(ctx: TWebContext);

    [MVCPath('/user')]
    [MVCHTTPMethod([httpGET])]
    procedure GetUser(ctx: TWebContext);

    [MVCPath('/user/save')]
    [MVCHTTPMethod([httpPOST])]
    procedure PostUser(ctx: TWebContext);

    [MVCPath('/users')]
    [MVCHTTPMethod([httpGET])]
    procedure GetUsers(ctx: TWebContext);

    [MVCPath('/users/save')]
    [MVCHTTPMethod([httpPOST])]
    procedure PostUsers(ctx: TWebContext);

    [MVCPath('/file/upload')]
    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes(TMVCMediaType.MULTIPART_FORM_DATA)]
    procedure ReceiveFile;

    [MVCPath('/body-url-encoded')]
    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_FORM_URLENCODED)]
    procedure PostBodyURLEncoded;
  end;

implementation

uses
  System.Hash, JsonDataObjects;

{ TAppController }

procedure TAppController.GetUser(ctx: TWebContext);
var
  LUser: TAppUser;
begin
  LUser := TAppUser.Create;
  try
    LUser.Cod := 1;
    LUser.Name := 'Ezequiel';
    LUser.Pass := '123';
  finally
    Render(LUser, True);
  end;
end;

procedure TAppController.GetUsers(ctx: TWebContext);
var
  LUsers: TObjectList<TAppUser>;
  LUser: TAppUser;
  I: Integer;
begin
  LUsers := TObjectList<TAppUser>.Create(True);

  for I := 0 to 10 do
  begin
    LUser := TAppUser.Create;
    LUser.Cod := I;
    LUser.Name := 'Ezequiel ' + IntToStr(I);
    LUser.Pass := IntToStr(I);

    LUsers.Add(LUser);
  end;

  Self.Render<TAppUser>(LUsers, True);
end;

procedure TAppController.HelloWorld(ctx: TWebContext);
begin
  Render('Hello World called with GET');
end;

procedure TAppController.PostBodyURLEncoded;
var
  lResponse: TJDOJsonObject;
  lParamValue: string;
begin
  lResponse := TJDOJsonObject.Create;

  if Context.Request.ContentFields.TryGetValue('field1', lParamValue) then
    lResponse.S['field1'] := lParamValue
  else
    lResponse.S['field1'] := '';

  if Context.Request.ContentFields.TryGetValue('field2', lParamValue) then
    lResponse.S['field2'] := lParamValue
  else
    lResponse.S['field2'] := '';

  if Context.Request.ContentFields.TryGetValue('field3', lParamValue) then
    lResponse.S['field3'] := lParamValue
  else
    lResponse.S['field3'] := '';

  Render(lResponse);
end;

procedure TAppController.PostUser(ctx: TWebContext);
var
  LUser: TAppUser;
begin
  LUser := ctx.Request.BodyAs<TAppUser>();
  try
    if (LUser.Cod > 0) then
      Render('Success!')
    else
      Render('Error!');
  finally
    LUser.Free;
  end;
end;

procedure TAppController.PostUsers(ctx: TWebContext);
var
  LUsers: TObjectList<TAppUser>;
begin
  LUsers := ctx.Request.BodyAsListOf<TAppUser>();
  try
    LUsers.OwnsObjects := True;

    if (LUsers.Count > 0) then
      Render('Success!')
    else
      Render('Error!');

  finally
    LUsers.Free;
  end;
end;

procedure TAppController.ReceiveFile;
begin
  if Context.Request.Files.Count = 0 then
  begin
    Render(HTTP_STATUS.BadRequest, 'No file found!')
  end
  else
  begin
    Render(THashMD5.GetHashString(Context.Request.Files[0].Stream))
  end;
end;

end.
