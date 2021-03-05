// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// Contributer on this file: Janidan - https://github.com/janidan
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

unit MainClientFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts,
  MVCFramework.RESTClient.Intf, MVCFramework.RESTClient,
  Vcl.ExtCtrls;

type
  TForm7 = class(TForm)
    GroupBox1: TGroupBox;
    edtUsername: TEdit;
    edtPassword: TEdit;
    btnLogInLogOut: TButton;
    ApplicationEvents1: TApplicationEvents;
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnLogInLogOutClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    FRESTClient: IMVCRESTClient;
    FLogoutUrl: string;
    FLogoutMethod: string;
    procedure FillMemo(Response: IMVCRESTResponse);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

uses
  System.JSON,
  MVCFramework.SystemJSONUtils;

{$R *.dfm}

procedure TForm7.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  if FRESTClient.SessionID.IsEmpty then
  begin
    btnLogInLogOut.Caption := 'LOGIN';
    Panel1.Caption := 'Not Logged';
    edtUsername.Enabled := True;
    edtPassword.Enabled := True;
  end
  else
  begin
    btnLogInLogOut.Caption := 'LOGOUT';
    Panel1.Caption := 'SessionID = ' + FRESTClient.SessionID;
    edtUsername.Enabled := False;
    edtPassword.Enabled := False;
  end;

end;

procedure TForm7.btnLogInLogOutClick(Sender: TObject);
var
  lJObj: TJSONObject;
  lRes: IMVCRESTResponse;
begin
  if btnLogInLogOut.Caption = 'LOGIN' then
  begin
    lJObj := TJSONObject.Create;
    try
      lJObj.AddPair('username', edtUsername.Text);
      lJObj.AddPair('password', edtPassword.Text);
      lRes := FRESTClient.Post('/system/users/logged', TSystemJSON.JSONValueToString(lJObj, False));
      if not lRes.Success then
      begin
        ShowMessage(lRes.Content);
      end;
      FLogoutUrl := lRes.HeaderValue('X-LOGOUT-URL');
      FLogoutMethod := lRes.HeaderValue('X-LOGOUT-METHOD');
    finally
      lJObj.Free;
    end;
  end
  else
  begin
    Assert(FLogoutMethod = 'DELETE');
    lRes := FRESTClient.Delete(FLogoutUrl);
    if not lRes.Success then
    begin
      ShowMessage(lRes.Content);
    end;
  end;
end;

procedure TForm7.Button1Click(Sender: TObject);
var
  lRes: IMVCRESTResponse;
begin
  lRes := FRESTClient.Get('/private/public/action');
  FillMemo(lRes);
end;

procedure TForm7.Button2Click(Sender: TObject);
var
  lRes: IMVCRESTResponse;
begin
  lRes := FRESTClient.Get('/private/role1');
  FillMemo(lRes);
end;

procedure TForm7.Button3Click(Sender: TObject);
var
  lRes: IMVCRESTResponse;
begin
  lRes := FRESTClient.Get('/private/role2');
  FillMemo(lRes);
end;

procedure TForm7.Button4Click(Sender: TObject);
var
  lRes: IMVCRESTResponse;
begin
  lRes := FRESTClient.Get('/private/role1and2');
  FillMemo(lRes);
end;

procedure TForm7.Button5Click(Sender: TObject);
var
  lRes: IMVCRESTResponse;
begin
  lRes := FRESTClient.Get('/private/role/admin');
  FillMemo(lRes);
end;

procedure TForm7.Button6Click(Sender: TObject);
var
  lRes: IMVCRESTResponse;
begin
  lRes := FRESTClient.Get('/private/authenticatedOnly');
  FillMemo(lRes);
end;

procedure TForm7.Button7Click(Sender: TObject);
var
  lRes: IMVCRESTResponse;
begin
  lRes := FRESTClient.Get('/private/role1or2');
  FillMemo(lRes);
end;

procedure TForm7.FillMemo(Response: IMVCRESTResponse);
begin
  Memo1.Lines.Add(Format('[%s] [%s] %s', [TimeToStr(Time),
    Response.StatusText, Response.Content]));
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  FRESTClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
end;

procedure TForm7.ListBox1Click(Sender: TObject);
var
  lText: string;
  lPieces: TArray<string>;
begin
  if ListBox1.ItemIndex > -1 then
  begin
    lText := ListBox1.Items[ListBox1.ItemIndex];
    lPieces := lText.Split([':']);
    edtUsername.Text := lPieces[0];
    edtPassword.Text := lPieces[1];
    ShowMessage('Now you can log in using the login/logout button');
  end;
end;

end.
