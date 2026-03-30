unit AppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/public')]
    [MVCHTTPMethod([httpGET])]
    function PublicSection: IMVCResponse;

    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

  [MVCPath('/admin')]
  TAdminController = class(TMVCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string;
      var AHandled: Boolean); override;
  public
    [MVCPath('/role1')]
    [MVCHTTPMethod([httpGET])]
    function OnlyRole1: IMVCResponse;

    [MVCPath('/role2')]
    [MVCHTTPMethod([httpGET])]
    function OnlyRole2: IMVCResponse;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework.Serializer.Commons;

{ TApp1MainController }

procedure TApp1MainController.Index;
begin
  Redirect('/static/index.html');
end;

function TApp1MainController.PublicSection: IMVCResponse;
begin
  Result := OkResponse(
    StrDict(
      ['message'],
      ['This is a public section - no JWT required']
    )
  );
end;

{ TAdminController }

procedure TAdminController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  Assert(AContext.LoggedUser.CustomData['customkey1'] = 'customvalue1', 'customkey1 not valid');
  Assert(AContext.LoggedUser.CustomData['customkey2'] = 'customvalue2', 'customkey2 not valid');
  AHandled := False;
end;

function TAdminController.OnlyRole1: IMVCResponse;
begin
  Result := OkResponse(
    StrDict(
      ['message', 'username', 'roles', 'customkey1', 'customkey2'],
      ['This is protected content accessible only by role1',
       Context.LoggedUser.UserName,
       String.Join(', ', Context.LoggedUser.Roles.ToArray),
       Context.LoggedUser.CustomData['customkey1'],
       Context.LoggedUser.CustomData['customkey2']]
    )
  );
end;

function TAdminController.OnlyRole2: IMVCResponse;
begin
  Result := OkResponse(
    StrDict(
      ['message', 'username', 'roles'],
      ['This is protected content accessible only by role2',
       Context.LoggedUser.UserName,
       String.Join(', ', Context.LoggedUser.Roles.ToArray)]
    )
  );
end;

end.
