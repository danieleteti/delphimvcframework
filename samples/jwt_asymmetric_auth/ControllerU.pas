// ***************************************************************************
//
// Delphi MVC Framework - JWT Asymmetric Auth Sample
//
// Controller with public and protected endpoints.
//
// ***************************************************************************

unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons;

type
  [MVCPath('/api')]
  TApiController = class(TMVCController)
  public
    [MVCPath('/public/info')]
    [MVCHTTPMethod([httpGET])]
    function PublicInfo: IMVCResponse;

    [MVCPath('/protected/profile')]
    [MVCHTTPMethod([httpGET])]
    function ProtectedProfile: IMVCResponse;

    [MVCPath('/protected/secret')]
    [MVCHTTPMethod([httpGET])]
    function ProtectedSecret: IMVCResponse;
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

function TApiController.PublicInfo: IMVCResponse;
begin
  Result := OkResponse(
    StrDict(
      ['message', 'algorithm', 'server_time'],
      ['This endpoint is public - no JWT required',
       'RS256 (RSA + SHA-256)',
       FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now)]
    )
  );
end;

function TApiController.ProtectedProfile: IMVCResponse;
begin
  // Context.LoggedUser is populated by the JWT middleware after token verification
  Result := OkResponse(
    StrDict(
      ['message', 'username', 'roles'],
      ['This endpoint requires a valid RS256 JWT',
       Context.LoggedUser.UserName,
       String.Join(', ', Context.LoggedUser.Roles.ToArray)]
    )
  );
end;

function TApiController.ProtectedSecret: IMVCResponse;
begin
  Result := OkResponse(
    StrDict(
      ['message', 'secret', 'note'],
      ['You have access to the secret!',
       'The cake is a lie',
       'This token was signed with a 2048-bit RSA private key and verified with the public key only']
    )
  );
end;

end.
