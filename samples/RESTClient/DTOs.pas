unit DTOs;

interface

type
  // Domain Models for JSONPlaceholder API
  TPost = class
  private
    FUserId: Integer;
    FId: Integer;
    FTitle: string;
    FBody: string;
  public
    property UserId: Integer read FUserId write FUserId;
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Body: string read FBody write FBody;
  end;

  TUser = class
  private
    FId: Integer;
    FName: string;
    FUsername: string;
    FEmail: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Username: string read FUsername write FUsername;
    property Email: string read FEmail write FEmail;
  end;

  TComment = class
  private
    FPostId: Integer;
    FId: Integer;
    FName: string;
    FEmail: string;
    FBody: string;
  public
    property PostId: Integer read FPostId write FPostId;
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Body: string read FBody write FBody;
  end;

implementation

end.
