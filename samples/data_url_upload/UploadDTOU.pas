unit UploadDTOU;

interface

uses
  MVCFramework.Validators,
  MVCFramework.Serializer.Commons,
  DataURLTypeU;

type
  TUploadDTO = class
  private
    FName: string;
    FFileDataURL: TDataURL;
  public
    [MVCNameAs('name')]
    [MVCRequired]
    [MVCNotEmpty]
    property Name: string read FName write FName;
    [MVCRequired]
    [MVCNameAs('file')]
    property FileDataURL: TDataURL read FFileDataURL write FFileDataURL;
  end;

  TUploadResultDTO = class
  private
    FName: string;
    FMimeType: string;
    FSize: Integer;
    FSavedAs: string;
  public
    [MVCNameAs('name')]
    property Name: string read FName write FName;
    [MVCNameAs('mimeType')]
    property MimeType: string read FMimeType write FMimeType;
    [MVCNameAs('size')]
    property Size: Integer read FSize write FSize;
    [MVCNameAs('savedAs')]
    property SavedAs: string read FSavedAs write FSavedAs;
  end;

implementation

end.
