unit DiscogsClasses;

interface

uses
  System.JSON,
  Generics.Collections,
  ObjectsMappers;

type
  TDiscogsArtist = class;
  TDiscogsFormat = class;
  TDiscogsLabel  = class;
  TDiscogsCollectionRelease = class;

  TDiscogsReleaseBasicInformation = class;

  TDiscogsCollectionReleases = class( TObjectList<TDiscogsCollectionRelease> )
  private
  end;

  TDiscogsCollectionRelease = class
  private
    Fbasic_information: TDiscogsReleaseBasicInformation;
    Finstance_id: Integer;
    Ffolder_id: Integer;
    Frating: Integer;
    Fid: Integer;
    procedure Setbasic_information(
      const Value: TDiscogsReleaseBasicInformation);
    procedure Setfolder_id(const Value: Integer);
    procedure Setid(const Value: Integer);
    procedure Setinstance_id(const Value: Integer);
    procedure Setrating(const Value: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property basic_information : TDiscogsReleaseBasicInformation read Fbasic_information write Setbasic_information;
    property folder_id : Integer read Ffolder_id write Setfolder_id;
    property instance_id : Integer read Finstance_id write Setinstance_id;
    property id : Integer read Fid write Setid;
    property rating : Integer read Frating write Setrating;
  end;


  TDiscogsArtist = class( TObject )
  private
    FName: String;
    FResourceURL: String;
    FJoin: String;
    FAnv: String;
    FRole: String;
    FId: Integer;
    FTracks: String;
  protected
    procedure SetAnv(const Value: String);
    procedure SetId(const Value: Integer);
    procedure SetJoin(const Value: String);
    procedure SetName(const Value: String);
    procedure SetResourceURL(const Value: String);
    procedure SetRole(const Value: String);
    procedure SetTracks(const Value: String);
  public
  published
    property Id          : Integer read FId write SetId;
    property ANV         : String read FAnv write SetAnv;
    property Join        : String read FJoin write SetJoin;
    [MapperJSONSer('name')] //the mapper is case sensitive... just rename it using the correct name/case
    property Name        : String read FName write SetName;
    property resource_url : String read FResourceURL write SetResourceURL;
    property Role        : String read FRole write SetRole;
    property Tracks      : String read FTracks write SetTracks;
  end;


 TDiscogsFormat = class( TObject )
  private
    FName: String;
    FQty: Integer;
    procedure SetName(const Value: String);
    procedure SetQty(const Value: Integer);
  public
  published
    property name : String read FName write SetName;
    property qty  : Integer read FQty write SetQty;
//    property Descirptions : TStringList;
  end;

  TDiscogsLabel = class( TObject )
  private
    FName: String;
    FResourceURL: String;
    FId: Integer;
    FEntityType: String;
    FCategoryNo: String;
    procedure SetCategoryNo(const Value: String);
    procedure SetEntityType(const Value: String);
    procedure SetId(const Value: Integer);
    procedure SetName(const Value: String);
    procedure SetResourceURL(const Value: String);
  public
  published
    property id : Integer read FId write SetId;
    property entityType : String read FEntityType write SetEntityType;
    property categoryNo : String read FCategoryNo write SetCategoryNo;
    property name : String read FName write SetName;
    property resource_url : String read FResourceURL write SetResourceURL;
  end;

  TDiscogsReleaseBasicInformation = class( TObject )
  private
    FYear: Integer;
    FResourceURL: String;
    FThumb: String;
    FTitle: String;
    FId: Integer;
    Fartists: TObjectList<TDiscogsArtist>;
    Flabels: TObjectList<TDiscogsLabel>;
    Fformats: TObjectList<TDiscogsFormat>;
    procedure SetId(const Value: Integer);
    procedure SetResourceURL(const Value: String);
    procedure SetThumb(const Value: String);
    procedure SetTitle(const Value: String);
    procedure SetYear(const Value: Integer);
    procedure Setartists(const Value: TObjectList<TDiscogsArtist>);
    procedure Setformats(const Value: TObjectList<TDiscogsFormat>);
    procedure Setlabels(const Value: TObjectList<TDiscogsLabel>);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property id : Integer read FId write SetId;
    property resource_url : String read FResourceURL write SetResourceURL;
    property thumb : String read FThumb write SetThumb;
    property title : String read FTitle write SetTitle;
    property year : Integer read FYear write SetYear;

    [MapperItemsClassType(TDiscogsArtist)]
    property artists : TObjectList<TDiscogsArtist> read Fartists write Setartists;
    [MapperItemsClassType(TDiscogsLabel)]
    property labels  : TObjectList<TDiscogsLabel> read Flabels write Setlabels;
    [MapperItemsClassType(TDiscogsFormat)]
    property formats : TObjectList<TDiscogsFormat> read Fformats write Setformats;
  end;




implementation


uses
  MVCFramework.Logger,
  SysUtils;

{ TDiscogsArtist }

procedure TDiscogsArtist.SetAnv(const Value: String);
begin
  FAnv := Value;
end;

procedure TDiscogsArtist.SetId(const Value: Integer);
begin
  FId := Value;
end;

procedure TDiscogsArtist.SetJoin(const Value: String);
begin
  FJoin := Value;
end;

procedure TDiscogsArtist.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TDiscogsArtist.SetResourceURL(const Value: String);
begin
  FResourceURL := Value;
end;

procedure TDiscogsArtist.SetRole(const Value: String);
begin
  FRole := Value;
end;

procedure TDiscogsArtist.SetTracks(const Value: String);
begin
  FTracks := Value;
end;

{ TDiscogsFormat }

procedure TDiscogsFormat.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TDiscogsFormat.SetQty(const Value: Integer);
begin
  FQty := Value;
end;

{ TDiscogsLabel }

procedure TDiscogsLabel.SetCategoryNo(const Value: String);
begin
  FCategoryNo := Value;
end;

procedure TDiscogsLabel.SetEntityType(const Value: String);
begin
  FEntityType := Value;
end;

procedure TDiscogsLabel.SetId(const Value: Integer);
begin
  FId := Value;
end;

procedure TDiscogsLabel.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TDiscogsLabel.SetResourceURL(const Value: String);
begin
  FResourceURL := Value;
end;

{ TDiscogsReleaseBasicInformation }

constructor TDiscogsReleaseBasicInformation.Create;
begin
  inherited;

  FArtists := TObjectList<TDiscogsArtist>.Create( True );
  FLabels  := TObjectList<TDiscogsLabel>.Create( True );
  FFormats := TObjectList<TDiscogsFormat>.Create( True );

end;

destructor TDiscogsReleaseBasicInformation.Destroy;
begin
  FreeAndNil( FArtists );
  FreeAndNil( FLabels );
  FreeAndNil( FFormats );

  inherited;
end;

procedure TDiscogsReleaseBasicInformation.Setartists(
  const Value: TObjectList<TDiscogsArtist>);
begin
  Fartists := Value;
end;

procedure TDiscogsReleaseBasicInformation.Setformats(
  const Value: TObjectList<TDiscogsFormat>);
begin
  Fformats := Value;
end;

procedure TDiscogsReleaseBasicInformation.SetId(const Value: Integer);
begin
  FId := Value;
end;

procedure TDiscogsReleaseBasicInformation.Setlabels(
  const Value: TObjectList<TDiscogsLabel>);
begin
  Flabels := Value;
end;

procedure TDiscogsReleaseBasicInformation.SetResourceURL(const Value: String);
begin
  FResourceURL := Value;
end;

procedure TDiscogsReleaseBasicInformation.SetThumb(const Value: String);
begin
  FThumb := Value;
end;

procedure TDiscogsReleaseBasicInformation.SetTitle(const Value: String);
begin
  FTitle := Value;
end;

procedure TDiscogsReleaseBasicInformation.SetYear(const Value: Integer);
begin
  FYear := Value;
end;


{ TDiscogsCollectionRelease }

constructor TDiscogsCollectionRelease.Create;
begin
  inherited Create;

  Fbasic_information := TDiscogsReleaseBasicInformation.Create;
end;

destructor TDiscogsCollectionRelease.Destroy;
begin
  FreeAndNil( Fbasic_information );
  inherited;
end;

procedure TDiscogsCollectionRelease.Setbasic_information(
  const Value: TDiscogsReleaseBasicInformation);
begin
  Fbasic_information := Value;
end;

procedure TDiscogsCollectionRelease.Setfolder_id(const Value: Integer);
begin
  Ffolder_id := Value;
end;

procedure TDiscogsCollectionRelease.Setid(const Value: Integer);
begin
  Fid := Value;
end;

procedure TDiscogsCollectionRelease.Setinstance_id(const Value: Integer);
begin
  Finstance_id := Value;
end;

procedure TDiscogsCollectionRelease.Setrating(const Value: Integer);
begin
  Frating := Value;
end;

end.
