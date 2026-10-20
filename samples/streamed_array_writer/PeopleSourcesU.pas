unit PeopleSourcesU;

// ===========================================================================
//  Object sources shared by the controller.
//
//  TPerson            - the plain object streamed by the object-based endpoints.
//  TPersonFileSource  - a LAZY object enumerator. It reads a delimited text
//                       file one line at a time and yields one TPerson per
//                       line, so the whole file is never loaded and at most one
//                       TPerson exists at any instant. This is the realistic
//                       "enumerator of objects read from a file (or a DB
//                       cursor)" case: usable with a plain "for p in source do"
//                       loop, exactly like any TEnumerable<T>.
//  TPeopleRange       - a reusable generator that implements IEnumerable<TPerson>
//                       (the interface, not the TEnumerable<T> class): what
//                       StreamItems<T> takes as its second overload. It builds
//                       one TPerson per MoveNext and does NOT own it: the
//                       response frees each item after writing it.
// ===========================================================================

interface

uses
  System.Classes,
  System.Generics.Collections;

type
  TPerson = class
  private
    FID: Integer;
    FFullName: string;
    FCountry: string;
    FAge: Integer;
  public
    constructor Create(const AID: Integer; const AFullName, ACountry: string;
      const AAge: Integer);
    property ID: Integer read FID write FID;
    property FullName: string read FFullName write FFullName;
    property Country: string read FCountry write FCountry;
    property Age: Integer read FAge write FAge;
  end;

  /// <summary>
  /// Lazy enumerator over a delimited text file ("id;full_name;country;age").
  /// Owns the current TPerson: the previous one is freed when MoveNext
  /// advances, the last one when the enumerator is freed. The consumer must
  /// NOT free Current.
  /// </summary>
  TPersonFileEnumerator = class(TEnumerator<TPerson>)
  private
    FReader: TStreamReader;
    FCurrent: TPerson;
  protected
    function DoGetCurrent: TPerson; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
  end;

  /// <summary>
  /// A TEnumerable&lt;TPerson&gt; backed by a file. "for p in source do" opens
  /// a fresh lazy reader; nothing is materialized up front.
  /// </summary>
  TPersonFileSource = class(TEnumerable<TPerson>)
  private
    FFileName: string;
  protected
    function DoGetEnumerator: TEnumerator<TPerson>; override;
  public
    constructor Create(const AFileName: string);
  end;

  // IEnumerable<T> inherits the non-generic IEnumerable, and a class that lists
  // both does not accept the method resolution clause for the non-generic one.
  // So the non-generic halves live in two small base classes.
  TEnumerableBase = class(TInterfacedObject, IEnumerable)
  protected
    function IEnumerable.GetEnumerator = GetNonGenericEnumerator;
    function GetNonGenericEnumerator: IEnumerator; virtual; abstract;
  end;

  TEnumeratorBase = class(TInterfacedObject, IEnumerator)
  protected
    function IEnumerator.GetCurrent = GetCurrentObject;
    function GetCurrentObject: TObject; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;
    procedure Reset; virtual; abstract;
  end;

  TPeopleRangeEnumerator = class(TEnumeratorBase, IEnumerator<TPerson>)
  private
    FFirst: Integer;
    FNext: Integer;
    FLast: Integer;
    FCurrent: TPerson;
  protected
    function GetCurrent: TPerson;
    function GetCurrentObject: TObject; override;
    function MoveNext: Boolean; override;
    procedure Reset; override;
  public
    constructor Create(const AFirst, ALast: Integer);
  end;

  /// <summary>
  /// The people with ID AFirst..ALast, built one at a time. Every
  /// GetEnumerator starts again from AFirst.
  /// </summary>
  TPeopleRange = class(TEnumerableBase, IEnumerable<TPerson>)
  private
    FFirst: Integer;
    FLast: Integer;
  protected
    function GetEnumerator: IEnumerator<TPerson>;
    function GetNonGenericEnumerator: IEnumerator; override;
  public
    constructor Create(const AFirst, ALast: Integer);
  end;

implementation

uses
  System.SysUtils;

const
  RANGE_COUNTRIES: array [0 .. 3] of string = ('IT', 'FR', 'DE', 'ES');

{ TPerson }

constructor TPerson.Create(const AID: Integer; const AFullName, ACountry: string;
  const AAge: Integer);
begin
  inherited Create;
  FID := AID;
  FFullName := AFullName;
  FCountry := ACountry;
  FAge := AAge;
end;

{ TPersonFileEnumerator }

constructor TPersonFileEnumerator.Create(const AFileName: string);
begin
  inherited Create;
  // The reader streams the file; it never loads it whole.
  FReader := TStreamReader.Create(AFileName, TEncoding.UTF8);
end;

destructor TPersonFileEnumerator.Destroy;
begin
  FCurrent.Free; // free the last yielded record
  FReader.Free;
  inherited;
end;

function TPersonFileEnumerator.DoGetCurrent: TPerson;
begin
  Result := FCurrent;
end;

function TPersonFileEnumerator.DoMoveNext: Boolean;
var
  lLine: string;
  lParts: TArray<string>;
begin
  // Release the record produced by the previous iteration: only one TPerson
  // is alive at a time.
  FreeAndNil(FCurrent);
  while not FReader.EndOfStream do
  begin
    lLine := FReader.ReadLine; // one line in memory
    if lLine.Trim.IsEmpty then
      Continue;
    lParts := lLine.Split([';']);
    if Length(lParts) < 4 then
      Continue;
    FCurrent := TPerson.Create(lParts[0].ToInteger, lParts[1], lParts[2],
      lParts[3].ToInteger);
    Exit(True);
  end;
  Result := False;
end;

{ TPeopleRangeEnumerator }

constructor TPeopleRangeEnumerator.Create(const AFirst, ALast: Integer);
begin
  inherited Create;
  FFirst := AFirst;
  FNext := AFirst;
  FLast := ALast;
end;

function TPeopleRangeEnumerator.GetCurrent: TPerson;
begin
  Result := FCurrent;
end;

function TPeopleRangeEnumerator.GetCurrentObject: TObject;
begin
  Result := FCurrent;
end;

function TPeopleRangeEnumerator.MoveNext: Boolean;
begin
  Result := FNext <= FLast;
  if Result then
  begin
    // the person belongs to whoever consumes it: not freed here
    FCurrent := TPerson.Create(FNext, 'Person ' + IntToStr(FNext),
      RANGE_COUNTRIES[FNext mod Length(RANGE_COUNTRIES)], 18 + (FNext mod 60));
    Inc(FNext);
  end;
end;

procedure TPeopleRangeEnumerator.Reset;
begin
  FNext := FFirst;
end;

{ TPeopleRange }

constructor TPeopleRange.Create(const AFirst, ALast: Integer);
begin
  inherited Create;
  FFirst := AFirst;
  FLast := ALast;
end;

function TPeopleRange.GetEnumerator: IEnumerator<TPerson>;
begin
  Result := TPeopleRangeEnumerator.Create(FFirst, FLast);
end;

function TPeopleRange.GetNonGenericEnumerator: IEnumerator;
begin
  Result := TPeopleRangeEnumerator.Create(FFirst, FLast);
end;

{ TPersonFileSource }

constructor TPersonFileSource.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

function TPersonFileSource.DoGetEnumerator: TEnumerator<TPerson>;
begin
  Result := TPersonFileEnumerator.Create(FFileName);
end;

end.
