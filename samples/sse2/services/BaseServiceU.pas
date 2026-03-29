unit BaseServiceU;

interface

type
  TBaseService = class
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TBaseService }

constructor TBaseService.Create;
begin
  inherited;
end;

destructor TBaseService.Destroy;
begin

  inherited;
end;

end.
