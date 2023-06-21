unit MVCFramework.Filters;

interface

uses
  MVCFramework.Commons,
  System.Generics.Collections,
  MVCFramework.Logger,
  MVCFramework;

type
  { Protocol Filters }
  IProtocolFilter = interface
    ['{43000B86-A2EC-49F1-99CD-86C7F45026FA}']
    procedure DoFilter(Context:  TWebContext);
    procedure SetNext(NextFilter: IProtocolFilter);
  end;

  IProtocolFilterChain = interface
    ['{AC865ED9-092F-46F6-A22B-CDE2964A9097}']
    procedure Execute(Context: TWebContext);
  end;

  IProtocolFilterChainBuilder = interface
    ['{3589290E-4305-4FDE-BCC0-9F4D8155E531}']
    function Use(Filter: IProtocolFilter): IProtocolFilterChainBuilder;
    function Build: IProtocolFilterChain;
  end;
  { END - Protocol Filters }

  { Controller Filters}
  IControllerFilter = interface
    ['{F47DDC56-7631-4838-AD8C-22883269197E}']
    procedure DoFilter(Context:  TWebContext);
    procedure SetNext(NextFilter: IControllerFilter);
  end;

  IControllerFilterChain = interface
    ['{2BEE7B64-10BE-4293-B02D-878CC3DF4D9E}']
    procedure Execute(Context: TWebContext);
  end;

  IControllerFilterChainBuilder = interface
    ['{41F0524A-534E-4900-9724-FD514D34B219}']
    function Use(Filter: IFilter): IFilterChainBuilder;
    function Build: IFilterChain;
  end;
  { END - Controller Filters}


  TProtocolFilter = class abstract(TInterfacedObject, IProtocolFilter)
  private
    fNext: IProtocolFilter;
  protected
    procedure DoNext(Context: TWebContext);
    procedure DoFilter(Context: TWebContext); virtual; abstract;
    procedure SetNext(NextFilter: IProtocolFilter);
  end;


  TProtocolFilterChain = class(TInterfacedObject, IProtocolFilterChainBuilder, IProtocolFilterChain)
  private
    fItems: TList<IProtocolFilter>;
  public
    constructor Create;
    destructor Destroy; override;
    function Use(Filter: IProtocolFilter): IProtocolFilterChainBuilder;
    procedure Execute(Context:  TWebContext);
    function Build: IProtocolFilterChain;
  end;

  TFilter1 = class(TFilter)
  protected
    procedure DoFilter(Context:  TWebContext); override;
  end;

  TFilter2 = class(TFilter)
  protected
    procedure DoFilter(Context:  TWebContext); override;
  end;

  TFilter3 = class(TFilter)
  protected
    procedure DoFilter(Context:  TWebContext); override;
  end;

implementation

{ TFilter1 }

procedure TFilter1.DoFilter(Context:  TWebContext);
begin
  begin var lProf := Profiler.Start(ClassName + '.DoFilter');
    DoNext(Context);
  end;
end;

{ TFilter2 }

procedure TFilter2.DoFilter(Context:  TWebContext);
begin
  begin var lProf := Profiler.Start(ClassName + '.DoFilter');
    DoNext(Context);
  end;

end;

{ TProtocolFilterChain }

function TProtocolFilterChain.Build: IProtocolFilterChain;
begin
  for var I := 0 to fItems.Count - 2 do
  begin
    fItems[I].SetNext(fItems[I+1]);
  end;
  Result := Self;
end;

constructor TProtocolFilterChain.Create;
begin
  inherited;
  fItems := TList<IProtocolFilter>.Create;
end;

destructor TProtocolFilterChain.Destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TProtocolFilterChain.Execute(Context:  TWebContext);
begin
  fItems.First.DoFilter(Context);
end;

function TProtocolFilterChain.Use(Filter: IProtocolFilter): IProtocolFilterChainBuilder;
begin
  fItems.Add(Filter);
  Result := Self;
end;

{ TFilter }

procedure TProtocolFilter.DoNext(Context:  TWebContext);
begin
  if Assigned(fNext) then
  begin
    fNext.DoFilter(Context);
  end;
end;

procedure TProtocolFilter.SetNext(NextFilter: IProtocolFilter);
begin
  fNext := NextFilter;
end;

{ TFilter3 }

procedure TFilter3.DoFilter(Context: TWebContext);
begin
  begin var lProf := Profiler.Start(ClassName + '.DoFilter');
    DoNext(Context);
  end;
end;

end.
