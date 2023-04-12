unit MVCFramework.Middleware.Prometheus;

interface

uses
  MVCFramework, System.SysUtils;

type
  TMVCPrometheusMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fPrometheusRoute: String;
    fOnBeforeMetricsGet: TProc;
  protected
    procedure OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string;
      var AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  public
    constructor Create(const PrometheusRoute: String = '/metrics'; const OnBeforeMetricsGet: TProc = nil);
  end;

  {Simplified access to the most common Prometheus metrics}
  Metrics = class sealed
  public
    // "Counter" metric
    class procedure IncCounterValue(const MetricName: string; const Amount: Double = 1); overload;
    class procedure IncCounterValue(const MetricName: string; const Amount: Double; const Labels: TArray<String>); overload;
    // "Gauge" metric
    class procedure SetGaugeValue(const MetricName: string; const Amount: Double = 1); overload;
    class procedure SetGaugeValue(const MetricName: string; const Amount: Double; const Labels: TArray<String>); overload;
    class procedure IncGaugeValue(const MetricName: string; const Amount: Double = 1); overload;
    class procedure IncGaugeValue(const MetricName: string; const Amount: Double; const Labels: TArray<String>); overload;
    class procedure DecGaugeValue(const MetricName: string; const Amount: Double = -1); overload;
    class procedure DecGaugeValue(const MetricName: string; const Amount: Double; const Labels: TArray<String>); overload;
  end;


implementation

uses
  System.Classes,
  MVCFramework.Commons,
  Prometheus.Registry,
  Prometheus.Collectors.Counter,
  Prometheus.Exposers.Text, Prometheus.Collectors.Gauge;

{ TMVCPrometheusMiddleware }

constructor TMVCPrometheusMiddleware.Create(const PrometheusRoute: String; const OnBeforeMetricsGet: TProc);
begin
  inherited Create;
  fPrometheusRoute := PrometheusRoute;
  fOnBeforeMetricsGet := OnBeforeMetricsGet;
end;

procedure TMVCPrometheusMiddleware.OnAfterControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; const AHandled: Boolean);
begin

end;

procedure TMVCPrometheusMiddleware.OnAfterRouting(AContext: TWebContext;
  const AHandled: Boolean);
begin
  Metrics.IncCounterValue('http_requests_handled', 1,
    [AContext.Request.PathInfo, IntToStr(AContext.Response.StatusCode)]);
end;

procedure TMVCPrometheusMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin

end;

procedure TMVCPrometheusMiddleware.OnBeforeRouting(AContext: TWebContext;
  var AHandled: Boolean);
begin
  if not SameText(AContext.Request.PathInfo, fPrometheusRoute) then
  begin
    // Get the metric counter and increment it.
    Metrics.IncCounterValue('http_requests_count');
  end
  else
  begin
    if Assigned(fOnBeforeMetricsGet) then
    begin
      fOnBeforeMetricsGet();
    end;
    var lMetricsStream := TMemoryStream.Create;
    AContext.Response.SetContentStream(lMetricsStream, TMVCMediaType.TEXT_PLAIN);
    var LExposer := TTextExposer.Create;
    try
      LExposer.Render(lMetricsStream, TCollectorRegistry.DefaultRegistry.Collect);
    finally
      LExposer.Free;
    end;
    lMetricsStream.Position := 0;
    AHandled := True;
  end;
end;

{ Metrics }

class procedure Metrics.IncCounterValue(const MetricName: string; const Amount: Double);
begin
  TCollectorRegistry
    .DefaultRegistry
    .GetCollector<TCounter>(MetricName)
    .Inc(Amount);
end;

class procedure Metrics.DecGaugeValue(const MetricName: string;
  const Amount: Double; const Labels: TArray<String>);
begin
  TCollectorRegistry.DefaultRegistry
    .GetCollector<TGauge>(MetricName)
    .Labels(Labels)
    .Dec(Amount);
end;

class procedure Metrics.DecGaugeValue(const MetricName: string;
  const Amount: Double);
begin
  TCollectorRegistry.DefaultRegistry
    .GetCollector<TGauge>(MetricName)
    .Dec(Amount);
end;

class procedure Metrics.IncCounterValue(const MetricName: string; const Amount: Double;
  const Labels: TArray<String>);
begin
  TCollectorRegistry
    .DefaultRegistry
    .GetCollector<TCounter>(MetricName)
    .Labels(Labels)
    .Inc(Amount);
end;

class procedure Metrics.IncGaugeValue(const MetricName: string;
  const Amount: Double);
begin
  TCollectorRegistry.DefaultRegistry
    .GetCollector<TGauge>(MetricName)
    .Inc(Amount);
end;

class procedure Metrics.IncGaugeValue(const MetricName: string;
  const Amount: Double; const Labels: TArray<String>);
begin
  TCollectorRegistry.DefaultRegistry
    .GetCollector<TGauge>(MetricName)
    .Labels(Labels)
    .Inc(Amount);
end;

class procedure Metrics.SetGaugeValue(const MetricName: string;
  const Amount: Double; const Labels: TArray<String>);
begin
  TCollectorRegistry.DefaultRegistry
    .GetCollector<TGauge>(MetricName)
    .Labels(Labels)
    .SetTo(Amount);
end;

class procedure Metrics.SetGaugeValue(const MetricName: string;
  const Amount: Double);
begin
  TCollectorRegistry.DefaultRegistry
    .GetCollector<TGauge>(MetricName)
    .SetTo(Amount);
end;

end.
