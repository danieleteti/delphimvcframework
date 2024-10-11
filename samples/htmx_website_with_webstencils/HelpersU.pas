unit HelpersU;

interface


type
  TMyMustacheHelpers = class sealed
  public
    class procedure MyHelper1(const Value: variant; out Result: variant);
    class procedure MyHelper2(const Value: variant; out Result: variant);
  end;

implementation

uses
  MVCFramework.View.Renderers.Mustache, System.SysUtils;

{ TMyMustacheHelpers }

class procedure TMyMustacheHelpers.MyHelper1(const Value: variant; out Result: variant);
begin
  Result := Value +  ' (I''m The MyHelper1)';
end;

class procedure TMyMustacheHelpers.MyHelper2(const Value: variant; out Result: variant);
begin
  Result := Value +  ' (I''m The MyHelper2)';
end;


end.
