program NullableTypesShowcase;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  MVCFramework.Nullables;


procedure Main;
begin
  var lNullableInt: NullableInt32;
  Assert(lNullableInt.IsNull);
  Assert(not lNullableInt.HasValue);
  Assert(lNullableInt.ValueOrDefault = 0);
  lNullableInt := 3; // compatible with base type
  Assert(not lNullableInt.IsNull);
  Assert(lNullableInt.HasValue);
  Assert(lNullableInt.ValueOrDefault = 3);
  Assert(lNullableInt.Value = 3);

  var lVal: Integer;
  if lNullableInt.TryHasValue(lVal) then
  begin
    // work with value...
    {
    TryHasValue is a shortcut for the following code:
    if lNullableInt.HasValue then
    begin
      var lVal := lNullableInt.Value;
      // work with value...
    end;
    }
  end;

  lVal := lNullableInt; // compatible with base type - equivalent to lNullableInt.Value
  {
    lNullableInt.Value raises exception if "lNullableInt.IsNull = True"
  }

  lNullableInt := nil; // set to null
  Assert(lNullableInt.IsNull);
  Assert(not lNullableInt.HasValue);
  lNullableInt := 123;
  Assert(lNullableInt.HasValue);

  lNullableInt.SetNull; // set to null
  Assert(lNullableInt.IsNull);
  Assert(not lNullableInt.HasValue);

  lNullableInt.Clear; // set to null
  Assert(lNullableInt.IsNull);
  Assert(not lNullableInt.HasValue);
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
  Write('Press return to continue');
  Readln;
end.
