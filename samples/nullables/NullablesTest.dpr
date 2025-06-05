program NullablesTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  MVCFramework.Console,
  MVCFramework.Nullables,
  System.DateUtils;

procedure Nullable;
begin
  TextColor(TConsoleColor.Red);
  Writeln('Nullable[Type]');
  SetDefaultColors;

  var lNullInt: NullableInteger;
  Assert(lNullInt.IsNull);
  Assert(lNullInt = nil);
  Assert(not lNullInt.HasValue);

  Writeln('lNullInt.ValueOrDefault = ', lNullInt.ValueOrDefault);
  var lInt := lNullInt.ValueOrDefault;
  Writeln('lInt = ', lInt); //print 0 because 0 is the default value for an integer

  lNullInt := 123; //implicit assign compatibility with the wrapped type
  Assert(not lNullInt.IsNull);
  Assert(lNullInt.HasValue);

  lInt := lNullInt;
  Writeln('lInt = ', lInt); //print 0 because 0 is the default value for an integer
  Writeln('lNullInt.Value = ', lNullInt.Value); //print 123
  Writeln('Integer(lNullInt) = ', Integer(lNullInt)); //print 123

  lNullInt.CheckHasValue; //raise exception if value is null

  Assert(lNullInt.ValueOrElse(-1) = 123); //similar to "ValueOrDefault" but in case of null returns the value passed as parameter
  lNullInt.SetNull; //Clear is another alias
  Assert(lNullInt.ValueOrElse(-1) = -1);

  lNullInt := 123;
  lNullInt := nil; //like SetNull
  Assert(lNullInt.IsNull);
  Assert(lNullInt = nil);
end;

procedure GenericNullable;
begin
  TextColor(TConsoleColor.Red);
  Writeln('Nullable<T>');
  SetDefaultColors;

  var lNullInt: Nullable<Integer>;
  Assert(lNullInt.IsNull);
  Assert(lNullInt = nil);
  Assert(not lNullInt.HasValue);

  Writeln('lNullInt.ValueOrDefault = ', lNullInt.ValueOrDefault);
  var lInt := lNullInt.ValueOrDefault;
  Writeln('lInt = ', lInt); //print 0 because 0 is the default value for an integer

  lNullInt := 123; //implicit assign compatibility with the wrapped type
  Assert(not lNullInt.IsNull);
  Assert(lNullInt.HasValue);

  lInt := lNullInt;
  Writeln('lInt = ', lInt); //print 0 because 0 is the default value for an integer
  Writeln('lNullInt.Value = ', lNullInt.Value); //print 123
  Writeln('Integer(lNullInt) = ', Integer(lNullInt)); //print 123

  lNullInt.CheckHasValue; //raise exception if value is null

  Assert(lNullInt.ValueOrElse(-1) = 123); //similar to "ValueOrDefault" but in case of null returns the value passed as parameter
  lNullInt.SetNull; //Clear is another alias
  Assert(lNullInt.ValueOrElse(-1) = -1);

  lNullInt := 123;
  lNullInt := nil; //like SetNull
  Assert(lNullInt.IsNull);
  Assert(lNullInt = nil);

  var lNullStr1: Nullable<String> := 'Hello World';
  var lNullStr2: Nullable<String> := 'Hello Xorld';
  Assert(not lNullStr1.Equals(lNullStr2));
end;


//procedure MyProc(aInt: Nullable<Integer>; aDate: Nullable<TDate>; aString: Nullable<String>);
//begin
//
//end;

procedure MyProc(aInt: NullableInteger; aDate: NullableTDate; aString: NullableString);
begin
  TextColor(TConsoleColor.Red);
  Writeln('Nullables as Arguments');
  SetDefaultColors;

  var lDateAsStr := DateToStr(aDate.ValueOrElse(Date));
  Writeln(Format('Got %d at date %s with a string %s', [aInt.ValueOrDefault, lDateAsStr, aString.ValueOrDefault]));
end;


procedure Main;
begin
  GenericNullable;
  Nullable;
  MyProc(1, nil, 'Hello');
end;

begin
  try
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  GetCh;
end.
