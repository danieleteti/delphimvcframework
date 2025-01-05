program hmacsample;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  MVCFramework.HMAC in '..\..\sources\MVCFramework.HMAC.pas',
  MVCFramework.Commons in '..\..\sources\MVCFramework.Commons.pas';

var
  lAlg: String;
  lValue: String;
  i: Integer;
  AlgAndSign: array [0 .. 4] of array [0 .. 1] of string =
    (
    (
      'md5',
      '5256311089fa9c80f735fb8cc28bf4fe'
    ),
    (
      'sha1',
      '323ff5f4e53c43f2d9342952299a9d35f9ee5dc2'
    ),
    (
      'sha224',
      '2f42e18342d2d35afc9942364caec009e1ace1d1695c3e9178e65e35'
    ),
    (
      'sha256',
      '1f75a969e2b9c43e6d06969dfad2088f9aab68d3aa440904d2ed8710e2f8e38b'
    ),
    (
      'sha512',
      '22465b5f4138ab80801ff8eca8dd99a56844dd7dc54f76d38bb02bdd815596fc5859709ba4f7130c299a626864a84a4a79401f529d44c85a894fcd7e6192eee9'
    )
  );

begin
  ReportMemoryLeaksOnShutdown := true;
  try
    for I := Low(AlgAndSign) to High(AlgAndSign) do
    begin
      lAlg := AlgAndSign[I][0];
      lValue := AlgAndSign[I][1];
      Assert(lValue = BytesToHex(HMAC(lAlg, 'Daniele Teti', 'daniele')));
      WriteLn(Format('ALG NAME: %-15s '#13#10'VALUE: "%s"'#13#10#13#10,[lAlg, lValue]));
    end;
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
