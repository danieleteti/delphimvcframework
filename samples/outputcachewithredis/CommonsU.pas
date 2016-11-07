unit CommonsU;

interface

uses
  System.SysUtils, System.Classes;

type
  EWrongPage = class(Exception)

  end;

procedure GetLimitByPage(const APage: Integer; out StartRec: Integer; out EndRec: Integer);
procedure MergePaginationMetaInfos(AURLFormat: String; AHeaders: TStrings; ACurrPage: Integer);

implementation

procedure MergePaginationMetaInfos(AURLFormat: String; AHeaders: TStrings; ACurrPage: Integer);
begin
  AHeaders.Values['dmvc-next-page'] :=
    Format(AURLFormat, [ACurrPage + 1]);
  if ACurrPage > 1 then
    AHeaders.Values['dmvc-prev-page'] :=
      Format(AURLFormat, [ACurrPage - 1]);
end;

procedure GetLimitByPage(const APage: Integer; out StartRec: Integer; out EndRec: Integer);
begin
  if APage < 1 then
    raise EWrongPage.Create('Page must be greater than 0');
  EndRec := APage * 10;
  StartRec := EndRec - 9;
end;

end.
