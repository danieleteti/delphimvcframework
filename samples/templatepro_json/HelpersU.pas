unit HelpersU;

interface

uses
  System.Rtti, TemplatePro;

function Age(const Value: TValue; const Parameters: TArray<TFilterParameter>): TValue;


procedure TemplateProContextConfigure;

implementation

uses
  System.SysUtils, System.DateUtils, MVCFramework.Serializer.Commons;


function Age(const Value: TValue; const Parameters: TArray<TFilterParameter>): TValue;
begin
  //DOB is a string formatted as yyyy-mm-dd
  Result := YearsBetween(Now, ISODateToDate(Value.AsString));
end;


procedure TemplateProContextConfigure;
begin
  TTProConfiguration.OnContextConfiguration := procedure(const CompiledTemplate: ITProCompiledTemplate)
  begin
    // These filters will be available to the TemplatePro views as if they were the standard ones
    CompiledTemplate.AddFilter('age', Age);
  end;
end;


end.
