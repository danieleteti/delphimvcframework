unit HelpersU;

interface

uses
  System.Rtti;

function MyHelper1(const Value: TValue; const Parameters: TArray<string>): TValue;
function MyHelper2(const Value: TValue; const Parameters: TArray<string>): TValue;


procedure TemplateProContextConfigure;

implementation

uses
  TemplatePro, System.SysUtils;


function MyHelper1(const Value: TValue; const Parameters: TArray<string>): TValue;
begin
  Result := Value.ToString +  ' (I''m The MyHelper1)';
end;

function MyHelper2(const Value: TValue; const Parameters: TArray<string>): TValue;
begin
  Result := Value.ToString +  ' (I''m The MyHelper2)';
end;


procedure TemplateProContextConfigure;
begin
  TTProConfiguration.OnContextConfiguration := procedure(const CompiledTemplate: ITProCompiledTemplate)
  begin
    // These filters will be available to the TemplatePro views as if they were the standard ones
    CompiledTemplate.AddFilter('MyHelper1', MyHelper1);
    CompiledTemplate.AddFilter('MyHelper2', MyHelper2);

    CompiledTemplate.OnGetValue :=
      procedure(const DataSource, Members: string; var Value: TValue; var Handled: Boolean)
      begin
        if SameText(DataSource, 'ext1') then
        begin
          if Members.IsEmpty then
          begin
            Value := 'External Value Ext1'
          end
          else
          begin
            Value := 'Reading ext1.' + Members;
          end;
          Handled := True;
        end;
      end
  end;
end;


end.

