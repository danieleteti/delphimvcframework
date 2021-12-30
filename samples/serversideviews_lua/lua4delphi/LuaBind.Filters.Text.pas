{$WARNINGS OFF}
unit LuaBind.Filters.Text;

interface

uses
  System.SysUtils;

type
  TLuaEmbeddedTextFilter = class
  private
    FTemplateCode: string;
    FLuaCode: string;
    FCurrCharIndex: int64;
    FOutputStringBuilder: TStringBuilder;
    FCurrChar: Char;
    FOutputFunction: string;
    procedure SetLuaCode(const Value: string);
    procedure SetTemplateCode(const Value: string);
    procedure NextChar;
    function LookAhead(Offset: Integer): Char;
    procedure PushToOutput(const AChar: Char);
    procedure SetOutputFunction(const Value: string);

  public
    constructor Create;
    procedure Execute;
    property TemplateCode: string read FTemplateCode write SetTemplateCode;
    property LuaCode: string read FLuaCode write SetLuaCode;
    property OutputFunction: string read FOutputFunction write SetOutputFunction;
    // helpers
    class function ExecuteWithResult(eLuaScript: AnsiString; const ParamNames: array of string;
      const ParamValues: array of string): string;
  end;

implementation

uses
  LuaBind.Intf,
  System.Classes,
  LuaBind;

threadvar _OutputBuffer: TStringBuilder;

function __lua_stream_out(L: Plua_State): Integer; cdecl;
var
  s: string;
  o: TObject;
  stream: TStreamWriter;
begin
  if lua_gettop(L) <> 2 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit(0);
  end;

  if lua_isstring(L, - 1) = 1 then
  begin
    s := lua_tostring(L, - 1);
    lua_pop(L, 1);
  end
  else
  begin
    luaL_error(L, PAnsiChar('Type mismatch, expected String'));
    Exit(0);
  end;

  if lua_islightuserdata(L, - 1) then
  begin
    o := TObject(lua_topointer(L, - 1));
    lua_pop(L, 1);
  end
  else
  begin
    luaL_error(L, PAnsiChar('Type mismatch, expected LightUserData'));
    Exit(0);
  end;

  stream := o as TStreamWriter;
  stream.Write(s);
  Result := 0;
end;

{ TLuaEmbeddedTextFilter }

constructor TLuaEmbeddedTextFilter.Create;
begin
  inherited;
  FOutputFunction := 'response:out';
end;

procedure TLuaEmbeddedTextFilter.Execute;
var
  StartCode: Boolean;
  State: Integer;
const
  PARSER_VERBATIM_TEXT = $8000;
  PARSER_VERBATIM_CODE = PARSER_VERBATIM_TEXT + 1;
  PARSER_VERBATIM_EXPRESSION = PARSER_VERBATIM_CODE + 1;
  PARSER_VERBATIM_CODE_STRING_SINGLE_QUOTED = PARSER_VERBATIM_EXPRESSION + 1;
  PARSER_VERBATIM_CODE_STRING_DOUBLE_QUOTED = PARSER_VERBATIM_CODE_STRING_SINGLE_QUOTED + 1;
begin
  FCurrCharIndex := 0;
  State := PARSER_VERBATIM_TEXT;

  NextChar;
  FOutputStringBuilder := TStringBuilder.Create;
  try
    StartCode := True;
    while FCurrChar <> #0 do
    begin
      case State of
        PARSER_VERBATIM_TEXT:
          begin
            if FCurrChar = '<' then
            begin
              if (LookAhead(1) = '?') and (LookAhead(2) = 'l') and (LookAhead(3) = 'u') and
                (LookAhead(4) = 'a') then
              begin
                NextChar; // ?
                NextChar; // l
                NextChar; // u
                NextChar; // a
                NextChar; // _
                if not StartCode then
                  FOutputStringBuilder.Append(']]');
                StartCode := False;

                if FCurrChar = '=' then // expression
                begin
                  State := PARSER_VERBATIM_EXPRESSION;
                  FOutputStringBuilder.Append(FOutputFunction + '(');
                  NextChar;
                end
                else
                  State := PARSER_VERBATIM_CODE;
              end
              else
              begin
                if StartCode then
                begin
                  StartCode := False;
                  FOutputStringBuilder.Append(FOutputFunction + ' [[' + sLineBreak);
                end;
                PushToOutput('<');
                NextChar;
              end;
            end
            else // FCurrChar <> '<'
            begin
              if StartCode then
              begin
                StartCode := False;
                FOutputStringBuilder.Append(FOutputFunction + ' [[' + sLineBreak);
              end;
              PushToOutput(FCurrChar);
              NextChar;
            end;
          end;

        PARSER_VERBATIM_CODE_STRING_SINGLE_QUOTED:
          begin
            if FCurrChar = '''' then
            begin
              State := PARSER_VERBATIM_CODE;
              PushToOutput('''');
              NextChar;
            end
            else if (FCurrChar = '\') and (LookAhead(1) = '''') then
            begin
              PushToOutput('\');
              PushToOutput('''');
              NextChar;
              NextChar;
            end
            else
            begin
              PushToOutput(FCurrChar);
              NextChar;
            end;
          end;

        PARSER_VERBATIM_CODE_STRING_DOUBLE_QUOTED:
          begin
            if FCurrChar = '"' then
            begin
              State := PARSER_VERBATIM_CODE;
              PushToOutput('"');
              NextChar;
            end
            else if (FCurrChar = '\') and (LookAhead(1) = '"') then
            begin
              PushToOutput('\');
              PushToOutput('"');
              NextChar;
              NextChar;
            end
            else
            begin
              PushToOutput(FCurrChar);
              NextChar;
            end;
          end;

        PARSER_VERBATIM_CODE:
          begin
            if FCurrChar = '''' then
            begin
              State := PARSER_VERBATIM_CODE_STRING_SINGLE_QUOTED;
              PushToOutput('''');
              NextChar;
            end
            else if FCurrChar = '"' then
            begin
              State := PARSER_VERBATIM_CODE_STRING_DOUBLE_QUOTED;
              PushToOutput('"');
              NextChar;
            end
            else if FCurrChar = '?' then
            begin
              if (LookAhead(1) = '>') then
              begin
                NextChar; // >
                NextChar; // _
                FOutputStringBuilder.Append(sLineBreak + FOutputFunction + ' [[' + sLineBreak);
                State := PARSER_VERBATIM_TEXT;
              end
              else
              begin
                PushToOutput('?');
                NextChar;
              end;
            end
            else // FCurrChar <> '?'
            begin
              PushToOutput(FCurrChar);
              NextChar;
            end;
          end; // PARSER_VERBATIM_CODE

        PARSER_VERBATIM_EXPRESSION:
          begin
            if FCurrChar = '?' then
            begin
              if (LookAhead(1) = '>') then
              begin
                NextChar; // >
                NextChar; // _
                FOutputStringBuilder.Append(')');
                FOutputStringBuilder.Append(sLineBreak + FOutputFunction + ' [[' + sLineBreak);
                State := PARSER_VERBATIM_TEXT;
              end
              else
              begin
                PushToOutput('?');
                NextChar;
              end;
            end
            else // FCurrChar <> '?'
            begin
              PushToOutput(FCurrChar);
              NextChar;
            end;
          end;
      end; // case
    end; // while

    if (State = PARSER_VERBATIM_TEXT) and (not StartCode) then
      FOutputStringBuilder.Append(']]')
    else if not StartCode then
      raise ELuaFilterException.Create('Expected closing Lua tag');

    FLuaCode := FOutputStringBuilder.ToString;
  finally
    FOutputStringBuilder.Free;
  end;
end;

function __lua_out(L: Plua_State): Integer; cdecl;
var
  s: string;
begin
  if lua_gettop(L) <> 1 then
  begin
    luaL_error(L, PAnsiChar('Wrong parameters number'));
    Exit(0);
  end;

  if lua_isstring(L, - 1) = 1 then
  begin
    s := lua_tostring(L, - 1);
    lua_pop(L, 1);
  end
  else
  begin
    luaL_error(L, PAnsiChar('Type mismatch, expected String'));
    Exit(0);
  end;
  _OutputBuffer.Append(s);
  Result := 0;
end;

class function TLuaEmbeddedTextFilter.ExecuteWithResult(eLuaScript: AnsiString;
  const ParamNames, ParamValues: array of string): string;
var
  LuaFilter: TLuaEmbeddedTextFilter;
  L: TLuaEngine;
  I: Integer;

  function GetOutputBuffer: TStringBuilder;
  begin
    Result := _OutputBuffer;
  end;

begin
  if Length(ParamNames) <> Length(ParamValues) then
    raise ELuaRuntimeException.Create('Number of params names and param values is not equals');

  LuaFilter := TLuaEmbeddedTextFilter.Create;
  try
    LuaFilter.OutputFunction := '_out';
    LuaFilter.TemplateCode := eLuaScript;
    LuaFilter.Execute;

    _OutputBuffer := TStringBuilder.Create;
    try
      L := TLuaEngine.Create;
      try
        for I := 0 to Length(ParamNames) - 1 do
          L.DeclareGlobalString(ParamNames[I], ParamValues[I]);
        L.DeclareGlobalFunction('_out', @__lua_out);
        L.LoadScript(LuaFilter.LuaCode);
        L.Execute;
        Result := _OutputBuffer.ToString;
      finally
        L.Free;
      end;
    finally
      FreeAndNil(_OutputBuffer);
    end;
  finally
    LuaFilter.Free;
  end;
end;

function TLuaEmbeddedTextFilter.LookAhead(Offset: Integer): Char;
begin
  if FCurrCharIndex + Offset <= Length(FTemplateCode) then
    Result := FTemplateCode[FCurrCharIndex + Offset]
  else
    Result := #0;
end;

procedure TLuaEmbeddedTextFilter.NextChar;
begin
  if FCurrCharIndex = Length(FTemplateCode) then
  begin
    FCurrCharIndex := - 1;
    FCurrChar := #0;
  end
  else
  begin
    FCurrCharIndex := FCurrCharIndex + 1;
    FCurrChar := FTemplateCode[FCurrCharIndex];
  end;
end;

procedure TLuaEmbeddedTextFilter.PushToOutput(const AChar: Char);
begin
  FOutputStringBuilder.Append(AChar);
end;

procedure TLuaEmbeddedTextFilter.SetLuaCode(const Value: string);
begin
  FLuaCode := Value;
end;

procedure TLuaEmbeddedTextFilter.SetOutputFunction(const Value: string);
begin
  FOutputFunction := Value;
end;

procedure TLuaEmbeddedTextFilter.SetTemplateCode(const Value: string);
begin
  FTemplateCode := Value;
end;

end.
