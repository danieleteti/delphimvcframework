unit UtilsU;

interface

const
  DEFAULT_PROJECT_NAME = 'NewEntitiesGeneratorProject';


function IsReservedKeyword(const Value: String): Boolean;


implementation

uses
  System.SysUtils;

const PASCAL_KEYWORDS = ';and;array;as;as;asm;begin;break;case;class;class;const;' +
  'constref;constref;constructor;continue;destructor;dispose;dispose;div;do;downto;' +
  'else;end;except;except;exit;exit;exports;false;file;finalization;finally;for;function;' +
  'goto;if;implementation;in;inherited;initialization;inline;interface;is;label;' +
  'library;mod;new;nil;not;object;of;on;on;operator;or;out;packed;procedure;program;' +
  'property;raise;record;reference;repeat;self;set;shl;shr;string;then;threadvar;to;' +
  'true;try;type;unit;until;uses;var;while;with;xor;';



function IsReservedKeyword(const Value: String): Boolean;
begin
  Result := PASCAL_KEYWORDS.Contains(';' + Value.ToLower + ';');
end;

end.
