unit Controllers.BooksU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections;

type
  [MVCPath]
  TBooksController = class(TMVCController)
  public
    [MVCPath]
    [MVCPath('/search')]
    function Search([MVCFromQueryString('query','')] SearchQueryText: String): String;
  end;

implementation

uses
  System.StrUtils, System.SysUtils, MVCFramework.Logger,
  MVCFramework.ActiveRecord, JsonDataObjects,
  MVCFramework.HTMX,
  MVCFramework.Middleware.ActiveRecord,
  Data.DB;


{ TBooksController }

function TBooksController.Search(SearchQueryText: String): String;
var
  lDS: TDataSet;
  lBaseSelect, lOrdering: String;
begin
  SearchQueryText := SearchQueryText.ToLower;
  lBaseSelect := 'select id, book_name, author_name, genre from books';
  lOrdering := 'order by book_name COLLATE NOCASE';
  if SearchQueryText.IsEmpty then
  begin
    lDS := TMVCActiveRecord.SelectDataSet(lBaseSelect + ' ' + lOrdering, [], True);
  end
  else
  begin
    lDS := TMVCActiveRecord.SelectDataSet(
      lBaseSelect + ' where instr(lower(book_name), ?) > 0 or instr(lower(author_name), ?) > 0 or instr(lower(genre), ?) > 0 ' + lOrdering,
      [SearchQueryText, SearchQueryText, SearchQueryText], True);
  end;
  try
    ViewData['books'] := lDS;
    ViewData['books_count'] := lDS.RecordCount;
    if Context.Request.IsHTMX then
    begin
      Result := PageFragment(['search_results']);
    end
    else
    begin
      Result := Page(['index']);
    end;
  finally
    lDS.Free;
  end;
end;

end.
