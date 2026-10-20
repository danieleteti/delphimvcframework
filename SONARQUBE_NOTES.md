# SonarQube su DMVC: note operative

Stato al 16/10/2026. Sorgenti analizzati: `C:\dev\dmvcframework\sources\`. Tutto quello che segue l'ho verificato leggendo il server, tranne dove è scritto diversamente.

## Ambiente

- SonarQube Community Build 26.10.0 in Docker, container `sonarqube`, porta 9000.
- Plugin Delphi `sonar-delphi-plugin-1.21.0.jar` in `/opt/sonarqube/extensions/plugins/`. La cartella `extensions` è su volume Docker, quindi sopravvive ai riavvii. I plugin si caricano solo all'avvio: dopo averne messo uno nuovo serve `docker restart sonarqube`.
- Da PowerShell, `Invoke-RestMethod http://localhost:9000` può andare in timeout. `127.0.0.1:9000` o `curl.exe` funzionano.
- Progetto sul server: `dmvcframework`. Quality profile Delphi: `DMVC` (copia di "Sonar way comprehensive").

## Come lanciare lo scan

Il token va passato in `SONAR_TOKEN` e **non va salvato in un file**. Non è scritto qui.

```powershell
docker run --rm `
  -e SONAR_HOST_URL=http://host.docker.internal:9000 `
  -e SONAR_TOKEN=<token> `
  -v "C:\DEV\dmvcframework\sources:/usr/src" `
  -v "C:\Program Files (x86)\Embarcadero\Studio\37.0\source:/bds/source:ro" `
  sonarsource/sonar-scanner-cli `
  "-Dsonar.projectKey=dmvcframework" "-Dsonar.sources=." `
  "-Dsonar.sourceEncoding=UTF-8" `
  "-Dsonar.delphi.file.suffixes=.pas,.dpr,.dpk" `
  "-Dsonar.scm.disabled=true" `
  "-Dsonar.delphi.installationPath=/bds" `
  "-Dsonar.delphi.compilerVersion=VER370"
```

Perché queste opzioni:

- `sonar.delphi.installationPath` e `compilerVersion`: il plugin ha bisogno dei sorgenti RTL/VCL (qui Delphi 13 = Studio 37.0, `VER370`). I nomi `bds.path` e `compiler.version` **non funzionano**: il plugin li ignora e usa Studio 22.0 / VER350. Ho preso i nomi giusti dal jar.
- `.inc` è escluso dai suffissi: confligge con PHP (`sonar.lang.patterns.php`) e lo scanner si ferma.
- `sonar.scm.disabled=true`: nessun repository riconosciuto dal container.
- Lo scan dura circa 50 secondi.

## Trabocchetti incontrati

- **Non escludere `JsonDataObjects.pas` con `sonar.exclusions` né con `sonar.delphi.exclusions`.** L'unit serve a risolvere i simboli: senza, il parser del plugin va in `StackOverflowError` o `OutOfMemoryError`. Mi è successo quando l'esclusione era impostata dalla UI (*Source File Exclusions*) e ho dovuto cercarla nelle impostazioni del server.
- Per nascondere gli issue di quel file si usa invece la regola *Ignore Issues on Multiple Criteria* (Rule Key `*`, File Path `**/JsonDataObjects.pas`), impostata dalla UI. Funziona: gli issue del file passano a `FIXED` e non vengono ricreati.
- Il token di analisi (`sqp_...`) non può leggere l'attività delle analisi né modificare i profili (`Insufficient privileges`).
- Una ventina di file danno warning di encoding con UTF-8, probabilmente sono in Windows-1252. Non ho provato `sonar.sourceEncoding=Cp1252` né `sonar.delphi.codePage`.

## Profilo `DMVC`

Regole attive: 104 (una in meno per ogni regola disattivata rispetto a "comprehensive").

- `VariableName`: disattivata. Ha solo `globalPrefixes`, non può esprimere i prefissi `l`/`a`: segnalava circa 5.000 parametri e locali corretti.
- `FieldName`: disattivata. Accetta un solo prefisso con distinzione maiuscole/minuscole. Con `F` segnalava circa 620 campi `f` corretti, con `f` segnala i circa 650 che usano `F`.
- `UnusedRoutine`: attiva con `excludeApi=false`, quindi segnala metodi astratti e API pubbliche. Non l'ho ancora cambiata; con `excludeApi=true` dovrebbe scendere molto, ma non so di quanto.

Convenzione e regola di non rinominare in massa sono in `CLAUDE.md`, sezione "Naming convention".

## Risultati dell'ultimo scan

| | Valore |
|---|---|
| Righe di codice | 94.401 |
| Issue aperti | 7.249 (erano 12.905 con "comprehensive" non modificato) |
| Code smell | 7.175 |
| Bug | 74 |
| Vulnerabilità / hotspot | 0 / 0 |
| Gravità alta / media / bassa | 443 / 4.423 / 2.429 |

Regole più frequenti: `UnusedRoutine` 1.011, `MixedNames` 962, `LowercaseKeyword` 868, `TooLongLine` 563, `TrailingWhitespace` 446, `NilComparison` 386.

## Issue ad alta gravità (443): cosa sono

Valutazione su 3 esempi per regola (per `RoutineResultAssigned` ho letto anche i corpi), non su tutti i casi.

- **Complessità, 324** (`CognitiveComplexityRoutine` 215, `CyclomaticComplexityRoutine` 87, `TooLargeRoutine` 21, `TooManyNestedRoutines` 1): attendibili, ma sono misure e non bug. I picchi: `TemplatePro.Compile` (complessità cognitiva 1.528, 854 istruzioni), `RenderRange` (446), `GetVarAsTValue` (320), `MVCFramework.InternalExecuteAction` (216), `JSONObjectPropertyToTValueForRecord` (180). Anche `ExprEvaluator.Create` e `RQL.Parser.GetToken`.
- **Affidabilità, 119:**
  - `PlatformDependentTruncation` (42), `PlatformDependentCast` (6, aritmetica di puntatori in `Server.HttpSys` con `NativeUInt`) e `FormatArgumentType` (10): falsi allarmi probabili nei campioni.
  - `RoutineResultAssigned` (33): falsi allarmi. Sono metodi che chiamano solo `RaiseNotImplemented` nei serializzatori CSV, HTML, Text, URLEncoded.
  - `CompilerWarnings` (14): vero ma voluto (`{$WARN SYMBOL_DEPRECATED OFF}`, `{$WARNINGS OFF}`).
  - **`UnicodeToAnsiCast` (14): da guardare.** Per esempio `AnsiString(aValue.AsString)` su un memo FireDAC in `MVCFramework.ActiveRecord.pas` (righe 4013, 4039): perde i caratteri non ANSI.
- **Sicurezza, 14:** tutti `SwallowedException` (blocco `except` che non registra né rilancia), in `MVCFramework.pas` (2286-2302), `WebSocket.*`, `Server.HttpSys`, `Middleware.RateLimit.Redis`, `SSE.Writer`. Da valutare caso per caso.

## Da fare

- Aprire i 14 `UnicodeToAnsiCast` e i 14 `SwallowedException` e classificarli.
- Decidere se unificare i campi `F` → `f` (prima controllare RTTI e serializzazione dei campi); poi riattivare `FieldName` con `prefixes=f`.
- Valutare `UnusedRoutine` con `excludeApi=true`.
- Provare l'encoding Windows-1252 per i file con warning.
