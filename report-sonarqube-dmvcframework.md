# Report analisi SonarQube — DelphiMVCFramework

Data analisi: 16/10/2026
Server: SonarQube Community Build 26.10.0 con plugin Delphi (sonar-delphi 1.21.0)
Progetto: `dmvcframework` — sorgenti in `C:\DEV\dmvcframework\sources\`

## Ambito e configurazione

- Compilatore: Delphi 13 (`VER370`), libreria standard da `Studio\37.0\source`.
- File analizzati: `*.pas`, `*.dpr`, `*.dpk` (144 unit `.pas`). I file `.inc` non sono tra i file analizzati: la loro estensione confliggeva con PHP.
- `JsonDataObjects.pas` resta nell'analisi (serve a risolvere i simboli) ma i suoi issue sono ignorati con una regola `Ignore Issues on Multiple Criteria` (`*` su `**/JsonDataObjects.pas`).
- Una ventina di file contengono caratteri non UTF-8 (probabile Windows-1252) e generano warning di encoding. Il contenuto di quei file potrebbe essere letto in modo impreciso.

## Sintesi

| Metrica | Valore |
|---|---|
| Righe di codice | 94.401 |
| Issue aperti | 12.905 |
| Bug (misura Sonar) | 74 |
| Vulnerabilità | 0 |
| Security hotspot | 0 |
| Duplicazione | 4,6% |

Gli issue aperti per qualità del software:

| Qualità | Issue |
|---|---|
| Manutenibilità | 12.781 |
| Affidabilità | 156 |
| Sicurezza | 14 |

Per gravità:

| Gravità | Issue |
|---|---|
| Alta | 443 |
| Media | 4.425 |
| Bassa | 8.083 |

Per categoria Clean Code: Consistent 9.448, Intentional 2.460, Adaptable 997.

La grande maggioranza degli issue (circa il 99%) riguarda la manutenibilità e lo stile. Il codice non mostra vulnerabilità né hotspot. I punti da guardare per primi sono i 443 issue ad alta gravità e i 14 issue di sicurezza.

## Regole più frequenti

| Regola | Issue | Tipo di problema |
|---|---|---|
| `VariableName` | 5.036 | Convenzione di nomi delle variabili |
| `UnusedRoutine` | 1.011 | Routine mai usate |
| `MixedNames` | 962 | Stesso identificatore scritto con maiuscole/minuscole diverse |
| `LowercaseKeyword` | 868 | Parole chiave non in minuscolo coerente |
| `FieldName` | 618 | Convenzione di nomi dei campi |
| `TooLongLine` | 563 | Righe troppo lunghe |
| `TrailingWhitespace` | 446 | Spazi a fine riga |
| `NilComparison` | 386 | Confronti con `nil` |
| `EmptyArgumentList` | 245 | Parentesi vuote nelle chiamate |
| `UnusedImport` | 237 | Unit importate e non usate |
| `MissingSemicolon` | 222 | Punto e virgola mancante |
| `CognitiveComplexityRoutine` | 215 | Complessità cognitiva elevata |
| `ImportSpecificity` | 211 | Import nel posto sbagliato (interface/implementation) |
| `UnusedProperty` | 198 | Proprietà mai usate |
| `ShortIdentifier` | 196 | Identificatori troppo corti |

Le prime regole sono di stile e dipendono dal profilo qualità scelto. `VariableName` da sola pesa il 39% del totale: verificare se la convenzione del progetto coincide con quella del profilo, prima di intervenire sul codice.

## Issue ad alta gravità (443)

| Regola | Issue |
|---|---|
| `CognitiveComplexityRoutine` | 215 |
| `CyclomaticComplexityRoutine` | 87 |
| `PlatformDependentTruncation` | 42 |
| `RoutineResultAssigned` | 33 |
| `TooLargeRoutine` | 21 |
| `CompilerWarnings` | 14 |
| `UnicodeToAnsiCast` | 14 |
| `FormatArgumentType` | 10 |
| `PlatformDependentCast` | 6 |
| `TooManyNestedRoutines` | 1 |

Due gruppi:

- **Complessità** (`CognitiveComplexityRoutine`, `CyclomaticComplexityRoutine`, `TooLargeRoutine`, `TooManyNestedRoutines`): 324 issue. Sono routine da spezzare, un intervento di refactoring.
- **Affidabilità** (`PlatformDependentTruncation`, `RoutineResultAssigned`, `UnicodeToAnsiCast`, `FormatArgumentType`, `PlatformDependentCast`, `CompilerWarnings`): 119 issue. Possono produrre comportamenti scorretti su piattaforme a 64 bit o con stringhe Unicode, e vanno controllati uno per uno.

## Issue di sicurezza (14)

Sono tutti `SwallowedException`: un blocco `except` che non registra né rilancia l'eccezione.

| File | Righe |
|---|---|
| `MVCFramework.pas` | 2286, 2290, 2295, 2302 |
| `MVCFramework.WebSocket.Client.pas` | 443, 449, 672 |
| `MVCFramework.Server.HttpSys.pas` | 422, 493 |
| `MVCFramework.WebSocket.ConnectionManager.pas` | 601, 623 |
| `MVCFramework.Middleware.RateLimit.Redis.pas` | 191 |
| `MVCFramework.SSE.Writer.pas` | 402 |
| `MVCFramework.WebSocket.Server.pas` | 875 |

Alcuni di questi potrebbero essere voluti (per esempio la chiusura di una connessione già caduta). Vanno valutati caso per caso.

## Issue di affidabilità (156)

| Regola | Issue |
|---|---|
| `PlatformDependentTruncation` | 42 |
| `RoutineResultAssigned` | 33 |
| `CompilerWarnings` | 14 |
| `UnicodeToAnsiCast` | 14 |
| `EmptyRoutine` | 14 |
| `FormatArgumentType` | 10 |
| `VariableInitialization` | 9 |
| `NonLinearCast` | 7 |
| `PlatformDependentCast` | 6 |
| `CompilerHints` | 4 |
| `ObjectPassedAsInterface` | 3 |

## File con più issue

| File | Issue |
|---|---|
| `TemplatePro.pas` | 1.156 |
| `MVCFramework.ActiveRecord.pas` | 1.090 |
| `MVCFramework.pas` | 958 |
| `MVCFramework.Nullables.pas` | 826 |
| `MVCFramework.Swagger.Commons.pas` | 536 |
| `MVCFramework.Serializer.JsonDataObjects.pas` | 461 |
| `MVCFramework.JSONRPC.pas` | 371 |
| `MVCFramework.Commons.pas` | 364 |
| `MVCFramework.RESTClient.pas` | 328 |
| `MVCFramework.MinimalAPI.pas` | 301 |
| `MVCFramework.Validators.pas` | 269 |
| `MVCFramework.Filters.pas` | 238 |
| `MVCFramework.RESTClient.Indy.pas` | 212 |
| `MVCFramework.Serializer.Commons.pas` | 178 |
| `MVCFramework.RQL.Parser.pas` | 177 |

## Limiti di questo report

- I numeri vengono dalle API del server alla data dell'analisi. Non ho esaminato il codice dei singoli issue, tranne l'elenco dei 14 di sicurezza.
- Il conteggio degli issue (12.905) e la misura "code smell" del server (12.831) non coincidono per classificazione: gli issue sono contati per qualità del software, le misure per tipo.
- Gli issue del file `JsonDataObjects.pas` (727) sono esclusi dal totale.
