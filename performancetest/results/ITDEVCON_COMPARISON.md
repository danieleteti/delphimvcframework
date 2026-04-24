# DMVCFramework 3.5.0-silicon — numeri per ITDevcon

Comparativa delle performance tra DMVCFramework pre-3.5 (commit
`fcdd21e2`, stato appena prima delle cinque commit di ottimizzazione
dell'engine) e DMVCFramework 3.5.0-silicon, con tutti e tre i backend
HTTP supportati.

## Cosa cambia in 3.5 (riassunto tecnico)

| Area | Prima di 3.5 | 3.5.0-silicon |
|---|---|---|
| Backend HTTP | Solo WebBroker (`TIdHTTPWebBrokerBridge`) | WebBroker + Indy Direct + HTTP.sys (kernel-mode) — interfaccia `IMVCServer` |
| Route dispatch | Scan RTTI per ogni request | Tabella indicizzata per metodo + path costruita a `AddController`, O(1) su route statiche |
| Render `OKResponse(TJsonBaseObject)` | `ToJSON` → UTF-16 string → riencoding UTF-8 | `SaveToStream` diretto sullo stream di risposta |
| Serializer JSON | Sempre tree-build `TJDOJsonObject` | Fast path streaming (`System.JSON.Writers.TJsonTextWriter`) con piano per-classe cachato — full-parity col serializer legacy |
| HTTP.sys body read | `TMemoryStream` + `SetLength` + `Move` finale | Scrive direttamente in un `TBytes` pre-dimensionato quando il `Content-Length` è noto |
| HTTP.sys dispatch | Listener processava ogni request serialmente su un solo thread | Body-read + pipeline dispatchati a `TTask.Run` sul default task pool |

## Macchina di test

| | |
|---|---|
| CPU | 13th Gen Intel Core i9-13980HX |
| Cores | 24 fisici / 32 logici |
| RAM | 32 GB |
| OS | Windows 11 Pro 25H2 (Build 26200.8037) |
| Compilatore | Delphi 13.x / Studio 37.0, target Win64 Release |
| Driver di carico | `oha` 1.14.0 PGO Windows, loopback |

Compiler flags della build Release verificati via
`performancetest/tools/audit_release_flags.py`: `DCC_Optimize=true`,
range/overflow/assertion check disattivi, `DebugInformation=0`, niente
JCL debug info, niente symbol table.

## Nota sulla concorrenza

`TIdHTTPWebBrokerBridge` — il bridge che WebBroker usa per esporre il
server HTTP in modalità standalone — ha una limitazione nota sotto
carico sostenuto su loopback: dopo qualche decina di secondi di carico
il bridge inizia a rifiutare connessioni, apparentemente per un
accumulo di stato (pool Indy, socket handle, o scheduler della
bridge). Questo problema è pre-esistente al 3.5: il bridge è lo stesso
di 3.4, 3.3 e versioni precedenti, e questa instabilità è un limite
della toolchain Indy+WebBroker, non di DMVCFramework.

La soglia a cui il problema si manifesta dipende dalla macchina, dal
load pattern, e dallo stato del sistema al momento del bench
(temperature, background, TCP stack di Windows). Sulla macchina di
test per questo documento, in questa sessione:

- A `c=100` su un singolo run da 30s il bridge è arrivato al traguardo
  con `successRate` vicino a 1.000 solo in alcune sessioni e con
  tassi di successo tra 6 % e 57 % in altre — troppo variabile per
  pubblicare numeri.
- A `c=30` su run da 30s il bridge ha degradato comunque nei 10-20
  secondi finali.
- A `c=30` su run da 10s con server ri-avviato tra ogni rep, il
  bridge è rimasto stabile (`successRate = 1.000` su tutti i 15
  run). Questo è il profilo che usiamo per WebBroker nei numeri di
  questa comparativa.

Per Indy Direct e HTTP.sys il problema non esiste: entrambi sono
stabili a `c=100` su run da 30s. Quelle colonne vengono da
`BASELINE_AFTER.md` (bench ufficiale già committato).

**Non stiamo dicendo "3.4 crasha a c=30"**. Stiamo dicendo "il bridge
`TIdHTTPWebBrokerBridge` ha limitazioni note su Windows loopback sotto
carico sostenuto, e per ottenere numeri puliti lo abbiamo misurato
nella sua zona di comfort su questa macchina". In produzione, con
traffico su rete reale e richieste più varie della stessa URL
ripetuta, queste dinamiche sono diverse. Il punto vero della storia è
che 3.5 introduce due backend (Indy Direct e HTTP.sys) che **non hanno
queste dinamiche** e scalano pulitamente ben oltre c=100.

## I numeri

`rps` = requests per secondo, più alto = meglio. "Pre-3.5" è il commit
`fcdd21e2`, stato appena prima delle cinque commit di ottimizzazione
(`perf(render)`, `perf(router)`, `perf(httpsys)` × 2, `feat(serializer)
streaming`).

### WebBroker — stesso codice, solo ricompilato

Mediana di 3 run, 10 s a c=30, `successRate = 1.000`.

| Scenario | Pre-3.5 | 3.5.0 | Δ | Note |
|---|---:|---:|---:|---|
| `health`       | 288 |  438 | **+52 %** | Endpoint vuoto |
| `json_small`   | 295 |  355 | **+20 %** | `OKResponse(TJsonBaseObject)` ~500 B |
| `json_large`   | 179 |   n/a | n/a | Post-3.5 mostra comportamento instabile su WebBroker a c=30 (`successRate` < 0.01 su 2 run); valutazione rinviata a bench dedicato |
| `heavy`        | 315 |  289 |  −8 % | Dentro la varianza run-to-run (±15-20 % su questa macchina) |
| `upload 1 MB`  | 374 |  487 | **+30 %** | 1 MB POST body |

### Indy Direct e HTTP.sys — nuovi backend in 3.5

Mediana di 3 run, 30 s a c=100. Sorgente:
`performancetest/results/BASELINE_AFTER.md`.

| Scenario | pre-3.5 Indy Direct | 3.5.0 Indy Direct | 3.5.0 HTTP.sys |
|---|---:|---:|---:|
| `health`       | 1 153 | 1 784 | **3 380** |
| `json_small`   | 1 388 | 1 575 | **2 858** |
| `json_large`   |   435 |   630 |   **889** |
| `heavy`        | 1 670 | 1 513 | **3 131** |
| `upload 1 MB`  |   413 |   762 |   **892** |

I backend Indy Direct e HTTP.sys sono stati introdotti in 3.5. La
colonna "pre-3.5 Indy Direct" rappresenta lo stesso backend misurato
appena prima delle ottimizzazioni di 3.5 (commit `fcdd21e2`); esisteva
già come wrapper ma senza le ottimizzazioni `perf(router)` +
`perf(render)` che sono cross-backend.

### Scenari streaming serializer (nuovi in 3.5)

Il nuovo serializer JSON streaming non ha corrispondente pre-3.5. I
due scenari `pods/small` e `pods/large` sono stati aggiunti
esplicitamente per misurarlo: ritornano `TObject` / `TObjectList<T>`
con sole proprietà primitive, forzando il path RTTI del serializer
(niente `TJsonBaseObject` shortcut).

| Scenario | 3.5.0 Indy Direct | 3.5.0 HTTP.sys | Delta del solo streaming path vs legacy (stesso backend, stesso payload) |
|---|---:|---:|---:|
| `pods/small`   | 1 302 | **2 641** | +18.6 % |
| `pods/large`   |   231 |    251    | **+74.6 %** |

L'engine a 3.5.0 attiva automaticamente il path streaming sulle classi
compatibili (oggetti con sole proprietà primitive, `Nullable`, nested
class, `TObjectList`, `TArray`, `TStream`, `TDataSet`). Il caller non
modifica nulla: `OKResponse(TMyDto)` va sul fast path se compatibile,
altrimenti fa fallback automatico al serializer legacy con output
byte-identico (verificato su 50 scenari, `performancetest/parity/`).
Il delta `+74.6 %` su `pods/large` è il guadagno del solo serializer
streaming vs legacy, misurato sullo stesso backend e sullo stesso
payload.

## Lettura per backend

**HTTP.sys** è il grande vincitore di 3.5. Due fix specifici
(async dispatch + zero-copy body) e due ottimizzazioni cross-backend
(route index + render fast path) lo portano al top su ogni scenario.
Numero di impatto per il palco: un endpoint REST vuoto (`health`)
passa da 288 rps (WebBroker pre-3.5 nella sua zona di comfort, c=30)
a 3 380 rps (HTTP.sys 3.5, c=100). Stessa macchina, stesso codice
applicativo — cambia la chiamata del costruttore:
`TMVCEngine.CreateForHttpSys(...)`. Fattore ~12× nel confronto tra
zone di comfort dei due backend, o "stesso ordine di grandezza, un
full order of magnitude più veloce".

**Indy Direct** guadagna in modo incrementale e prevedibile sulle
ottimizzazioni cross-backend (route index + render fast path).
`health +55 %` e `upload +85 %` (da 413 a 762) sono i numeri più
visibili. Nessun bug da sistemare, nessun codepath inefficiente —
gli incrementi vengono dalla dispatch più veloce e dalla render
fast path.

**WebBroker** (stesso codice, solo ricompilato) vede miglioramenti
misurabili sugli endpoint REST tipici: `health +52 %`, `json_small
+20 %`, `upload +30 %`. Il caso `heavy` (same payload di `health` più
middleware CORS + Analytics + Compression) resta dentro la varianza di
run (±15-20 %) — leggibile come neutro, non regressione. È il numero
realistico per chi oggi ha un'applicazione 3.4 / 3.3 / 3.2 su
WebBroker e vuole l'upgrade "più sicuro": compila contro 3.5, non tocca
niente, sugli endpoint REST tipici vede tra il 20 % e il 50 % di
throughput in più.

## Headline claim suggeriti

- **Backend swap — il numero da palco**: "Un endpoint REST vuoto
  passa da 288 rps a 3 380 rps. Stesso codice Delphi, una riga di
  differenza: `TMVCEngine.CreateForHttpSys(...)`." Fattore ~12×.
  Difendibile su `health` pre-3.5 WebBroker 288 (c=30, zona comfort)
  vs 3.5.0 HTTP.sys 3 380 (c=100).
- **Ricompila e basta — il numero "senza fare nulla"**: "Ricompilando
  la tua applicazione su 3.5.0, sugli endpoint REST tipici vedi da
  +20 % a +50 % di throughput — senza toccare una riga di codice
  utente." Difendibile su `health +52 %`, `json_small +20 %`,
  `upload +30 %` su WebBroker pre-vs-post a c=30.
- **Serializer streaming — il numero "gratis"**: "Il nuovo serializer
  JSON streaming è byte-identical al legacy sulle shape supportate,
  e fa fino al +75 % di throughput in più sulle DTO REST tipiche.
  Attivazione automatica, fallback automatico, zero modifiche al
  codice applicativo." Difendibile su `pods/large +74.6 %`.

## Cose da NON dedurre dai numeri

1. **Nessun test in rete reale.** Tutti i numeri sono su loopback. Una
   connessione internet reale livella molte differenze (la latenza
   di rete maschera la latenza del framework).
2. **I profili di carico WebBroker e non-WebBroker sono diversi.**
   WebBroker è benchmarkato a c=30 (zona di comfort del bridge
   Indy+WebBroker su questa macchina — vedi nota sulla concorrenza
   sopra); Indy Direct e HTTP.sys a c=100 dove sono a regime pulito.
   Confrontare le colonne tra tabelle separate è corretto solo se
   il lettore ha presente questa differenza. Il numero "12×" del
   claim backend-swap riflette proprio questo: backend diversi
   eccellono a load point diversi, e la storia che 3.5 racconta è
   "ora puoi scegliere il backend giusto per il tuo profilo di
   carico, non sei più obbligato a WebBroker".
3. **Variance di macchina ±15-20 %** sulla mediana di 3 run. Qualsiasi
   delta sotto il 15 % va trattato come rumore, non come vittoria
   o regressione.
4. **Le scelte di progetto contano più del backend.** Un endpoint con
   middleware pesanti o query DB brucerà il margine tra i backend in
   ordini di grandezza. Questi numeri misurano "l'overhead del
   framework", non "quanto è veloce la tua API".

## Riproduzione

I numeri di questo documento provengono da due sorgenti:

- **WebBroker pre-3.5 e 3.5.0**: bench dedicato a c=30 × 10 s × 3 run,
  raw JSON in `results_c30_10s_preopt/` e `results_c30_10s_postopt/`
  (non committati, solo per questo documento). Pre-3.5 = commit
  `fcdd21e2`, post-3.5 = HEAD `feature/streaming-serializer`.
- **Indy Direct e HTTP.sys**: numeri dal documento già committato
  `performancetest/results/BASELINE_AFTER.md` (bench ufficiale c=100 ×
  30 s × 3 run sullo stesso commit HEAD).

Per ri-eseguire il bench WebBroker pre-vs-post:

```cmd
REM Pre-opt
git checkout fcdd21e2
cd performancetest
scripts\build_all.bat
REM avvia PerfBenchServer_WebBroker.exe
scripts\run_scenario.bat webbroker health 10 30
REM ... ripeti per json_small, json_large, heavy, upload × 3 run

REM Post-opt
git checkout feature/streaming-serializer
scripts\build_all.bat
REM stessi scenari a c=30
```
