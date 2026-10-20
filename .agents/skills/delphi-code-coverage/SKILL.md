---
name: delphi-code-coverage
description: Use when measuring Delphi line coverage with DelphiCodeCoverage (DCC), setting up coverage in a new Delphi repo, adding a coverage gate to CI, updating DelphiCodeCoverage to the latest release, or reading/interpreting coverage HTML/XML reports. Covers detailed map files (DCC_MapFile=3), the .dproj parsing mode, the known unit-naming bugs, threshold-based anti-regression, and offline operation in CI agents. Triggers on "code coverage", "DelphiCodeCoverage", "DCC", "line coverage Delphi", "misurare copertura", "aggiungere coverage a CI", "alzare la soglia di coverage", "aggiornare DelphiCodeCoverage", "0% coperto", "CodeCoverage_summary.html", "EMMA XML Delphi".
---

# Delphi code coverage con DelphiCodeCoverage

DelphiCodeCoverage (DCC) è il modo pratico per misurare la copertura a livello di riga di un progetto
Delphi che abbia già una suite di test (DUnit, DUnitX, o un eseguibile di verifica). È un debugger Win32 a
riga di comando: lancia l'eseguibile sotto osservazione, mette breakpoint a ogni indirizzo del `.map`, e
conta quante volte ognuno viene colpito. La skill copre setup, uso, integrazione CI, manutenzione e
troubleshooting reale.

---

# Quando usare questa skill

Caricala in questi scenari concreti:

- Setup iniziale: aggiungere misurazione di coverage a un progetto Delphi che non ce l'ha.
- Cambio di toolchain: aggiornamento Delphi (es. 12 → 13) che cambia il layout dei segmenti e rompe i vecchi map.
- Anti-regressione: pinnare una soglia di coverage minima come check di rilascio in CI.
- Indagine mirata: "perché il modulo X ha solo il 40% di coverage" → leggere l'HTML per riga, scrivere test, ri-misurare.
- Refresh snapshot: rigenerare `tests/coverage/CodeCoverage_summary.html` committato dopo un cambio di versione DCC.
- Operazioni di upgrade: passare da una release di DCC a una più nuova (procedura e rollback).
- Debug di falsi positivi/negativi: 0% coperto nonostante i test passino, 100% coperto nonostante i test falliscano, unit mancanti dal report.

---

# Cosa fa DelphiCodeCoverage

DelphiCodeCoverage è un singolo eseguibile (`CodeCoverage.exe`) disponibile in build `Win32` e `Win64`.
Non richiede installazione: è un file `.exe` con qualche `.dll` di dipendenza, distribuito come ZIP
sulle release GitHub del progetto. Il funzionamento è volutamente semplice: legge il **map file
"detailed"** prodotto dal compilatore Delphi (`DCC_MapFile=3`), ricostruisce per ogni riga di codice
Pascal l'intervallo di indirizzi macchina corrispondente, lancia il programma sotto test come fosse in
debug, e mette un breakpoint su ognuno di quegli indirizzi. Una riga è "coperta" se il breakpoint
corrispondente viene colpito almeno una volta.

Limiti importanti da dichiarare prima di iniziare:

- **Solo Delphi**: il formato `.map` di Embarcadero è closed e cambia tra versioni. Delphi XE2 non è
  supportato; XE3 e successive sì.
- **Line coverage, non branch**: una riga coperta non implica che tutti i suoi rami siano stati eseguiti.
  Un `if X then A else B` è "coperto" se la riga del test viene eseguita in qualche percorso.
- **Map file "detailed" obbligatorio**: `DCC_MapFile=3`. Map "segment-only" (valori 0/1/2) non contengono
  gli indirizzi per riga e producono report vuoti.
- **Offuscato/inlining**: codice morto eliminato dall'optimizer appare come "non copribile" (riga gialla).
  Va accettato, non combattuto.
- **Dipendenza dalla versione Delphi**: DCC ha numeri di release Delphi hardcoded nel parser. Quando Embarcadero
  cambia layout (es. Delphi 13 ha introdotto `.itext` separato da `.text`), serve un aggiornamento di DCC.

Formati di output supportati: **HTML** navigabile (uno per unit + sommario aggregato), **XML** Cobertura-like,
e **EMMA** XML per integrazione con sonar/JaCoCo/Coverage.py.

---

# Setup in un nuovo progetto Delphi

## a. Scaricare l'ultima release

```powershell
$latest = (Invoke-WebRequest -Uri 'https://api.github.com/repos/DelphiCodeCoverage/DelphiCodeCoverage/releases/latest' -UseBasicParsing | ConvertFrom-Json)
$zipUrl = $latest.assets | Where-Object { $_.name -like '*.zip' } | Select-Object -First 1 -ExpandProperty browser_download_url
Invoke-WebRequest -Uri $zipUrl -OutFile "$env:TEMP\DCC.zip" -UseBasicParsing
Expand-Archive -LiteralPath "$env:TEMP\DCC.zip" -DestinationPath "$env:TEMP\DCC" -Force
```

Risultato: `$env:TEMP\DCC\Win32\CodeCoverage.exe` e `$env:TEMP\DCC\Win64\CodeCoverage.exe`.

Conviene spostare il tool in una posizione stabile (es. `$env:USERPROFILE\tools\DCC`) e aggiungere il path
al profilo, così resta raggiungibile da qualunque script di build e da CI. La skill usa sempre il pattern
`$env:DCC` come variabile convenzionale.

## b. Abilitare map file dettagliati nei progetti di test

Il `.map` deve essere generato con `DCC_MapFile=3` (detailed, con righe). Per DUnit console:

```bat
@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild /t:Rebuild /p:Config=Debug /p:Platform=Win32 ^
  /p:DCC_Define=CONSOLE_TESTRUNNER /p:DCC_MapFile=3 RedisClientTests.dproj
exit /b %errorlevel%
```

Per il progetto di verification console: stesso pattern. Importante: `DCC_MapFile=3`, **non** 0/1/2.
Da Delphi 13 in avanti verifica anche che il map file contenga il segmento `.itext` (introdotto di recente):
se DCC non lo riconosce serve un upgrade del tool.

## c. Lanciare CodeCoverage

Il comando base, modo consigliato (`.dproj`):

```bat
CodeCoverage.exe ^
  -m bin\RedisClientTests.map ^
  -e bin\RedisClientTests.exe ^
  -sp ..\sources ^
  -dproj RedisClientTests.dproj ^
  -od coverage ^
  -html -xml -tec
```

Flag utili, in ordine di frequenza d'uso:

- `-dproj <file>`: **raccomandato**. Il tool parsa `.dproj` per estrarre source dirs, eseguibili, code page.
  Risolve il bug noto dei nomi unit con spazio prima del punto (es. `Redis .Values`).
- `-u unit1 unit2`: filtra le unit misurate. Usare solo se `-dproj` non basta; nomi come nel `.map` (esatti).
- `-uf units.lst`: lista di unit in file, una per riga.
- `-sd <dir>`: aggiunge directory ai search path.
- `-sp dir1 dir2`: search path per il sorgente (HTML li usa per il rendering colorato).
- `-od <dir>`: directory di output (deve esistere).
- `-html` / `-xml` / `-emma`: formati output, combinabili.
- `-tec`: passthrough dell'exit code del test exe. **Fondamentale in CI**: senza, DCC ritorna sempre 0 e
  un fallimento dei test passa inosservato.
- `-a "arg1 arg2"`: argomenti passati al test exe (es. `-a "-b"` per il run batch DUnit).
- `-lcl N`: limita conteggio esecuzioni per riga (utile per test con loop infiniti ragionevoli).
- `-lt <file>`: log debug su file.

## d. Esempio di ciclo completo in PowerShell

```powershell
$env:DCC = "$env:USERPROFILE\tools\DCC"
if (-not (Test-Path "$env:DCC\Win32\CodeCoverage.exe")) {
  # branch di fallback: scarica di nuovo (vedi sezione a)
}

cd tests
.\build_dunit_console.bat

& "$env:DCC\Win32\CodeCoverage.exe" `
  -dproj RedisClientTests.dproj `
  -sp ..\sources `
  -od coverage `
  -html -tec

$html = Get-Content coverage\CodeCoverage_summary.html -Raw
$m = [regex]::Match($html, 'Aggregated.+?(\d+(?:\.\d+)?)<small.+?%')
Write-Host "Aggregated coverage: $($m.Groups[1].Value)%"
```

Il pattern "build → run coverage → parse summary" è il ciclo fondamentale. Va incapsulato in uno script
e tenuto committato, così tutti (umano, CI, agente) usano lo stesso comando.

---

# Leggere il report

**`CodeCoverage_summary.html`** è l'entry point. La tabella ha una riga per unit misurata con:

- righe coperte / righe con codice generato / totale
- percentuale di copertura
- link al file `Unit(Unit.pas).html` con il sorgente colorato riga per riga

Codice colore dell'HTML per unit:

- **Verde**: riga coperta (breakpoint colpito almeno una volta).
- **Blu**: riga non coperta.
- **Giallo**: riga senza codice generato (commenti, dichiarazioni, righe vuote). **Non** è una mancanza.

Il numero aggregato in fondo è quello da comunicare in release note, da pinnare come soglia, e da
usare nei check di anti-regressione. Non guardare solo l'aggregato: la tabella per unit dice dove
concentrare gli sforzi.

---

# Integrazione in flusso CI/CD

## a. GitHub Actions (esempio completo)

```yaml
name: Tests with Coverage
on: [push, pull_request]

jobs:
  tests:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v4

      - name: Start Redis
        run: |
          docker run -d --rm -p 6379:6379 redis:8
          docker run -d --rm -p 6390:6390 redis:8 --tls-port 6390 ^
            --tls-cert-file /tls/redis.crt ^
            --tls-key-file /tls/redis.key ^
            --tls-ca-cert-file /tls/ca.crt ^
            --tls-auth-clients no

      - name: Install Delphi
        uses: charon-platform/emu-dxhub-actions/setup-licensed-delphi@v1
        with:
          delphi-version: '12'

      - name: Build & Run
        run: |
          cd tests
          build_dunit_console.bat
          DCC\Win32\CodeCoverage.exe ^
            -dproj RedisClientTests.dproj ^
            -sp ..\sources ^
            -od coverage ^
            -html -xml -tec

      - name: Upload coverage HTML
        uses: actions/upload-artifact@v4
        with:
          name: coverage-report
          path: tests/coverage

      - name: Check coverage threshold
        shell: pwsh
        run: |
          $html = Get-Content tests/coverage/CodeCoverage_summary.html -Raw
          $m = [regex]::Match($html, 'Aggregated.+?(\d+(?:\.\d+)?)<small.+?%')
          $pct = [double]$m.Groups[1].Value
          Write-Host "Aggregated coverage: $pct%"
          if ($pct -lt 66.0) {
            Write-Error "Coverage $pct% is below the 66.0% release threshold"
            exit 1
          }
```

Note operative:

- `-tec` è già nel comando DCC: il check "Tests with Coverage" fallisce se i test rossi, **prima** di arrivare
  al check di soglia. Questo è il comportamento desiderato.
- L'artifact `coverage-report` permette di scaricare l'HTML e navigarlo da PR.
- Il threshold va in un passo separato, leggibile da qualunque reviewer senza far girare Delphi.

## b. Azure DevOps / Jenkins

Stesso pattern, runner Windows obbligatorio (DCC è Win32/Win64 nativo). Su Jenkins salvare l'agent come
`windows` e mettere il path di DCC nel `PATH` globale. Su Azure DevOps usare `windows-2022` come VM image.

## c. Anti-regressione (soglia)

Regola ferrea, da scrivere in `AGENTS.md` e in `tests/coverage/README.md`:

> La soglia di coverage va alzata solo aggiungendo test. Mai abbassata per "far passare la pipeline".

Numeri operativi:

- Soglia iniziale: rilevata dal primo snapshot reale (es. `66.0%`), non inventata.
- Aggiornare sia il valore nello script CI sia la cifra dichiarata in `AGENTS.md`.
- Un rilascio che abbassa la soglia è un rilascio che rinuncia a uno standard: rifiutare in review.

---

# Aggiornare DelphiCodeCoverage all'ultima versione

DCC non rilascia spesso (l'ultima release risale al 2022). Un upgrade ogni 6-12 mesi è ragionevole, e
solo se si è cambiato toolchain Delphi o se serve un fix di parser. Procedura:

```powershell
# 1. Controlla cosa c'è di nuovo
$latest = (Invoke-WebRequest -Uri 'https://api.github.com/repos/DelphiCodeCoverage/DelphiCodeCoverage/releases/latest' -UseBasicParsing).Content | ConvertFrom-Json
"$($latest.tag_name)  $($latest.published_at)"

# 2. Upgrade con backup
$repo = "$env:USERPROFILE\tools\DCC"
$latest = (Invoke-WebRequest -Uri 'https://api.github.com/repos/DelphiCodeCoverage/DelphiCodeCoverage/releases/latest' -UseBasicParsing).Content | ConvertFrom-Json
$zipUrl = $latest.assets | Where-Object { $_.name -like '*.zip' } | Select-Object -First 1 -ExpandProperty browser_download_url
$tmp = "$env:TEMP\dcc_upgrade_$(Get-Random)"
New-Item -ItemType Directory -Path $tmp | Out-Null
Invoke-WebRequest -Uri $zipUrl -OutFile "$tmp\dcc.zip" -UseBasicParsing
Expand-Archive -LiteralPath "$tmp\dcc.zip" -DestinationPath "$tmp\expanded" -Force
if (Test-Path $repo) { Move-Item $repo "$repo.bak.$(Get-Date -Format yyyyMMddHHmmss)" -Force }
Move-Item "$tmp\expanded" $repo
& "$repo\Win32\CodeCoverage.exe" 2>&1 | Select-Object -First 3
```

Punti operativi dopo l'upgrade:

- Leggere il **CHANGELOG** del tool: DCC ha dipendenze hardcoded sui numeri di release Delphi, un upgrade
  Delphi spesso richiede un upgrade DCC.
- **Ri-generare il baseline** di coverage: la stessa sorgente può dare numeri leggermente diversi se il
  parser del map cambia.
- Se la coverage crolla senza che il codice sia cambiato, è un bug dello strumento. Aprire issue su
  `https://github.com/DelphiCodeCoverage/DelphiCodeCoverage/issues` allegando: tag DCC, versione Delphi, map
  file anonimizzato, prima/dopo.

---

# Aggancio agentico

Come un modello (Codex, opencode, Cursor, ecc.) deve usare questa skill in pratica:

- **"Aggiungi code coverage al progetto X"** → eseguire il setup da zero (sezione a/b/c/d), produrre gli
  script `.bat`/`.ps1`, committare il primo snapshot in `tests/coverage/`, dichiarare la soglia iniziale in
  `AGENTS.md`. Non misurare contro un numero inventato: il primo run è il baseline.
- **"Alza la coverage del modulo Y"** → aprire il file `Y.html` corrispondente, identificare le righe blu,
  capire il pattern (rami non esercitati, eccezioni non simulate, helper non chiamati), scrivere test
  mirati, ri-misurare. Non accontentarsi di "eseguire il metodo" come test: serve branching reale.
- **"Perché il check di coverage fallisce in CI"** → checklist in ordine:
  1. Il `.map` esiste ed è stato generato con `DCC_MapFile=3`? (`grep -i 'Line numbers' bin/*.map` deve
     trovare la sezione; i map "summary-only" non ce l'hanno).
  2. Redis (o il backend richiesto dai test) è raggiungibile dall'agent?
  3. Il test exe termina con exit code 0 in assenza di DCC?
  4. Il parsing del summary HTML è andato a buon fine? (stampare il match, non solo la percentuale.)
- **"Aggiorna DelphiCodeCoverage"** → seguire la procedura della sezione 7 paragrafo per paragrafo, con
  backup. Non saltare il ri-generare il baseline dopo.
- **"Abbassa la soglia di coverage"** → **rifiutare**. È una regola anti-regressione, esplicita nel
  progetto. Proporre invece: aggiungere test per il codice non coperto. Eccezione ammissibile: rimuovere
  codice morto (riduzione di righe = soglia naturale più alta, non più bassa).
- La skill produce artefatti concreti e committabili: script `.bat`/`.ps1`, snapshot HTML in
  `tests/coverage/`, soglia documentata in `AGENTS.md`. Mai produrre solo "spiegazioni" senza aver
  effettivamente lanciato DCC almeno una volta.

---

# Troubleshooting

Problemi reali, con diagnosi e fix. Leggere in ordine: i primi capitoli coprono il 90% dei fallimenti.

**0% coperto anche se i test passano**

Quasi sempre map file "summary-only". Il `.map` deve avere la sezione "Line numbers" per unit:
```
grep -i "Line numbers" bin\MyTests.map
```
Se non c'è, rigenerare con `DCC_MapFile=3`. MAI `0`, `1` o `2` (segment-only / public-only).

**"Could not open: MyTests.map"**

Il map deve stare accanto all'EXE, oppure `-m` deve puntare al path assoluto. Verificare che il
build abbia effettivamente prodotto il `.map` (alcune configurazioni Debug lo omettino se `DCC_MapFile`
non è settato esplicitamente).

**Unit mancanti dal report**

Di solito il nome unit nel `.map` non combacia con quello del file. Con `-dproj` il tool parsa i source
dir e li risolve. Con `-u unit1 unit2` si è fragili: il nome deve essere **esatto** come nel `.map`,
incluso lo spazio prima del punto in `Redis .Values`. Sintomo tipico: "track coverage for: Redis "
(namespace intero, troncato). Fix: rimuovere `-u` e usare `-dproj`.

**"track coverage for: Redis " (intero namespace, troncato)**

Conferma del bug noto: DCC tronca il nome al primo `.` se si passa `-u Redis.Values`. Workaround:
passare `-dproj` e non passare `-u`. Se serve per forza, copiare il `.dproj` in una posizione
raggiungibile o usare un file `-spf` con i path delle unit una per una.

**DELPHI_MapFile nuovo formato non riconosciuto (es. `.itext` separato da `.text` in D13)**

È un bug di DCC che richiede upgrade del tool. Aprire issue upstream allegando il `.map` di esempio
anonimizzato e la versione Delphi.

**Test exe solleva eccezione non gestita ma coverage mostra 100%**

Quasi sempre è la cache: DCC riusa i breakpoint del run precedente se non si cambia `-od`. Cancellare
la directory di output e rieseguire. Verificare anche `-tec` sia attivo: senza, l'exit code non viene
propagato e DCC ritorna 0 anche se il test è esploso.

**PowerShell quoting per `-a`**

`CodeCoverage.exe -a "arg with spaces"` funziona. Ma se gli argomenti contengono caratteri speciali
PowerShell (`$`, `"`), passare attraverso `cmd.exe`:
```powershell
cmd.exe /c "CodeCoverage.exe -a `"-foo=`$bar`" ..."
```
In alternativa, scrivere il comando in un `.bat` ed eseguirlo. Più robusto in CI.

**Test exe non compilato con `DCC_MapFile=3`**

La coverage usa sempre l'ultimo map. Se si è cambiato il build script, rigenerare. Non modificare
a mano il `.map`.

**Coverage > 100% (impossibile) o numeri strani**

Map file corrotto o rigenerato in parte. Cancellare `bin/` e rebuildare da zero.

**CI termina con success ma il file `CodeCoverage_summary.html` non esiste**

`-od` deve esistere prima di lanciare DCC. Aggiungere `New-Item -ItemType Directory -Force coverage`
(o `mkdir` in `.bat`) prima dell'invocazione.

**L'agent Windows non ha i tool di build**

Verificare `rsvars.bat` sia stato chiamato (`where dcc32` deve trovarlo) e che `msbuild` sia nel PATH
dopo `rsvars.bat`. Ricordare: Delphi 13 installa `rsvars.bat` in un path con spazi; quotare.

**Soglia di coverage che non si ricalibra dopo un cambio Delphi**

Il baseline va rigenerato quando si cambia versione DCC o Delphi. Non è un'operazione automatica:
richiede un commit esplicito che aggiorna `AGENTS.md` e lo script CI. Una soglia "vecchia" in CI è un
falso senso di sicurezza.

---

# Riferimenti

- Repo upstream: https://github.com/DelphiCodeCoverage/DelphiCodeCoverage
- Releases API: https://api.github.com/repos/DelphiCodeCoverage/DelphiCodeCoverage/releases/latest
- CHANGELOG (storico release): incluso nello ZIP, file `CHANGELOG.txt` o equivalente.
- Issue tracker: https://github.com/DelphiCodeCoverage/DelphiCodeCoverage/issues
- EMMA XML schema: standard della ROSE framework Java, parsato da sonar/Coverage.py/codecov.
- Cobertura XML: schema alternativo, supportato da Jenkins e Azure DevOps.
