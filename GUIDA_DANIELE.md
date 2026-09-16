# Guida Test: JWT Asymmetric + OIDC + JWKS

## 1. JWT Asymmetric Auth (`samples/jwt_asymmetric_auth/`)

Il piu' semplice da testare - zero dipendenze esterne, chiavi PEM gia' incluse.

### Prerequisiti

- OpenSSL DLLs nella directory dell'eseguibile (`libcrypto-3.dll` + `libssl-3.dll`)
  oppure (`libcrypto-3-x64.dll` + `libssl-3-x64.dll` per Win64)
- Le chiavi sono gia' in `bin/keys/` (rsa, ec, ed25519)

### Avvio

Compila ed esegui `JWTAsymmetricAuthSample.dproj`. Il server parte su porta **8080**.

### Test con curl

```bash
# 1. Endpoint pubblico (nessun token richiesto)
curl http://localhost:8080/api/public/info

# 2. Login come admin -> ottieni JWT firmato RS256
curl -X POST http://localhost:8080/api/login \
  -H "Content-Type: application/json" \
  -d "{\"jwtusername\":\"admin\",\"jwtpassword\":\"admin\"}"

# Dalla risposta, copia il valore del campo "token"

# 3. Endpoint protetto con il token
curl http://localhost:8080/api/protected/profile \
  -H "Authorization: Bearer <TOKEN>"

curl http://localhost:8080/api/protected/secret \
  -H "Authorization: Bearer <TOKEN>"

# 4. Verifica che senza token ritorna 401
curl -v http://localhost:8080/api/protected/profile
```

### Credenziali demo

| Username | Password | Ruoli |
|----------|----------|-------|
| admin | admin | admin, user |
| user | user | user |

### Test automatico di tutti gli algoritmi

Compila ed esegui `TestJWTSigners.dpr` (stessa cartella). Testa in automatico:
HS256/384/512, RS256/384/512, PS256/384/512, ES256/384/512, EdDSA (Ed25519).

Le chiavi usate sono in `bin/keys/`:
```
keys/rsa/         -> private.pem, public.pem
keys/ec/          -> es256_*.pem, es384_*.pem, es512_*.pem
keys/ed25519/     -> private.pem, public.pem
```

---

## 2. OIDC senza verifica firma (`samples/middleware_oidc/`)

Autentica utenti tramite un OIDC provider (Google, Azure AD, Keycloak).
Non verifica la firma crittografica del token - si fida del canale TLS.

### Prerequisiti

Serve un OIDC provider configurato. Tre opzioni:

#### Opzione A: Keycloak locale (la piu' veloce)

```bash
# Avvia Keycloak in Docker
docker run -d --name keycloak -p 8180:8080 \
  -e KC_BOOTSTRAP_ADMIN_USERNAME=admin \
  -e KC_BOOTSTRAP_ADMIN_PASSWORD=admin \
  quay.io/keycloak/keycloak:latest start-dev
```

Apri http://localhost:8180 e accedi come admin/admin. Poi:

1. Crea un realm (es. `test`)
2. Crea un client:
   - Client ID: `dmvc-oidc-sample`
   - Client authentication: ON
   - Valid redirect URIs: `http://localhost:8080/auth/callback`
   - Valid post logout redirect URIs: `http://localhost:8080`
   - Web origins: `http://localhost:8080`
3. In "Credentials" copia il Client Secret
4. Crea un utente di test (es. `testuser` / `testpass123`)

I valori per `.env`:
```
OIDC_ISSUER=http://localhost:8180/realms/test
OIDC_CLIENT_ID=dmvc-oidc-sample
OIDC_CLIENT_SECRET=<client secret copiato>
```

#### Opzione B: Google

1. Vai su https://console.cloud.google.com/apis/credentials
2. Crea un progetto (o usa uno esistente)
3. Crea credenziali -> OAuth 2.0 Client ID -> Web Application
4. Authorized redirect URIs: `http://localhost:8080/auth/callback`
5. Annota Client ID e Client Secret

I valori per `.env`:
```
OIDC_ISSUER=https://accounts.google.com
OIDC_CLIENT_ID=<your-client-id>.apps.googleusercontent.com
OIDC_CLIENT_SECRET=<your-client-secret>
```

#### Opzione C: Azure AD / Entra ID

1. Vai su https://portal.azure.com -> App registrations -> New registration
2. Redirect URI: `http://localhost:8080/auth/callback` (tipo Web)
3. In "Certificates & secrets" crea un client secret
4. Annota Application (client) ID e Directory (tenant) ID

I valori per `.env`:
```
OIDC_ISSUER=https://login.microsoftonline.com/<tenant-id>/v2.0
OIDC_CLIENT_ID=<application-id>
OIDC_CLIENT_SECRET=<client-secret-value>
```

### Configurazione

```bash
cd samples/middleware_oidc
cp .env.example .env
# Modifica .env con i valori del tuo provider
```

Il file `.env` completo:
```
DMVC_SERVER_PORT=8080
OIDC_ISSUER=http://localhost:8180/realms/test
OIDC_CLIENT_ID=dmvc-oidc-sample
OIDC_CLIENT_SECRET=<il-tuo-secret>
BASE_URL=http://localhost:8080
JWT_SECRET=cambia-questa-stringa-con-almeno-32-caratteri-random
JWT_EXPIRATION_HOURS=8
DMVC_VIEW_PATH=templates
```

### Avvio e test

Compila ed esegui `OIDCSample.dproj`. Server su porta **8080**.

1. Apri http://localhost:8080 -> pagina pubblica
2. Clicca "Login" -> redirect al provider OIDC
3. Autenticati con le credenziali del provider
4. Redirect a http://localhost:8080/auth/callback -> sessione creata
5. http://localhost:8080/dashboard -> mostra i claim OIDC (email, nome, sub)
6. Clicca "Logout" -> sessione invalidata

### Endpoint

| Endpoint | Protezione | Descrizione |
|----------|-----------|-------------|
| `GET /` | Pubblica | Landing page |
| `GET /dashboard` | Protetta | Mostra claim OIDC |
| `GET /auth/login` | Pubblica | Redirect al provider |
| `GET /auth/callback` | Pubblica | Callback OIDC |
| `GET /auth/logout` | Pubblica | Logout |

---

## 3. OIDC con verifica JWKS (`samples/middleware_oidc_jwks/`)

Come il precedente, ma verifica crittograficamente la firma dell'ID token
usando le chiavi pubbliche del provider (scaricate dal JWKS endpoint).

### Prerequisiti aggiuntivi rispetto al sample 2

- OpenSSL DLLs nella directory dell'eseguibile
  - Win32: `libcrypto-3.dll` + `libssl-3.dll`
  - Win64: `libcrypto-3-x64.dll` + `libssl-3-x64.dll`
- Stesso OIDC provider configurato come per il sample 2

### Configurazione

```bash
cd samples/middleware_oidc_jwks
cp .env.example .env
# Modifica .env con i valori del tuo provider
```

Il file `.env` ha un campo aggiuntivo:
```
DMVC_SERVER_PORT=8080
OIDC_ISSUER=http://localhost:8180/realms/test
OIDC_CLIENT_ID=dmvc-oidc-sample
OIDC_CLIENT_SECRET=<il-tuo-secret>
BASE_URL=http://localhost:8080
JWT_SECRET=cambia-questa-stringa-con-almeno-32-caratteri-random
JWT_EXPIRATION_HOURS=8
OIDC_VERIFY_SIGNATURE=true
DMVC_VIEW_PATH=templates
```

Con `OIDC_VERIFY_SIGNATURE=true` il middleware:
1. Scarica le chiavi pubbliche dal JWKS endpoint del provider
2. Le mette in cache per 1 ora
3. Verifica la firma dell'ID token ad ogni login
4. Se il `kid` non e' in cache, fa refetch (gestisce la key rotation)

Con `OIDC_VERIFY_SIGNATURE=false` si comporta come il sample 2 (TLS trust).

### Avvio e test

Compila ed esegui `OIDCJWKSSample.dproj`. Server su porta **8080**.

Il flusso di test e' identico al sample 2. La differenza e' nei log della console:
- Con verifica: `OIDC: ID token signature verified successfully via JWKS`
- Senza verifica: `OIDC: ID token signature verification not configured`

### Verifica che la firma funzioni

Per testare che la verifica JWKS sia realmente attiva:

1. Avvia il sample con `OIDC_VERIFY_SIGNATURE=true`
2. Fai login -> nei log vedrai il messaggio di verifica OK
3. Controlla che il log mostri `JWKS provider configured`
4. Al primo login vedrai anche il fetch delle chiavi dal JWKS endpoint

### Algoritmi supportati dal JWKS client

| Tipo chiave | Algoritmi |
|------------|-----------|
| RSA | RS256, RS384, RS512 |
| RSA-PSS | PS256, PS384, PS512 |
| EC (P-256/384/521) | ES256, ES384, ES512 |
| OKP (Ed25519) | EdDSA |

Il provider tipicamente usa RS256 (Google, Azure AD, Keycloak tutti usano RSA).

---

## Ordine consigliato per il test

1. **`TestJWTSigners.dpr`** - Test unitario, zero config, verifica che OpenSSL funzioni
2. **`JWTAsymmetricAuthSample.dproj`** - Server con JWT RS256, test con curl
3. **`OIDCSample.dproj`** - OIDC base con Keycloak locale
4. **`OIDCJWKSSample.dproj`** - OIDC + JWKS, stesso Keycloak

I sample 3 e 4 usano lo stesso provider e le stesse credenziali.
Basta configurare Keycloak una volta e poi usare lo stesso `.env` (copiato) in entrambi.

---

## Troubleshooting

### "Cannot load OpenSSL libraries"
Mancano le DLL di OpenSSL. Copiale nella directory dell'eseguibile.
Le trovi in `C:\DEV\dmvcframework\lib\taurustls\` oppure scaricale da
https://github.com/nickelsworth/openssl-binaries

### "OIDC: Discovery failed"
L'issuer URL non e' raggiungibile. Verifica che Keycloak sia attivo e che
l'URL sia corretto (incluso il nome del realm).

### "OIDC: ID token audience mismatch"
Il `OIDC_CLIENT_ID` nel `.env` non corrisponde a quello configurato nel provider.

### "OIDC: ID token issuer mismatch"
L'issuer nel token non corrisponde a `OIDC_ISSUER`. Con Keycloak deve essere
esattamente `http://localhost:8180/realms/<nome-realm>`.

### "OIDC: No matching key found in JWKS"
Il provider usa un algoritmo/chiave non supportata, oppure il JWKS endpoint
non e' raggiungibile. Controlla i log per il kid richiesto.

### Redirect callback ritorna errore
Verifica che nel provider OIDC il redirect URI sia esattamente
`http://localhost:8080/auth/callback` (con http, non https).
