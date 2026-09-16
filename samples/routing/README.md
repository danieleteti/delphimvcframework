# Routing sample

Shows how `[MVCPath]`, `[MVCHTTPMethod]`, `[MVCProduces]` and `[MVCConsumes]`
combine to select an action, including two paths mapped to the same controller
(`/` and `/api`) and two paths mapped to the same action.

Run it and the server listens on **http://localhost:8080**.

## Searching twice: GET and QUERY

The sample deliberately contains the same search written both ways.

**`GET /api/searches/(searchtext)`** — the traditional shape. The criteria live
in the URL: the text as a path segment, the rest as query-string parameters.

```
curl "http://localhost:8080/api/searches/rossi?page=2&order=lastname" \
     -H "Accept: text/plain"
```

This is fine while the criteria stay flat. It stops being fine as soon as they
do not: a list of cities and a price range have no standard URL encoding, so
each project invents one — `cities=rome,milan&min=10&max=90` — and then writes a
parser for its own invention on the server.

**`QUERY /api/customers/searches`** — the same search with a JSON body:

```
curl -X QUERY http://localhost:8080/api/customers/searches \
     -H "Content-Type: application/json" \
     -d '{"searchtext":"rossi","cities":["rome","milan"],"pricerange":{"min":10,"max":90},"orderby":"lastname","page":2}'
```

`QUERY` (RFC 10008) is the only HTTP method that is safe and idempotent *and*
carries a body. It is a read — free to be retried or prefetched, like `GET` —
whose criteria travel as JSON. The action receives a deserialized object and
contains no parsing code at all.

`POST /searches` is what people reach for instead, and it works, but it tells
every cache, proxy and client library that the request changes something.

### Things to know before using it

- **For CSRF, treat `QUERY` like `POST`, not like `GET`.** It is safe by
  specification, but it carries a body and a same-origin request is not
  preflighted.
- **Cross-origin callers need `QUERY` in the allowed methods explicitly** — the
  CORS default does not include it.
- **A browser address bar cannot issue it.** Use `curl`, or from JavaScript
  `fetch(url, {method: 'QUERY', body: ...})` — the Fetch specification allows
  it, forbidding a body only on `GET` and `HEAD`.
- **It does not appear in the generated Swagger/OpenAPI document.** The `query`
  slot exists only in OpenAPI 3.2, and the current emitters target older
  versions.
- **Adoption is the real limit, and it is not about clients.** Every generic
  HTTP client already sends `QUERY` today. What tends to reject it are the
  intermediaries in front of your server — proxies, WAFs, CDNs — which drop
  methods they do not recognise. IIS Express and Apache 2.4 both pass it
  through; anything else in your chain is worth testing before you rely on it.
