---
name: dmvcframework-ui
description: Use when styling or laying out the HTML of a DMVCFramework web application — adding a page, a nav entry, a card, a form, a toast, a theme toggle, or touching style.css. Covers the presentation layer the DMVCFramework wizard actually generates: Bootstrap 5.3 (loaded from CDN in baselayout), the brand tokens in style.css, dark/light mode via data-bs-theme, and the HTMX activity classes. Triggers on "style.css", "Bootstrap", "dark mode", "theme toggle", "navbar", "card", "toast", "hero", "CSS", "layout", "responsive", "look and feel", "brand colors".
---

# DMVCFramework — UI layer (Bootstrap 5.3 + style.css)

The wizard's web application ships a complete presentation layer. **Use it — do not invent one.**
Before writing any markup or CSS, know these three facts:

1. **Bootstrap 5.3 is already loaded** from CDN in `baselayout.html` (CSS in `<head>`, `bootstrap.bundle.min.js`
   before `</body>`). Every stock component — grid, cards, navbar, buttons, forms, alerts, badges, modals,
   spinners, toasts — is available. Do not add another CSS framework, and do not hand-roll what Bootstrap has.
2. **`style.css` is deliberately small** (~100 lines). It is *not* a design system: it sets brand tokens,
   repaints Bootstrap's primary colour, and adds four helpers. Read it before adding anything to it.
3. **Dark mode is Bootstrap's**, driven by `data-bs-theme` on `<html>`. Never write a `.dark-mode` class or a
   `prefers-color-scheme` media query — use Bootstrap's theme-aware variables and they follow the toggle for free.

**STOP — this skill works inside a wizard-generated project.** It describes the layout and stylesheet the
DMVCFramework IDE wizard produces. If there is no wizard project in the current folder (no
`bin/templates/baselayout.html`, no `bin/www/css/style.css`), stop and tell the user to create one first
(**Delphi IDE → File → New → Other → Delphi Projects → DelphiMVCFramework → New DMVCFramework Application**,
preset **Web Application**, defaults accepted — the server backend is **Indy Direct**), then `cd` into the
folder and start you there. **Read `baselayout.html` and `style.css` before writing any markup or CSS.**

**COMPANION SKILL:** `dmvcframework-webapp` — the server side (TemplatePro syntax, `RenderView`, `ViewData`,
HTMX, controllers). This skill is only about what the browser renders.

---

## When in doubt about an API — verify it, never guess

Never invent an identifier or answer from memory. If you need a signature this skill does not cover, ask the
user for the path to their **DelphiMVCFramework checkout** and read `sources/` (and the matching `samples/`
project); failing that, read the official repository —
[sources](https://github.com/danieleteti/delphimvcframework/tree/master/sources) ·
[samples](https://github.com/danieleteti/delphimvcframework/tree/master/samples).
If you still cannot verify it, **say so**. Where a skill and a sample disagree, **the sample wins**.

---

## 1. What `baselayout.html` gives you

Every page does `{{extends "../baselayout.html"}}` and fills blocks. The layout already provides the
`<!DOCTYPE>`, `<head>`, the navbar, `<main class="container">`, the footer, the toast container, Bootstrap's
JS, the theme toggle and the no-flash theme restore script.

**Blocks you override:**

| Block | Purpose |
|-------|---------|
| `title` | Page title in the browser tab |
| `body` | The page content — this is the one you always fill |
| `extra_css` | Extra `<link>`/`<style>` for this page only |
| `scripts` | Page-specific `<script>` before `</body>` |
| `extra_js` | Extra JS after `scripts` |

**Variables the controller sets** (available in any template): `app_name`, `dmvc_version`, `current_year`,
`page_id`.

A new page is just this:

```html
{{extends "../baselayout.html"}}

{{block "title"}}Customers{{endblock}}

{{block "body"}}
<h1 class="mb-4">Customers</h1>
<div class="row g-3">
  {{for c in customers}}
  <div class="col-md-4">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">{{:c.FirstName}} {{:c.LastName}}</h5>
        <p class="card-text text-secondary">{{:c.Email}}</p>
      </div>
    </div>
  </div>
  {{endfor}}
</div>
{{endblock}}
```

No `<html>`, no `<head>`, no stylesheet link, no container div. The layout owns those.

---

## 2. The navbar — adding an entry

Nav links live in `baselayout.html`. The active state is driven by `page_id`, which the controller sets:

```html
<li class="nav-item">
  <a class="nav-link {{if page_id|eq,"customers"}}active{{endif}}"
     {{if page_id|eq,"customers"}}aria-current="page"{{endif}}
     href="/web/customers">Customers</a>
</li>
```

```delphi
ViewData['page_id'] := 'customers';
```

Keep `aria-current` — it is what a screen reader announces. The nav is `navbar-expand-md`: it collapses into
the burger toggler below `md`, which already works. Do not remove the toggler.

---

## 3. Theme (dark / light)

`<html data-bs-theme="dark">` is the default; a toggle button in the navbar flips it and persists the choice in
`localStorage['dmvc-theme']`, and an inline script in `<head>` restores it **before first paint** (no flash).

To make your markup theme-aware, use Bootstrap's semantic classes and variables — never hard-coded colours:

| Instead of | Use |
|-----------|-----|
| `background: #fff` | `class="bg-body"` / `bg-body-tertiary` |
| `color: #333` | `class="text-body"` / `text-secondary` |
| `border: 1px solid #ddd` | `class="border"` |
| a custom colour in CSS | `var(--bs-body-bg)`, `var(--bs-border-color)`, `var(--bs-secondary-color)` |

These all flip automatically with the theme. A literal hex does not.

---

## 4. `style.css` — what is in it, and what to add

```css
:root {
  --brand-accent: #fbbf24;          /* amber-400 — the brand colour */
  --brand-accent-strong: #d4991a;
  --brand-font-body: 'Outfit', system-ui, sans-serif;
  --brand-font-mono: 'JetBrains Mono', ui-monospace, monospace;
}
```

- **Bootstrap's primary is repainted** to the accent (`--bs-primary`, `--bs-link-color`), so `.btn-primary`,
  `.text-primary` and links are already gold. Use `btn-primary`; do not write a custom gold button.
  (Note the file also overrides `.btn-primary`'s own `--bs-btn-*` vars explicitly — Bootstrap 5.3 bakes button
  colours at compile time, so overriding `--bs-primary` alone would not repaint them.)
- **Fonts**: Outfit for body, JetBrains Mono for `code/pre/kbd/samp`, imported from Google Fonts.

**The four helpers it defines** — use them instead of reinventing:

| Class | What it is |
|-------|-----------|
| `.hero` | Centred landing block with a subtle accent gradient. `<div class="hero"><h1>…</h1><p class="lead">…</p></div>` |
| `.section-label` | Small mono uppercase marker above a section |
| `.badge.brand-mono` | Monospace badge, for versions/ids |
| `.htmx-request` / `.htmx-indicator` | HTMX activity: the element dims while its request is in flight; `.htmx-indicator` shows only during it |

**Before adding a rule to `style.css`, check in this order:** does a Bootstrap utility do it (spacing `m-*`/`p-*`,
flex `d-flex`, text, colour)? Does a Bootstrap component do it? Only then write CSS — and write it against the
brand tokens and `--bs-*` variables, not literals.

---

## 5. Toasts

The layout defines `#toastContainer` and a global `showToast(message, type)` — `type` is `success`, `danger`,
`warning` or `info`. From an HTMX response, trigger it with a client event from Delphi:

```delphi
uses MVCFramework.HTMX;

// The payload is serialized as a JSON *value*. A string arrives at the client as evt.detail.value —
// NOT as evt.detail.<something>. This is the shape the framework's own sample uses.
Context.Response.HXTriggerClientEvent('showMessage', 'Customer saved');
```

```html
{{# in the page's "scripts" block #}}
<script>
  document.body.addEventListener('showMessage', function (evt) {
    showToast(evt.detail.value, 'success');
  });
</script>
```

Need structured data instead of a plain string? Pass an **object** and read its properties:

```delphi
Context.Response.HXTriggerClientEvent('customerSaved', lCustomer);   // a Delphi object
```
```javascript
document.body.addEventListener('customerSaved', function (evt) {
  showToast('Saved ' + evt.detail.FirstName, 'success');
});
```

Do not hand-build a JSON string for the payload — you get a *quoted string* on the client, not an object.
And do not write your own toast markup or `alert()`.

---

## 6. Forms

Stock Bootstrap, plus HTMX for the submit. Server-side validation errors come back as HTML.

```html
<form hx-post="/web/customers" hx-target="#form-wrap" hx-swap="outerHTML" class="row g-3">
  <div class="col-md-6">
    <label for="firstName" class="form-label">First name</label>
    <input type="text" class="form-control {{if errors.FirstName}}is-invalid{{endif}}"
           id="firstName" name="firstName" value="{{:customer.FirstName}}" required>
    {{if errors.FirstName}}<div class="invalid-feedback">{{:errors.FirstName}}</div>{{endif}}
  </div>
  <div class="col-12">
    <button type="submit" class="btn btn-primary">Save</button>
  </div>
</form>
```

`is-invalid` + `.invalid-feedback` is Bootstrap's error pattern — it needs no JS. Every input needs a `<label for>`.

---

## 7. Common mistakes

| Mistake | Why it is wrong |
|---------|-----------------|
| Adding Tailwind / Pico / a second framework | Bootstrap 5.3 is already loaded. Two frameworks = two resets fighting. |
| Writing a custom grid / flexbox layout | `row`/`col-*`, `d-flex`, `gap-*` already exist. |
| Hard-coded `#fff` / `#333` in CSS or `style=""` | Breaks in the other theme. Use `bg-body` / `text-body` / `--bs-*` vars. |
| A `.dark-mode` class or `prefers-color-scheme` query | Dark mode is `data-bs-theme`. Yours will not follow the toggle. |
| A custom gold button | `.btn-primary` is already the brand accent. |
| Hand-rolled toast / modal / dropdown | Bootstrap ships them; `showToast()` is already global. |
| `<link>`ing style.css or Bootstrap in a page template | The layout does it. A child template only fills blocks. |
| Restyling `.htmx-request` | It is already wired to the HTMX request lifecycle. |

---

## 8. Files

| File | What it is |
|------|-----------|
| `bin/templates/baselayout.html` | The layout: head, navbar, main, footer, toasts, theme toggle, Bootstrap JS |
| `bin/www/css/style.css` | Brand tokens, primary override, 4 helpers, HTMX classes (~100 lines) |
| `bin/www/` | Static root, served at `/static` by `TMVCStaticFilesMiddleware` |

Bootstrap docs: https://getbootstrap.com/docs/5.3/ — read the component page rather than guessing class names.
