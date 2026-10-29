# CLAUDE.md

@AGENTS.md

## Every release: the top of README.md

At every release, a release candidate or a final one, check and update the header of `README.md` before publishing. It is the first thing people see on GitHub, and it goes stale silently.

- **`stable` and `release candidate` badges**: the text names the version, the link goes to that release's tag (`https://github.com/danieleteti/delphimvcframework/releases/tag/v<version>`). Same tag for the "Release candidate" line in Quick Links. At a final release the stable badge moves to it, and the release candidate badge and line go until the next RC.
- **Every badge renders and every link answers**: relative links point to files that exist (the license is `License.txt`), absolute ones answer 200. A badge image served by a site that refuses non-browser clients shows broken on GitHub, because GitHub fetches it through its image proxy: DeepWiki answers 429, so its badge is a static shields.io image. LinkedIn and some sponsor sites block `curl` but work in a browser.
- A release candidate is published on GitHub as a **pre-release**, so "Latest" stays on the stable release; only a final release becomes "Latest".

## Naming convention

**Fields use a lowercase `f` prefix**: `fItems`, `fName`. Not `FItems`. Locals use `l` (`lName`). New code follows this; do not introduce `F`-prefixed fields.

The local SonarQube (project key `dmvcframework`, Delphi plugin, quality profile `DMVC`, a copy of "Sonar way comprehensive") does **not** enforce it: both naming rules for this are **deactivated** in the profile.

- `VariableName` has no parameter for the `l`/`a` prefixes used here (it flagged ~5000 correct parameters and locals).
- `FieldName` takes a single, case-sensitive prefix: with `F` (default) it flagged ~620 correct `f` fields, with `f` it flags the ~650 that use `F`. It was switched off while the code is not uniform. Re-activate it with `prefixes=f` once the fields are unified.

Not uniform yet: ~650 fields in the sources still use `F` (e.g. `MVCFramework.HttpSys.*`, `Middleware.JWT`, `Validators`, `SSE.Writer`). Unifying them to `f` is a rename; **before a mass rename check that nothing depends on the field name** (RTTI, field serialization, `MVCNameAs`), and do it in a commit of its own, not mixed with functional changes.

To find the remaining ones, grep the sources for `^\s+F[A-Z]\w*\s*:` or re-activate `FieldName` (`prefixes=f`) in the `DMVC` profile and re-run the analysis.
