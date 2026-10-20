# CLAUDE.md

@AGENTS.md

## Naming convention

**Fields use a lowercase `f` prefix**: `fItems`, `fName`. Not `FItems`. Locals use `l` (`lName`). New code follows this; do not introduce `F`-prefixed fields.

The local SonarQube (project key `dmvcframework`, Delphi plugin, quality profile `DMVC`, a copy of "Sonar way comprehensive") does **not** enforce it: both naming rules for this are **deactivated** in the profile.

- `VariableName` has no parameter for the `l`/`a` prefixes used here (it flagged ~5000 correct parameters and locals).
- `FieldName` takes a single, case-sensitive prefix: with `F` (default) it flagged ~620 correct `f` fields, with `f` it flags the ~650 that use `F`. It was switched off while the code is not uniform. Re-activate it with `prefixes=f` once the fields are unified.

Not uniform yet: ~650 fields in the sources still use `F` (e.g. `MVCFramework.HttpSys.*`, `Middleware.JWT`, `Validators`, `SSE.Writer`). Unifying them to `f` is a rename; **before a mass rename check that nothing depends on the field name** (RTTI, field serialization, `MVCNameAs`), and do it in a commit of its own, not mixed with functional changes.

To find the remaining ones, grep the sources for `^\s+F[A-Z]\w*\s*:` or re-activate `FieldName` (`prefixes=f`) in the `DMVC` profile and re-run the analysis.
