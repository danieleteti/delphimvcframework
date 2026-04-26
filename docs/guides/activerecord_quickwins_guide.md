# TMVCActiveRecord Quick Wins Guide

Three additive features added to `TMVCActiveRecord`. All opt-in — existing
classes are unaffected.

1. **Change Tracking** — UPDATE only the columns that actually changed.
2. **`foRefresh` field option** — refetch server-set values (DB DEFAULT,
   computed columns, triggers) after INSERT/UPDATE.
3. **Soft Delete** — turn `Delete` into a marker UPDATE; SELECT auto-filters
   soft-deleted rows.

## Change Tracking

### Why

A naive `Update` writes every Updatable column, even those the user did
not change. For wide tables, this means more bytes on the wire, more
trigger firing, more contention with concurrent transactions. Change
tracking emits SET only for the fields whose value actually differs
from the last persisted state.

`IsChanged(FieldName)` lookup is **case-insensitive** (uses `SameText`),
so `IsChanged('total')`, `IsChanged('Total')`, `IsChanged('TOTAL')` are
all equivalent.

### Enable

```pascal
[MVCTable('orders')]
[MVCChangeTracking]                // class-level, opt-in
TOrder = class(TMVCActiveRecord)
  ...
end;
```

### Use

```pascal
lOrder := TMVCActiveRecord.GetByPK<TOrder>(42);
try
  lOrder.Total := 99.99;
  Assert(lOrder.IsChanged);
  Assert(lOrder.IsChanged('total'));
  Assert(not lOrder.IsChanged('notes'));
  lOrder.Update;                  // emits: UPDATE orders SET total = ?, version = version + 1 WHERE id = ? AND version = ?
  Assert(not lOrder.IsChanged);     // snapshot rehashed in-memory after Update
finally
  lOrder.Free;
end;
```

### Edge cases

- Calling `Update` with **zero changed fields** still emits SQL: SET
  becomes `pk = pk` (no-op data change). The statement runs so triggers
  fire, version increments where applicable, and `OnAfterUpdate`
  invokes. This intentional "lock-touch" behaviour matches the SQL
  conventional use of `UPDATE` to acquire a row-level write lock.
- Use `UpdateIfChanged` to opt out of that behaviour when you want a
  silent no-op:
  ```pascal
  lOrder.UpdateIfChanged;  // skips SQL entirely if nothing changed
  ```
- `UpdateIfChanged` on a class without `[MVCChangeTracking]` raises
  `EMVCActiveRecord` ("change tracking not enabled").
- Audit columns (`MVCAuditUpdatedAt` / `MVCAuditUpdatedBy`) are always
  included in the SET when their values change in `OnBeforeUpdate`.

### Memory cost

Hash-based snapshot. Per instance: `4 bytes × N tracked fields`. For 20
tracked fields = 80 bytes per instance. Independent of value sizes
(strings, blobs).

For instances of classes WITHOUT `[MVCChangeTracking]`, the snapshot is
not allocated — zero memory cost.

### Collision risk

CRC32 is not cryptographic. The chance that two different values for
the same field hash to the same Cardinal is approximately 1 in 4
billion. For business data this is negligible. The cost of a collision
is a false-negative: the field is considered unchanged and not
included in the UPDATE — a missed write. If your data domain has
adversarial inputs (e.g. user-supplied values that could be crafted to
collide), do not rely on change tracking for correctness; use
unconditional `Update` instead.

## `foRefresh` field option

### Why

Some columns are populated by the database, not by the application:
- Server-side defaults (`created_at TIMESTAMP DEFAULT NOW()`).
- Computed columns (`full_name AS first_name || ' ' || last_name`).
- Triggers that modify the row on INSERT or UPDATE.

After persisting, the in-memory instance must be updated with the
DB-assigned values. `foRefresh` makes this declarative.

### Semantics — refetch through RETURNING / OUTPUT

`foRefresh` instructs the framework: "after every `Insert` / `Update`,
re-fetch this column from the database so any server-side derivation
is reflected in the in-memory instance — through the same round-trip
as the write whenever the engine allows it."

How it shows up in SQL:

| Engine | INSERT | UPDATE |
|---|---|---|
| PostgreSQL / Firebird / SQLite (3.35+) | `INSERT … RETURNING <pk>, <foRefresh cols>` | `UPDATE … RETURNING <foRefresh cols>` |
| Oracle (12c+) | `INSERT … RETURNING <cols> INTO :params` | `UPDATE … RETURNING <cols> INTO :params` |
| SQL Server | `INSERT … OUTPUT inserted.<pk>, inserted.<foRefresh cols>` | `UPDATE … OUTPUT inserted.<foRefresh cols> WHERE …` |
| MariaDB (10.5+) | `INSERT … RETURNING …` | Fallback SELECT (no UPDATE RETURNING) |
| MySQL | Fallback SELECT for both INSERT and UPDATE | Fallback SELECT |

The fallback SELECT — `SELECT <foRefresh cols> FROM <table> WHERE pk=?`
— costs one extra round-trip. The framework issues it automatically
and only on engines that do not provide native single-statement
support.

### Combining with [foReadOnly]

`foRefresh` does NOT make a column read-only. If your scenario is
"the user CAN write the value, but a BEFORE trigger may rewrite it"
— mark the field with `foRefresh` only:

```pascal
[MVCTableField('name', [foRefresh])]
FName: string;
```

If the column is fully DB-managed (computed / `GENERATED ALWAYS AS`,
or any column the user must not write) — combine `foRefresh` with
`foReadOnly`:

```pascal
[MVCTableField('upper_name', [foReadOnly, foRefresh])]
FUpperName: NullableString;
```

`foReadOnly` excludes the column from `INSERT` / `UPDATE`; `foRefresh`
ensures the framework refetches its DB-computed value through
`RETURNING` / `OUTPUT` after the write.

### When to use it

| Use case | foRefresh? |
|---|---|
| Column rewritten by a trigger (uppercase, normalize, audit-by-trigger) | ✅ |
| Computed/generated column the DB recomputes on every write | ✅ |
| Identity-style column whose actual stored value differs from the input | ✅ |
| Server-side `created_at` / `updated_at` timestamps | ❌ — use `[MVCAuditCreatedAt]` / `[MVCAuditUpdatedAt]` (framework-driven, easier to reason about) |
| Column with a DB DEFAULT that should fire only when the user did not supply a value | ❌ — `foRefresh` does not suppress the column from the INSERT, so the DEFAULT will not fire when the field is NULL-bound. Either omit the column from the entity, or let the user set it explicitly |

### Enable

```pascal
TPerson = class(TMVCActiveRecord)
  [MVCTableField('id', [foPrimaryKey, foAutoGenerated])]    FID: Int64;
  [MVCTableField('name')]                                    FName: string;
  // Computed column: DB stores UPPER(name). User does not write it.
  [MVCTableField('upper_name', [foReadOnly, foRefresh])]     FUpperName: NullableString;
end;
```

Matching DDL (per engine):
- PostgreSQL / SQLite / MySQL / MariaDB: `upper_name TEXT GENERATED ALWAYS AS (UPPER(name)) STORED`
- SQL Server: `upper_name AS (UPPER(name)) PERSISTED`
- Firebird: `upper_name COMPUTED BY (UPPER(name))`

### Use

Just call `Insert` or `Update` as usual. The framework refetches
`foRefresh` fields automatically.

```pascal
lP := TPerson.Create;
try
  lP.Name := 'alice';
  lP.Insert;
  // lP.FUpperName is now 'ALICE' — surfaced through the same RETURNING /
  // OUTPUT clause that brought back the auto-generated PK. Single round-trip.
finally
  lP.Free;
end;
```

## Soft Delete

### Why

Audit, retention, and recovery scenarios require keeping deleted rows
in the database with a marker rather than physically removing them.
Compliance regimes (GDPR right-to-erasure, financial record
retention) drive much of the demand.

### Enable

Field-level attribute. Field type drives the mode:

```pascal
// Timestamp mode (recommended)
[MVCTable('customers')]
TCustomer = class(TMVCActiveRecord)
  [MVCTableField('id', [foPrimaryKey, foAutoGenerated])] FID: Int64;
  [MVCTableField('name')] FName: string;
  [MVCTableField('deleted_at')]
  [MVCSoftDeleted]
  FDeletedAt: NullableTDateTime;  // <-- MUST be NullableTDateTime, NOT plain TDateTime
end;

// Flag mode (for legacy schemas)
[MVCTable('orders')]
TOrder = class(TMVCActiveRecord)
  [MVCTableField('id', [foPrimaryKey, foAutoGenerated])] FID: Int64;
  [MVCTableField('description')] FDescription: string;
  [MVCTableField('is_deleted')]
  [MVCSoftDeleted]
  FIsDeleted: Boolean;
end;
```

**Important — timestamp mode REQUIRES `NullableTDateTime`.** A plain
`TDateTime` defaults to `0.0`, which serializes to the date
`1899-12-30` (not NULL). The auto-filter `WHERE deleted_at IS NULL`
would never match → all rows always look "deleted". Use the nullable
wrapper.

### Use

#### Soft-delete an instance

```pascal
lCustomer.Delete;
// Equivalent to: UPDATE customers SET deleted_at = Now() WHERE id = ? AND version = ?
```

For flag mode: `SET is_deleted = TRUE` (or `= 1` on MSSQL).

#### Hard-delete an instance

```pascal
lCustomer.HardDelete;
// Always physical: DELETE FROM customers WHERE id = ? AND version = ?
```

Use this for GDPR right-to-erasure or retention cleanup. Bypasses the
soft-delete attribute entirely.

#### Restore an instance

```pascal
lCustomer.Restore;
// UPDATE customers SET deleted_at = NULL WHERE id = ? AND version = ?
```

Raises `EMVCActiveRecord` if the class lacks `[MVCSoftDeleted]` or if
the record is currently alive (`IsDeleted = False`).

#### Check soft-delete state

```pascal
if lCustomer.IsDeleted then ...
```

Returns `False` for classes without `[MVCSoftDeleted]` (graceful
default — calling `IsDeleted` is always safe).

#### Auto-filter SELECT

By default, every SELECT against a `[MVCSoftDeleted]` class
auto-injects a `WHERE deleted_at IS NULL` (or `WHERE is_deleted =
FALSE` for flag mode):

```pascal
lAlive := TMVCActiveRecord.All<TCustomer>;
// Returns only rows where deleted_at IS NULL
```

To see soft-deleted rows (audit screen, restore UI, GC):

```pascal
TMVCActiveRecord.IncludeSoftDeleted(True);
try
  lAll := TMVCActiveRecord.All<TCustomer>;
  // Returns ALL rows, alive and deleted
finally
  TMVCActiveRecord.IncludeSoftDeleted(False);
end;
```

The `IncludeSoftDeleted` flag is **thread-local** — safe in
multi-threaded servers as long as the try/finally is observed.

#### Bulk operations

```pascal
TMVCActiveRecord.DeleteRQL<TCustomer>('le(score, 0)');
// Smart: soft-delete classes get UPDATE; non-soft classes get physical DELETE.

TMVCActiveRecord.HardDeleteRQL<TCustomer>('le(score, 0)');
// Always physical DELETE, even for soft classes.

TMVCActiveRecord.RestoreRQL<TCustomer>('eq(name, "Alice")');
// Bulk UPDATE deleted_at = NULL.
```

### Known limitations

- `Select<T>(SQL, params)` and `SelectOne<T>(SQL, params)` accept
  literal SQL — the framework cannot inject the auto-filter. Add the
  `WHERE deleted_at IS NULL` clause manually if needed.
- Cascade soft-delete (deleting a parent automatically soft-deletes
  dependent children) is **not implemented**. It depends on
  relationship metadata which is part of the Lazy Loading quick win
  (deferred to a future round).

### How RQL auto-filtering works

`SelectRQL<T>`, `SelectOneByRQL<T>`, and `Count(RQL)` all auto-inject
the soft-delete predicate by wrapping the user RQL with an `and()`:

```pascal
// User writes:
TMVCActiveRecord.SelectRQL<TCustomer>('eq(active,true);limit(0,50)', 100);

// Framework rewrites internally to:
//   and(eq(deleted_at,null), eq(active,true)); limit(0,50)
```

Modifiers (`limit`, `sort`) are preserved unchanged — only the filter
portion before the first `;` is wrapped. Empty RQL becomes the
soft-delete predicate alone. Inside an `IncludeSoftDeleted(True)`
scope, the wrapping is skipped entirely.

## `IsNew` / `IsPersisted`

### Why

A common pattern is "Insert if new, otherwise Update". Without a helper
this requires either calling `Store` (which only works on nullable PKs)
or reaching into framework internals (`PKIsNull`, which raises on
non-nullable PKs).

`IsNew` is a single, safe read of the in-memory PK state that works
across all PK shapes.

### Use

```pascal
if AR.IsNew then
  AR.Insert
else
  AR.Update;

// Or, equivalently (when the PK is nullable):
AR.Store;
```

### Behaviour by PK shape

| PK declaration | `IsNew` returns True when... |
|---|---|
| `NullableInt64` / `NullableInt32` / `NullableString` / `NullableTGUID` | `HasValue` is False |
| Plain `Int64` / `Integer` | value is `0` |
| Plain `string` | value is `''` |
| Plain `TGUID` | value is the empty GUID |
| Class without a declared PK | always True (caller is mid-construction) |

`IsNew` never touches the database and never raises. `IsPersisted` is
just `not IsNew`, kept for readability at the call site.

## Interactions matrix

| Feature × Feature | Behaviour |
|---|---|
| Soft Delete × `foVersion` | Transparent. Soft-delete passes through UPDATE → version check + increment apply as usual. Bulk RQL forms (`DeleteRQL`, `HardDeleteRQL`, `RestoreRQL`) bypass version check (existing bulk semantics). |
| Soft Delete × Audit (row-level) | `MVCAuditUpdatedAt` / `MVCAuditUpdatedBy` fire on soft-delete and on `Restore` of a single instance (both go through the UPDATE path that calls `OnBeforeUpdate`). |
| Soft Delete × Validation | `OnInputValidate` and `OnStorageValidate` do **not** fire on soft-delete or Restore. These are marker operations, not content changes. |
| Soft Delete × Hooks | `OnBeforeDelete` / `OnAfterDelete` fire on both soft and hard delete. `OnBeforeUpdate` / `OnAfterUpdate` are suppressed during soft-delete dispatch (the user called Delete, not Update). |
| Soft Delete × `[MVCChangeTracking]` | Soft-delete changes one tracked field (the marker). Snapshot is rehashed normally on Update. `IsChanged` correctly reports the marker as the only changed field during the transition. |
| Bulk RQL (`DeleteRQL`, `HardDeleteRQL`, `RestoreRQL`) × Audit / Hooks / Change Tracking | **Bypassed.** Bulk forms emit raw `UPDATE` / `DELETE` SQL with a single round-trip — no per-row Delphi instance is materialized, so audit columns are NOT touched, lifecycle hooks (`OnBefore*` / `OnAfter*`) do NOT fire, and change-tracking snapshots are NOT rehashed. If you need any of those side-effects, iterate per-row and call `Delete` / `Restore` / `Update` on each instance. The trade-off is by design: bulk operations exist precisely to skip the per-row machinery. |
| `foRefresh` × `[MVCChangeTracking]` | Refresh runs after Insert/Update execute. The change-tracking snapshot is rehashed AFTER refresh, so the new snapshot reflects DB-truth values (not stale in-memory pre-Insert defaults). |
| `foRefresh` × `foVersion` | Independent. Version field is never `foRefresh`. |
| `IsNew` / `IsPersisted` × everything | Pure read of the PK in-memory state. Does not touch the DB, does not raise on PK-less classes (returns True). Safe to call on any TMVCActiveRecord instance at any lifecycle point. |

## What's deferred

- **Lazy loading** of relationships (`TMVCLazy<T>` / `TMVCLazyList<T>`,
  `[MVCBelongsTo]`, `[MVCHasMany]`). Designed but deferred to a
  future quick-wins round.
- **Composite primary keys**. Estimated 7-12 days of cross-cutting
  refactor; moved to 4.0-oxygen scope.
- **Cascade soft-delete**. Depends on lazy-loading metadata.
- **ZeosLib backend**. Designed for 4.0-oxygen as alternative to FireDAC.

## API reference

```pascal
// Class-level attributes
MVCChangeTrackingAttribute = class(MVCActiveRecordCustomAttribute);
MVCSoftDeletedAttribute   = class(MVCActiveRecordCustomAttribute);

// Field option
TMVCActiveRecordFieldOption = (
  foPrimaryKey, foAutoGenerated, foReadOnly, foVersion,
  foDoNotSelect, foDoNotInsert, foDoNotUpdate,
  foRefresh                                    // <-- new
);

// Instance methods on TMVCActiveRecord
function IsChanged: Boolean; overload;
function IsChanged(const FieldName: string): Boolean; overload;
function GetChangedFields: TArray<string>;
procedure UpdateIfChanged;

function IsDeleted: Boolean;
procedure HardDelete;
procedure Restore;

function IsNew: Boolean;            // PK unset -> true
function IsPersisted: Boolean;      // negation of IsNew, reads better at call site

// Class methods on TMVCActiveRecord
class procedure IncludeSoftDeleted(const Value: Boolean); static;
class function GetIncludeSoftDeleted: Boolean; static;

// Bulk operations on TMVCActiveRecordHelper (class helper)
class function DeleteRQL<T>(const RQL: string = ''): Int64;          // smart
class function HardDeleteRQL<T>(const RQL: string = ''): Int64;      // physical
class function RestoreRQL<T>(const RQL: string = ''): Int64;         // bulk un-delete
```

## See also

- Design document: `docs/superpowers/specs/2026-04-25-ar-quick-wins-design.md`
- Implementation plan: `docs/superpowers/plans/2026-04-25-ar-quick-wins.md`
- Streaming Enumerable Guide: `docs/guides/streaming_enumerable_guide.md` (separate feature)
