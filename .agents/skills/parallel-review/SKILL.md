---
name: parallel-review
description: Use before merging or shipping anything non-trivial, or when the user asks for a code review, an audit, a pre-release check, or "review this branch". Splits the review across independent agents, one lens each, each required to prove or drop its findings. Triggers on "review this branch", "code review", "pre-merge review", "audit this", "can I deploy", "release check", "is this ready", "review before merge".
---

# Parallel review

One reviewer with five concerns finds the first defect, attaches to it, and files
everything else as minor. Five reviewers with one concern each do not contaminate
one another. That is the whole idea; the rest of this file is how to keep them
honest.

Reach for this when the change is big enough that missing something costs more
than the review. A two-line fix does not need it.

## What you write before you dispatch

The reusable part of a review brief is the method. The part that actually finds
defects is the part only you can write, because it comes from this change.

**Do not dispatch until you can name, for each lens, two or three things to check
hard, specific to this diff.** "Look for bugs" produces a report about naming
conventions. "Is the UPSERT genuinely the last statement before every commit on
both ingest paths?" produces an answer. If you cannot write those, you have not
read the change yet. Go read it.

Get the diff first and look at it yourself:

```
git diff --stat <base>...HEAD
git diff <base>...HEAD -- <the code paths, not generated files or binaries>
```

Scope the agents to source. Sending them lock files, build output, generated
HTML or PDFs wastes their context on noise.

## The lenses

Five is a default, not a law. Drop the ones that do not apply, add one the change
demands (concurrency, migrations, accessibility, protocol compatibility).

| Lens | Its question |
|---|---|
| **Correctness** | Logic errors, broken invariants, edge cases, error paths that swallow failures |
| **Simplification** | What can be deleted. Leftovers from removed machinery, abstractions with one caller |
| **Doubt** | Falsify the change's own claims. Not "is it good", but "which promise can I break" |
| **Performance** | Cost per request, new serialization points, query plans, what got slower |
| **Security** | Trust boundaries, authorization (not just authentication), injection, what leaks |

The doubt lens is the one people skip and the one that pays. Give it the list of
claims the change makes, in the change's own words, and tell it to attack them.

## What every brief must contain

1. **Read-only.** No edits, no commits, no writes. It is a review.
2. **The context**, in a paragraph: what the change does and why. Not the commit
   log, your own summary.
3. **The lens**, and the two or three change-specific checks.
4. **The project's own rules** that a finding could violate. Do not paste them:
   say "read AGENTS.md / AGENTS.md and hold findings against it". Copied rules
   drift from the originals, which is the defect you are reviewing for.
5. **The evidence bar**: for every candidate finding, read the actual code and
   state a concrete failure scenario, inputs to wrong outcome. Discard whatever
   cannot be substantiated that way. Mark each CONFIRMED (read it) or PLAUSIBLE.
6. **The out-of-scope list**: known issues, accepted trade-offs, deliberate
   duplication. Without it several agents rediscover the same known thing and
   drown the real findings.
7. **Permission to find nothing.** Say it in those words: "an empty list is an
   acceptable answer". Without it an agent invents a finding rather than come
   back with nothing.

Dispatch them in a single message so they run concurrently.

## Reading what comes back

**Agreement across lenses is the strongest signal you get.** Two agents that
never spoke to each other reaching the same file and line means it is real. Rank
those first, whatever severity each one assigned.

**A finding is not true because an agent said so.** They are confident about
things they inferred. Before acting: does the file say what the report says it
says? Reports overclaim, especially about what a test covers.

**The best findings arrive as questions you cannot answer.** "This guard scans
`app/` only, and the change added a violation in `admin/`" is worth more than ten
style notes.

Cheap fixes and real defects arrive mixed together. Fix them in one pass, not one
commit per report.

## After the fixes

**Prove every behavioural fix by breaking the code on purpose.** Revert the fix,
run the new test, watch it fail, restore. A test written after a fix, never seen
red, is a decoration: it can assert the wrong thing and stay green forever. This
costs a minute and catches the failure mode that recurs most.

Then re-run whatever gate the project has. If a guard or a pattern was widened,
run the evasions against it and confirm the real tree stays clean, so you know
you gained coverage instead of noise.

## Where this fails

- **The agents cannot supply the change-specific checks.** They write good general
  reviews and miss what matters here. That part is yours.
- **Cost is real**: five agents on a large diff is not free. Match the count to
  the change.
- **They do not know what the project already accepted.** That is what the
  out-of-scope list is for, and forgetting it is the most common way one of these
  reviews turns into noise.
