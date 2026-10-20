---
name: council
description: Run a model-diverse subagent council to investigate the same problem from multiple perspectives, compare findings, and produce a final recommendation. Use this skill whenever the user asks for a council, second opinions, multiple agents/models to evaluate one question, parallel investigation, red-team/blue-team comparison, or help deciding between competing technical approaches.
---

# Council

Use this skill to coordinate multiple subagents investigating the same question, with different models first and different assigned perspectives second, then synthesize their reports into one recommendation.

This skill is best for judgment-heavy tasks: architecture tradeoffs, risky bug fixes, code review red-teaming, rollout decisions, incident analysis, and “is this alternative worth pursuing?” questions.

## Workflow

### 1. Frame the council question

State the decision the council should answer in one sentence. Identify:

- the competing options or hypothesis under review;
- the codebase, branch, PR, issue, design, or artifact to inspect;
- whether agents should be read-only or may make code changes;
- the final decision criteria, such as correctness, risk, implementation cost, testability, rollout safety, or product behavior.

If the user’s request is ambiguous, ask only the minimum clarification needed. Otherwise choose sensible defaults and proceed.

### 2. Choose council members

Prioritize model diversity. A council should not default to three agents on the same model with different angles; use that only when the available launch configuration cannot provide multiple useful models, or when the user explicitly asks for one model. If model diversity is unavailable, say so briefly before falling back to perspective-only diversity.

Preferred default roster for a three-member council:

- Opus 4.7 or the strongest available Codex/Opus reasoning model: architecture, correctness, and edge-case analysis.
- GPT 5.5 or the strongest available GPT/Codex model: implementation-grounded review, feasibility, and test strategy.
- An open-source model such as Kimi 2.6, GLM 5.1, or the strongest available OSS/local model: contrarian critique, hidden assumptions, and alternative framing.

If one of these exact models is unavailable in the active harness, use the closest available model from that family and note the substitution. If no open-source model is available, use a third distinct frontier model if possible; otherwise use the strongest remaining model with a deliberately adversarial or specialist angle.

Assign both a model and an angle to each member. Avoid making the angles redundant with the models; for example, do not ask all members to do general architecture review. Useful angle combinations include:

- architect/correctness reviewer;
- implementation/testability reviewer;
- red-team, security, performance, or product-risk reviewer;
- contrarian “argue against the obvious solution” reviewer.

When different children need different models, launch them in separate `run_agents` calls because model selection is run-wide. If the requested model resolves differently than expected, treat the resolved launch settings as authoritative and continue unless they make the task infeasible.

When using non-default harnesses, choose valid model IDs for that harness. For example, Codex may expose `Codex-opus-4-7`, Codex may expose `gpt-5.5`, and open-source models depend on the currently configured local or remote provider. Do not invent unsupported model IDs; if a desired model is not available, select the closest supported substitute and preserve the intended angle diversity.

For read-only investigations, keep all children in the same checkout and explicitly tell them not to edit files. For implementation or prototyping councils, give each local child its own git worktree and branch so they cannot collide.

### 3. Brief before launching

For explicit orchestration requests, briefly tell the user which council members you plan to launch and what each will investigate, then wait for approval before calling `run_agents`.

The shared brief should include:

- repository path or artifact location;
- current branch or base context;
- the exact question to answer;
- relevant background and known concerns;
- required files/symbols to inspect, if known;
- constraints, especially read-only/no commits/no PRs;
- expected report format.

Keep launch prompts short enough that task titles stay compact. If a long brief causes launch validation issues, launch with a minimal prompt and send the full brief to the child agents immediately afterward.

### 4. Ask for structured reports

Ask every council member to return:

1. exact file paths, symbols, docs, or evidence inspected;
2. the current behavior or current implementation;
3. the alternative being evaluated;
4. correctness risks and edge cases;
5. implementation and testing cost;
6. recommendation: keep current approach, pursue alternative, or use a hybrid;
7. confidence level and unknowns.

Encourage independence. Do not share one child’s findings with the others unless you are intentionally doing a second-round critique.

### 5. Collect reports

Read completion messages as they arrive. Do not rely on lifecycle success alone; the useful output is in the child’s report.

If a report is missing key evidence or makes an unsupported claim, send a focused follow-up question to that same child rather than launching a replacement. Reuse existing children for follow-ups because they retain context.

### 6. Synthesize the recommendation

Compare the reports by evidence quality, not by vote count. In the final answer:

- lead with the recommendation;
- call out consensus and disagreements;
- explain why the recommended option wins against the decision criteria;
- explicitly address the user’s stated concern;
- include relevant file paths/symbols without overloading the answer;
- distinguish “do now” from optional future hardening;
- mention confidence and material unknowns.

Prefer a concise decision memo over a transcript summary. The user needs the distilled recommendation, not every intermediate detail.

## Final answer template

Use this shape unless the task calls for something different:

```markdown
## Recommendation

[One or two sentences with the decision.]

## Why

- [Key reason 1]
- [Key reason 2]
- [Key reason 3]

## Tradeoffs and risks

- [Risk or caveat]
- [Testing/rollout implication]

## Final call

[Concrete next step: merge current change, pursue alternative, hybrid, run tests, etc.]
```

## Practical notes

- If the council is read-only, tell children not to modify files, commit, create branches, or open PRs.
- If the council involves PR or branch work, follow the repository’s normal version-control rules and use isolated worktrees for parallel local edits.
- If the council is about code review feedback, mark review comments resolved only after the underlying issue is actually addressed.
- Do not expose internal child agent IDs in user-facing summaries unless the user explicitly asks for them.
