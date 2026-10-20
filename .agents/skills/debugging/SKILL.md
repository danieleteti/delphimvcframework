---
name: debugging
description: Control execution, manage breakpoints, and inspect state in RAD Studio.
---

# Debugging

## When to use this skill

Use this skill during active debugging sessions in RAD Studio to control the process, inspect variables, and manage breakpoints.

## How to control execution

- **Start/Continue**: Use `runProject()`.
- **Step Over**: Use `stepOver()`.
- **Step Into**: Use `stepInto()`.
- **Stop**: Use `terminateProcess()`.

## How to inspect state

1. **Call Stack**: Use `getCallStack()` when the debugger is stopped.
2. **Variables**: Use `evaluateExpression(expression)` to check values.
3. **Navigation**: Use `goToEditorPosition` to jump to relevant code locations found in the call stack.

## Breakpoint Management

- **Add**: `addBreakpoint(filepath, line)`.
- **Remove**: `removeBreakpoint(filepath, line)` or `removeAllBreakpoints()`.
- **List**: `listBreakpoints()`.
