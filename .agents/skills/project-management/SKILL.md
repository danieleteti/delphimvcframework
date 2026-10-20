---
name: project-management
description: Manage RAD Studio project structure, adding or removing units and forms.
---

# Project Management

## When to use this skill

Use this skill when creating new units, adding existing files to a project, or removing files from the project structure in RAD Studio.

## How to add a new file to a project

1. **Create on Disk**: First, create the file physically on the filesystem.
2. **Register in Project**: Use `addFileToProject(filepath, isunitorform)`.
3. **IDE Visibility**: Only call `openFile(filepath, false)` if the user explicitly asked to see the new file. Otherwise, keep it closed to avoid stealing focus.

## How to remove a file

1. **Unregister**: Use `removeFileFromProject(filepath)`.
2. **Disk Cleanup**: Do not delete the file from disk unless the user explicitly requests it.

## Project Information

- Use `getProjectInfo(projectfile)` to retrieve the framework, targets, and file list.
- Use `openProject(filepath, newprojectgroup)` to switch between projects.
