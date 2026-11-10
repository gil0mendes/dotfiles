---
description: Review staged changes, recent changes, or paths
argument-hint: "[recent | path ...]"
---
Delegate a read-only code review directly to `reviewer` using `team_spawn`. Put the complete scope and instructions below in the `task`, not just the brief. Do not call `team_list` first, review the scope yourself, or duplicate the delegated work.

<review-arguments>
$ARGUMENTS
</review-arguments>

Interpret the arguments as follows and include the applicable scope in the task:

- Empty: review Git's staged changes (`git diff --cached`) or jj's working-copy change (`jj diff -r @`, against its parent).
- Exactly `recent`: review the net tracked changes from Git `HEAD~1` through the working tree (`git diff HEAD~1`), including staged and unstaged changes but not untracked files; or from the parent of jj's previous change through the working copy (`jj diff --from '@--' --to @`), including both changes. Do not substitute a different range if history is missing or a parent is ambiguous.
- Otherwise: review the specified file(s) or directory as they exist in the working tree. Use `./recent` for a path named `recent`. Pi joins arguments with spaces; if the intended paths are ambiguous, ask for clarification rather than guessing.

In the task, instruct the reviewer to:

- Read the discovered `vcs-detect` SKILL.md before any VCS operation and follow it, preferring jj in colocated repositories. If no VCS is available for a change scope, report that instead of substituting a scope.
- Use the reviewer's own review methodology. For change scopes, review the specified diff and relevant context without confusing working-tree content with the staged version. For paths, limit findings to the specified paths while reading context as needed.
- Report the actual scope and any empty diff, missing history, ambiguous parent, inaccessible path, or command failure truthfully; do not claim a review was completed when it was not. Do not modify files or run mutating commands.

After spawning, end the turn and await the steering result; do not poll, sleep, or make placeholder calls. Return the complete reviewer findings when received. If spawning or the review fails, report the failure and do not present it as a successful review.
