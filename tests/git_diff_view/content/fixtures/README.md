# Git Diff View Test Fixtures

## The test moves this data!

1. Copy `head/` to temp dir
2. Init git repo, commit all
3. Remove all .skg files
4. Copy `worktree/` into temp dir
5. Run diff view test

## Structure

## head/ = git HEAD
Initial state (committed to git)
```
Node 1 contains [11, 12]
  Node 11 contains [gets-removed]
  Node 12 contains [moves]
```

## worktree/ = uncommitted changes

```
Node 1 contains [11, 12]
  Node 11 contains [moves]    <- moved here from 12
  Node 12 contains []         <- empty now
Node new exists               <- new file
(gets-removed deleted)
```

## Expected Output

In the git diff view, after replacing the .skg contents of head
with those from worktree, we should see:

```
* 1
** 11
*** (skg .. (diff removed)) gets-removed     <- deleted from disk
*** (skg .. (diff new-here)) moves           <- moved TO here
** 12
*** (skg .. (diff removed-here)) moves       <- phantom: was here in git
* (skg .. (diff new)) new                    <- new file
```
