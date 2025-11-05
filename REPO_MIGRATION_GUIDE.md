# Repository Migration Guide: Preserving Stars/Forks While Adding New Content

## 🎯 Goal
- Keep the same GitHub repository (preserve stars, forks, issues)
- Archive current multi-agent system work
- Add new content to main branch

## ✅ Recommended Approach: Archive Branch Strategy

This approach preserves all your work while allowing you to start fresh on main.

### Step 1: Archive Current Work to a Branch

```bash
# 1. Make sure you're on main and everything is committed
git checkout main
git status  # Check for uncommitted changes

# 2. Create an archive branch from current main
git branch archive/v1-multi-agent-system

# 3. Push the archive branch to GitHub
git push origin archive/v1-multi-agent-system

# 4. Add a README to the archive branch documenting what it contains
git checkout archive/v1-multi-agent-system
echo "# Archive: Multi-Agent COBOL Modernization System (v1)
This branch contains the original multi-agent system implementation.
See main branch for current project." > ARCHIVE_README.md
git add ARCHIVE_README.md
git commit -m "docs: Add archive documentation"
git push origin archive/v1-multi-agent-system
```

### Step 2: Update Main Branch with New Content

```bash
# 1. Switch back to main
git checkout main

# 2. Option A: Keep current structure, add new content alongside
# (Recommended if new content is related/compatible)

# 2. Option B: Replace main with new content
# Create a fresh start (BE CAREFUL - this removes current files)
# First, backup everything:
cp -r . ../legacy2modern-backup/

# Then remove everything (except .git)
git rm -r --cached .
git clean -fd

# Now add your new content
# ... add new files ...

# Commit the new content
git add .
git commit -m "feat: New project direction - [describe new content]"
git push origin main
```

## 📋 Alternative Approaches

### Option A: Directory-Based Separation (Safest)

Keep both old and new content in the same repo:

```bash
# On main branch
mkdir -p legacy/v1-multi-agent-system
# Move current code to legacy directory (except .git, README, etc.)
git mv src legacy/v1-multi-agent-system/src
git mv docs legacy/v1-multi-agent-system/docs
# ... move other relevant directories ...

# Add new content at root level
# ... add new files ...

git commit -m "refactor: Archive v1 to legacy/, add new content"
git push origin main
```

### Option B: Submodule Approach

If you want to completely separate:

```bash
# 1. Create a new repository for old content
# (On GitHub, create new repo: legacy2modern-v1)

# 2. Add it as a submodule in current repo
git submodule add https://github.com/astrio-ai/legacy2modern-v1.git legacy/v1

# 3. Commit submodule
git commit -m "chore: Add v1 as submodule"
```

### Option C: Keep Both Active (Recommended for Research)

Structure your repo to support both:

```
legacy2modern/
├── v1-multi-agent-system/     # Current work
│   ├── src/
│   ├── docs/
│   └── README.md
├── v2-new-content/            # New work
│   ├── ...
│   └── README.md
├── README.md                  # Main project overview
└── ...
```

## 🎨 Update README Strategy

### Update Main README to Reflect New Direction

```markdown
# Legacy2Modern

[Brief description of NEW project focus]

## 🗂️ Project Structure

This repository contains multiple versions:

- **[v1: Multi-Agent System](./v1-multi-agent-system/)** - Original COBOL modernization system
- **[v2: New Project](./v2-new-content/)** - [Description of new content]

## 📚 Archive

Previous versions are preserved in:
- `archive/v1-multi-agent-system` branch - Full multi-agent system implementation
```

## ⚠️ Important Considerations

### 1. Preserve Git History
**DO NOT** force-push or rewrite history on main - this can break forks

### 2. Update GitHub Topics/Tags
- Update repository description
- Add/update topics
- Create releases for archived versions

### 3. Communicate Changes
- Update README prominently
- Consider an announcement in GitHub Discussions
- Add a migration notice if breaking changes

### 4. Preserve Links
- If you have documentation linking to specific files, update them
- Consider redirects or symlinks if needed

## 🚀 Recommended Workflow (Safest)

```bash
# 1. Archive current work
git checkout main
git branch archive/v1-multi-agent-system
git push origin archive/v1-multi-agent-system

# 2. Create a new branch for new content
git checkout -b v2-new-content

# 3. Add new content to this branch
# ... work on new content ...

# 4. When ready, merge or replace main
git checkout main
git merge v2-new-content  # OR replace entirely

# 5. Update main README to explain both versions
# 6. Push changes
git push origin main
```

## 📝 Checklist

- [ ] Archive current work to branch
- [ ] Push archive branch to GitHub
- [ ] Decide on approach (archive branch / directory / submodule)
- [ ] Update main README
- [ ] Add migration notice if needed
- [ ] Update repository description on GitHub
- [ ] Test that old content is accessible
- [ ] Notify contributors/users of changes

## 💡 Best Practice

**For research projects where you want to preserve all work:**

1. **Keep archive branch** - Never delete, always reference
2. **Update main README** - Clearly explain what's current vs archived
3. **Use semantic versioning** - v1, v2, etc. in branch names
4. **Document transitions** - Explain why and what changed
5. **Preserve history** - Never force-push to main

