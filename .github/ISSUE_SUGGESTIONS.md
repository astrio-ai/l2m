# Quick Issue Creation Guide

## Copy-paste ready issues:

### Issue 1: Add More COBOL Sample Files
**Title**: `Add more diverse COBOL sample files to data/samples/`
**Labels**: `good first issue`, `enhancement`
**Body**:
```
Add more COBOL sample files to help users test the system. Include examples of:
- File I/O operations
- Complex data structures  
- Nested PERFORM loops
- CALL statements
- COPY statements

Include expected Python output for each sample.
```

### Issue 2: Improve Error Messages
**Title**: `Make error messages more user-friendly and actionable`
**Labels**: `good first issue`, `enhancement`, `ux`
**Body**:
```
Review and improve error messages throughout the codebase to be more user-friendly and include actionable suggestions for resolution.
```

### Issue 3: Add Batch Processing Support
**Title**: `Add support for processing multiple COBOL files at once`
**Labels**: `enhancement`, `feature`
**Body**:
```
Currently processes one file at a time. Add support for batch processing multiple files with parallel or sequential options.
```

### Issue 4: Add Progress Tracking
**Title**: `Add progress tracking and callbacks for long-running tasks`
**Labels**: `enhancement`, `feature`
**Body**:
```
Add progress tracking and callback system so users can monitor modernization progress. Include CLI progress bar option.
```

### Issue 5: Add Retry Logic for API Calls
**Title**: `Add retry logic with exponential backoff for OpenAI API calls`
**Labels**: `enhancement`, `reliability`
**Body**:
```
Handle transient API failures with retry logic, especially for rate limits (429 errors).
```

### Issue 6: Add Code Diff Visualization
**Title**: `Add diff visualization between COBOL and generated Python`
**Labels**: `enhancement`, `feature`, `ux`
**Body**:
```
Add ability to visualize differences between original COBOL and generated Python code using difflib or similar.
```

### Issue 7: Improve Test Coverage
**Title**: `Increase test coverage to 80%+`
**Labels**: `enhancement`, `testing`
**Body**:
```
Focus on edge cases and error handling paths. Current coverage: [run pytest --cov to check]
```

### Issue 8: Add Docker Support
**Title**: `Add Docker support for easy deployment`
**Labels**: `enhancement`, `devops`
**Body**:
```
Add Dockerfile and docker-compose.yml for consistent development and deployment environments.
```
