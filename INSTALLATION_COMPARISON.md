# Installation Options Comparison

## Option 1: `devtools::install()` (Full Installation)

### How to Use
```r
library(devtools)
install("/Users/mark/Documents/rprojects/package_development/table1")
library(table1)
```

### Benefits
✅ **Permanent installation** - Package is installed in your R library and persists across R sessions  
✅ **Available everywhere** - Can use `library(table1)` from any R session or script  
✅ **Standard R package** - Behaves like any other installed package  
✅ **Documentation accessible** - Can use `?specify_table1` to view help pages  
✅ **Works in R Markdown** - Can be used in R Markdown documents and Shiny apps  
✅ **Version control** - R tracks the installed version  
✅ **Can share** - Others can install the same way if they have the source  

### Limitations
❌ **Requires reinstall for changes** - Must reinstall after code changes  
❌ **Slower iteration** - Takes time to install each time you make changes  
❌ **Requires write permissions** - Needs to write to R library directory  
❌ **Can't easily switch versions** - Harder to test different versions side-by-side  

### Best For
- **Production use** - When you want to use the package in real projects
- **Stable code** - When the code is relatively stable
- **Sharing with others** - When others need to install it
- **R Markdown/Shiny** - When using in documents or apps

---

## Option 2: `devtools::load_all()` (Development Mode)

### How to Use
```r
library(devtools)
load_all("/Users/mark/Documents/rprojects/package_development/table1")
# Now you can use the functions directly
```

### Benefits
✅ **Fast iteration** - Instant reload after code changes (just run `load_all()` again)  
✅ **No installation needed** - Doesn't modify your R library  
✅ **Easy testing** - Perfect for development and debugging  
✅ **See changes immediately** - Code changes take effect immediately  
✅ **No write permissions needed** - Doesn't need to write to system directories  
✅ **Multiple versions** - Can load different versions in different R sessions  
✅ **Source code visible** - Can easily inspect and modify source code  

### Limitations
❌ **Session-specific** - Must run `load_all()` in each new R session  
❌ **Not persistent** - Package not available after restarting R  
❌ **Limited documentation** - Help pages may not work as well  
❌ **Can't use in R Markdown easily** - May have issues in R Markdown documents  
❌ **Not for production** - Not suitable for stable production code  
❌ **Requires devtools** - Must have devtools loaded  

### Best For
- **Active development** - When you're actively writing/changing code
- **Testing changes** - When testing new features or bug fixes
- **Quick prototyping** - When experimenting with functionality
- **Debugging** - When you need to step through code

---

## Option 3: Installation Script (`install.R`)

### How to Use
```r
source("/Users/mark/Documents/rprojects/package_development/table1/install.R")
```

### Benefits
✅ **Convenient** - Single command to install  
✅ **Automated** - Handles devtools installation check  
✅ **Clear feedback** - Provides installation status messages  
✅ **Full installation** - Actually installs the package (uses `devtools::install()` under the hood)  
✅ **Same as Option 1** - Gets all benefits of full installation  

### Limitations
❌ **Same as Option 1** - All limitations of full installation apply  
❌ **Requires script** - Must have the install.R file  
❌ **Less flexible** - Harder to customize installation options  

### Best For
- **First-time setup** - When installing for the first time
- **Non-technical users** - When you want a simple one-command install
- **Automation** - When you want to script the installation process

---

## Comparison Table

| Feature | `install()` | `load_all()` | `install.R` |
|---------|-------------|--------------|--------------|
| **Persistence** | ✅ Yes | ❌ No | ✅ Yes |
| **Speed** | ⚠️ Slow | ✅ Fast | ⚠️ Slow |
| **Iteration** | ❌ Must reinstall | ✅ Instant reload | ❌ Must reinstall |
| **Documentation** | ✅ Full | ⚠️ Limited | ✅ Full |
| **R Markdown** | ✅ Works | ⚠️ Issues | ✅ Works |
| **Production** | ✅ Yes | ❌ No | ✅ Yes |
| **Development** | ⚠️ Slow | ✅ Perfect | ⚠️ Slow |
| **Write Permissions** | ✅ Needed | ❌ Not needed | ✅ Needed |
| **Multiple Versions** | ❌ Hard | ✅ Easy | ❌ Hard |

---

## Recommended Workflow

### For Development
1. **Start with `load_all()`** - Use during active development
   ```r
   library(devtools)
   load_all("/path/to/table1")
   # Make changes to code
   load_all("/path/to/table1")  # Reload instantly
   ```

2. **Test with `install()`** - When code is stable, install to test as a real package
   ```r
   install("/path/to/table1")
   library(table1)
   ```

### For Production Use
1. **Use `install()` or `install.R`** - Install once, use everywhere
   ```r
   devtools::install("/path/to/table1")
   # or
   source("install.R")
   ```

2. **Then use normally** - Just `library(table1)` in your scripts

---

## Quick Decision Guide

**Use `load_all()` if:**
- You're actively developing/changing code
- You want to test changes quickly
- You're debugging
- You don't need it in R Markdown

**Use `install()` if:**
- Code is stable
- You want to use it in production
- You need it in R Markdown/Shiny
- You want it available in all R sessions

**Use `install.R` if:**
- You want a simple one-command install
- You're setting up for the first time
- You want automated installation

---

## Example Workflow

```r
# Development phase
library(devtools)
load_all("/path/to/table1")  # Fast iteration

# Make code changes...

load_all("/path/to/table1")  # Reload instantly
# Test your changes

# When ready for production
install("/path/to/table1")   # Install properly
library(table1)              # Use normally
```
