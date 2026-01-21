# Issues Identified for Fixing

## Critical Issues

### 1. Missing Error Handling for Custom Functions
**Location**: `R/summarize_variable.R`, lines ~67-72 and ~202-210
**Issue**: If `center_fun` or `spread_fun` fail or return unexpected values, no error handling
**Fix**: Add tryCatch and validate return values

### 2. fmt() Doesn't Handle Inf/NaN
**Location**: `R/fmt.R`
**Issue**: `format()` with Inf/NaN might produce unexpected results
**Fix**: Check for Inf/NaN and handle explicitly

### 3. "other" Type Falls Through to Continuous Summary
**Location**: `R/summarize_variable.R` (fallback branch for unknown types)
**Issue**: Variables classified as `other` are summarized with numeric functions, which can produce warnings and `NA` for character data
**Fix**: Add explicit handling for `other` (return NA with warning, or treat as categorical with a controlled limit)

## Medium Priority Issues

### 4. IQR with Small Samples
**Location**: `R/summarize_variable.R`
**Issue**: `IQR()` returns `NA` and can warn when sample size is small
**Fix**: Add validation or fallback behavior for small samples

### 5. Binary Variable Detection Edge Cases
**Location**: `R/detect_type.R`
**Issue**: May not correctly identify binary variables in all cases (e.g., 0/2 instead of 0/1)
**Fix**: Improve detection logic

### 6. Subgroup Filter Validation Is Soft-Fail
**Location**: `R/specify_table1.R` (`.create_multi_column_table1`)
**Issue**: Invalid subgroup filters are skipped with warnings, which can silently drop columns
**Fix**: Offer stricter option (error on invalid filter) or better summary in the result

## Low Priority / Enhancements

### 7. R CMD check NOTE: .github Included in Build
**Location**: `.Rbuildignore`
**Issue**: `R CMD check` reports `.github` as a hidden directory in the package tarball
**Fix**: Add `^\\.github$` to `.Rbuildignore`

### 8. Performance with Many Variables
**Location**: Multiple
**Issue**: Type detection called for each variable - could be optimized
**Enhancement**: Consider caching or vectorized approach

### 9. Better Error Messages
**Location**: Multiple
**Issue**: Some error messages could be more helpful
**Enhancement**: Add context to error messages

### 10. Documentation Examples
**Location**: Function documentation
**Issue**: Some examples might not be fully runnable
**Enhancement**: Test all examples, add more edge case examples
