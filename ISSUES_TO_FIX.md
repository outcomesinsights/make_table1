# Issues Identified for Fixing

## Critical Issues

### 1. SMD Division by Zero
**Location**: `R/summarize_variable.R`, line 221-222
**Issue**: If both groups have zero variance, `pooled_sd` will be 0, causing division by zero
**Fix**: Check for zero variance and handle appropriately (return NA or 0)

### 2. IQR with Small Samples
**Location**: `R/summarize_variable.R`, line 61
**Issue**: `IQR()` requires at least 4 values, will fail with fewer
**Fix**: Add validation or use alternative spread function for small samples

### 3. Missing Error Handling for Custom Functions
**Location**: `R/summarize_variable.R`, lines 60-61, 149-150, 215-218
**Issue**: If `center_fun` or `spread_fun` fail or return unexpected values, no error handling
**Fix**: Add tryCatch and validate return values

### 4. fmt() Doesn't Handle Inf/NaN
**Location**: `R/fmt.R`
**Issue**: `format()` with Inf/NaN might produce unexpected results
**Fix**: Check for Inf/NaN and handle explicitly

## Medium Priority Issues

### 5. Categorical SMD Only Uses First Level
**Location**: `R/summarize_variable.R`, line 261
**Issue**: SMD for categorical variables only compares first level, not all levels
**Fix**: Consider calculating SMD for each level or using a different approach

### 6. Binary Variable Detection Edge Cases
**Location**: `R/detect_type.R`
**Issue**: May not correctly identify binary variables in all cases (e.g., 0/2 instead of 0/1)
**Fix**: Improve detection logic

### 7. Empty Subgroup Handling in Multi-Column
**Location**: `R/make_table1_multi.R`
**Issue**: When creating empty structures, we copy reference table which might modify original
**Fix**: Use proper copying (avoid reference issues)

## Low Priority / Enhancements

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
