# Code Review Checklist

## Areas to Review and Improve

### 1. Type Detection (`detect_type.R`)
- [ ] Handle edge cases: all NA, single value, constant values
- [ ] Date/time handling - is it appropriate?
- [ ] Character variable detection - threshold for categorical vs free text
- [ ] Factor handling - what about ordered factors?
- [ ] Numeric variables with few unique values - when should they be binary vs continuous?

### 2. Summary Functions (`summarize_variable.R`)
- [ ] Continuous variables: handle cases where spread_fun might fail (e.g., IQR with < 4 values)
- [ ] Binary variables: ensure consistent handling of factors vs logical vs 0/1
- [ ] Categorical variables: handle missing levels, ensure all levels are shown
- [ ] Error handling for custom center_fun/spread_fun that might fail
- [ ] Handle cases where center_fun or spread_fun return NA

### 3. SMD Calculations (`summarize_variable.R`)
- [ ] Verify SMD formula for continuous variables (pooled SD)
- [ ] Verify SMD formula for categorical variables
- [ ] Handle edge cases: zero variance, all same value in one group
- [ ] What if group variable has more than 2 levels? (currently errors)

### 4. Multi-Column Tables (`specify_table1_multi.R`)
- [ ] Alignment logic - ensure it works with categorical variables that have different numbers of levels
- [ ] Empty subgroup handling - test all three options
- [ ] Grouping variable closure issue - verify it works correctly
- [ ] Performance with many subgroups
- [ ] Column naming when combining

### 5. Formatting (`fmt.R`)
- [ ] Handle very large numbers
- [ ] Handle very small numbers (scientific notation?)
- [ ] Handle Inf and -Inf
- [ ] Handle NaN

### 6. API Design
- [ ] Is the API intuitive?
- [ ] Are parameter names clear?
- [ ] Should we add more convenience functions?
- [ ] Error messages - are they helpful?

### 7. Edge Cases to Test
- [ ] Empty data frame
- [ ] Single row data
- [ ] All NA variables
- [ ] Variables with only one unique value
- [ ] Very large datasets
- [ ] Variables with special names (spaces, special characters)
- [ ] Factor variables with unused levels
- [ ] Date variables
- [ ] Character variables that look numeric

### 8. Documentation
- [ ] Are examples correct and runnable?
- [ ] Are parameter descriptions clear?
- [ ] Missing documentation for internal functions?
- [ ] Should we add vignettes?

### 9. Performance
- [ ] Any obvious inefficiencies?
- [ ] Should we add memoization for type detection?
- [ ] Large number of variables - any bottlenecks?

### 10. Testing
- [ ] Need to add comprehensive test suite
- [ ] Test all edge cases
- [ ] Test error conditions
- [ ] Test with real-world data
