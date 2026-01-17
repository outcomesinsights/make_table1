# YAML Specification for Table 1

## Overview

YAML provides a much more readable and easier-to-write format for specifying variables, labels, subheaders, and function overrides for Table 1.

## Basic YAML Structure

```yaml
Table Title:
  Subheader 1:
    variable1: Label 1
    variable2: Label 2
  Subheader 2:
    variable3: Label 3
```

## Examples

### Example 1: Simple Table

```yaml
Patient Characteristics:
  Demographics:
    age: Age (years)
    sex: Sex
  Treatment:
    treated: Treated
```

**Usage:**
```r
yaml_str <- "
Patient Characteristics:
  Demographics:
    age: Age (years)
    sex: Sex
  Treatment:
    treated: Treated
"

table1 <- specify_table1(data, vars = yaml_str)
```

### Example 2: With Variable-Level Function Overrides

```yaml
Patient Characteristics:
  Demographics:
    age: Age (years)
    sex: Sex
  Treatment:
    treated: Treated
  Outcomes:
    score:
      var: score
      label: Score
      center_fun: median
      spread_fun: IQR
```

**Usage:**
```r
yaml_str <- "
Patient Characteristics:
  Demographics:
    age: Age (years)
    sex: Sex
  Treatment:
    treated: Treated
  Outcomes:
    score:
      var: score
      label: Score
      center_fun: median
      spread_fun: IQR
"

table1 <- specify_table1(data, vars = yaml_str)
```

### Example 3: Using YAML File

Create a file `table1_spec.yaml`:

```yaml
Baseline Characteristics:
  Demographics:
    age: Age (years)
    sex: Sex
    race: Race
  Clinical Variables:
    bmi:
      var: bmi
      label: Body Mass Index (kg/m²)
      center_fun: median
      spread_fun: IQR
    sbp:
      var: sbp
      label: Systolic Blood Pressure (mmHg)
      center_fun: mean
      spread_fun: sd
  Comorbidities:
    diabetes: Diabetes
    hypertension: Hypertension
    ckd: Chronic Kidney Disease
```

**Usage:**
```r
table1 <- specify_table1(data, vars = "table1_spec.yaml")
```

### Example 4: Complex Real-World Example

```yaml
Patient Baseline Characteristics:
  Demographics:
    age: Age (years)
    sex: Sex
    race: Race/Ethnicity
    marital_status: Marital Status
  
  Clinical Measurements:
    bmi:
      var: bmi
      label: Body Mass Index (kg/m²)
      center_fun: median
      spread_fun: IQR
    sbp:
      var: sbp
      label: Systolic Blood Pressure (mmHg)
    dbp:
      var: dbp
      label: Diastolic Blood Pressure (mmHg)
    heart_rate:
      var: heart_rate
      label: Heart Rate (bpm)
  
  Laboratory Values:
    creatinine:
      var: creatinine
      label: Creatinine (mg/dL)
      center_fun: median
      spread_fun: IQR
    hemoglobin:
      var: hemoglobin
      label: Hemoglobin (g/dL)
  
  Comorbidities:
    diabetes: Diabetes
    hypertension: Hypertension
    ckd: Chronic Kidney Disease
    copd: COPD
    heart_failure: Heart Failure
  
  Medications:
    ace_inhibitor: ACE Inhibitor
    beta_blocker: Beta Blocker
    statin: Statin
```

## Function Override Syntax

To override `center_fun` and/or `spread_fun` for a specific variable, use the structured format:

```yaml
variable_name:
  var: variable_name
  label: Display Label
  center_fun: median    # Options: mean, median, or custom function name
  spread_fun: IQR       # Options: sd, IQR, or custom function name
```

**Available Functions:**
- `center_fun`: `mean`, `median`, or any function name available in base R or stats
- `spread_fun`: `sd`, `IQR`, or any function name available in base R or stats

## YAML File vs String

### YAML String (Inline)
```r
yaml_str <- "
Patient Characteristics:
  Demographics:
    age: Age (years)
"
table1 <- specify_table1(data, vars = yaml_str)
```

### YAML File (Recommended for Complex Tables)
```r
# Save to file
writeLines(yaml_str, "table1_spec.yaml")

# Use file
table1 <- specify_table1(data, vars = "table1_spec.yaml")
```

## Benefits of YAML

1. **Readability** - Much easier to read than nested R lists
2. **Editability** - Easy to edit in any text editor
3. **Version Control** - YAML files work well with git
4. **Reusability** - Save specifications for reuse across analyses
5. **Collaboration** - Non-R users can edit YAML files

## YAML Syntax Tips

1. **Indentation matters** - Use spaces (not tabs) and be consistent
2. **Quotes optional** - For simple strings, quotes are optional
3. **Colons** - Required after keys
4. **Lists** - Use `-` for lists (not needed for our use case)
5. **Comments** - Use `#` for comments

## Comparison

### YAML (Recommended)
```yaml
Patient Characteristics:
  Demographics:
    age: Age (years)
    sex: Sex
```

### Nested R List
```r
list(
  "Patient Characteristics" = list(
    "Demographics" = list(
      age = "Age (years)",
      sex = "Sex"
    )
  )
)
```

### Data Frame
```r
data.frame(
  var = c("age", "sex"),
  label = c("Age (years)", "Sex"),
  subheader = c("Demographics", "Demographics")
)
```

YAML is clearly the most readable and easiest to write!
