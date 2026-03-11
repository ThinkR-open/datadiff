test_that("compare_datasets_from_yaml uses key parameter over YAML rules", {
  # Create test data with multiple potential key columns
  ref <- data.frame(
    id = 1:3,
    customer_id = c("A", "B", "C"),
    value = c(10.0, 20.0, 30.0),
    name = c("Alice", "Bob", "Charlie")
  )

  # Candidate data - same values but different order to test key-based sorting
  cand <- data.frame(
    id = c(2, 1, 3),
    customer_id = c("B", "A", "C"),
    value = c(20.1, 10.1, 30.1),
    name = c("Bob", "Alice", "Charlie")
  )

  # YAML with customer_id as key
  yaml_content <- '
version: 1
defaults:
  na_equal: yes
  keys: ["customer_id"]
by_type:
  numeric:
    abs: 0.5
'

  writeLines(yaml_content, 'test_key_param.yaml')

  # Test 1: Without key parameter, should use YAML key (customer_id)
  result_yaml <- compare_datasets_from_yaml(ref, cand, path = 'test_key_param.yaml')
  expect_true(result_yaml$all_passed)

  # Test 2: With key parameter "id", should override YAML and use "id"
  result_param <- compare_datasets_from_yaml(ref, cand, key = "id", path = 'test_key_param.yaml')
  expect_true(result_param$all_passed)

  # Test 3: With multiple keys parameter, should work
  result_multi <- compare_datasets_from_yaml(ref, cand, key = c("id", "customer_id"), path = 'test_key_param.yaml')
  expect_true(result_multi$all_passed)

  # Clean up
  unlink('test_key_param.yaml')
})

test_that("compare_datasets_from_yaml key parameter handles NULL and missing keys", {
  ref <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  cand <- data.frame(id = 1:3, value = c(10.1, 20.1, 30.1))

  # YAML without keys
  yaml_content <- '
version: 1
defaults:
  na_equal: yes
by_type:
  numeric:
    abs: 0.5
'

  writeLines(yaml_content, 'test_key_null.yaml')

  # Test 1: NULL key parameter with no keys in YAML should work (no key comparison)
  result_null <- compare_datasets_from_yaml(ref, cand, key = NULL, path = 'test_key_null.yaml')
  expect_true(result_null$all_passed)

  # Test 2: No key parameter (default NULL) with no keys in YAML
  result_default <- compare_datasets_from_yaml(ref, cand, path = 'test_key_null.yaml')
  expect_true(result_default$all_passed)

  # Clean up
  unlink('test_key_null.yaml')
})

test_that("compare_datasets_from_yaml key parameter validation works", {
  ref <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  cand <- data.frame(id = 1:3, value = c(10.1, 20.1, 30.1))

  yaml_content <- '
version: 1
defaults:
  na_equal: yes
by_type:
  numeric:
    abs: 0.5
'

  writeLines(yaml_content, 'test_key_validation.yaml')

  # Test: Invalid key should return an empty output with all_passed FALSE

  res <- compare_datasets_from_yaml(ref, cand, key = "nonexistent", path = 'test_key_validation.yaml')

  expect_false(res$all_passed)
  expect_null(res$agent)
  expect_null(res$response)
  # Clean up
  unlink('test_key_validation.yaml')
})

test_that("key parameter takes precedence over YAML in edge cases", {
  # Create data where YAML key would cause issues but parameter key works
  ref <- data.frame(
    id = 1:3,
    category = c("X", "Y", "Z"),
    value = c(1.0, 2.0, 3.0)
  )

  cand <- data.frame(
    id = c(1, 3, 2),  # Different order
    category = c("X", "Z", "Y"),  # Different order
    value = c(1.01, 3.01, 2.01)
  )

  # YAML specifies category as key (which would sort differently)
  yaml_content <- '
version: 1
defaults:
  na_equal: yes
  keys: ["category"]
by_type:
  numeric:
    abs: 0.1
'

  writeLines(yaml_content, 'test_key_precedence.yaml')

  # Using YAML key (category) would compare X->X, Y->Z, Z->Y which fails
  # But we override with id parameter
  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = 'test_key_precedence.yaml')
  expect_true(result$all_passed)

  # Clean up
  unlink('test_key_precedence.yaml')
  unlink('test_key_precedence.yaml')
})

# --- Duplicate key detection tests ---

test_that("warning is raised when reference data has duplicate keys", {
  # Reference with duplicate key values
  ref <- data.frame(
    id = c(1, 1, 2, 3),
    value = c(10.0, 11.0, 20.0, 30.0)
  )

  cand <- data.frame(
    id = c(1, 2, 3),
    value = c(10.5, 20.5, 30.5)
  )

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id"),
    regexp = "Duplicate keys detected.*data_reference.*1 duplicate key value.*affecting 2 rows",
    ignore.case = TRUE
  )
})

test_that("warning is raised when candidate data has duplicate keys", {
  ref <- data.frame(
    id = c(1, 2, 3),
    value = c(10.0, 20.0, 30.0)
  )

  # Candidate with duplicate key values
  cand <- data.frame(
    id = c(1, 2, 2, 3),
    value = c(10.5, 20.5, 21.0, 30.5)
  )

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id"),
    regexp = "Duplicate keys detected.*data_candidate.*1 duplicate key value.*affecting 2 rows",
    ignore.case = TRUE
  )
})

test_that("warning is raised when both datasets have duplicate keys", {
  ref <- data.frame(
    id = c(1, 1, 2, 3),
    value = c(10.0, 11.0, 20.0, 30.0)
  )

  cand <- data.frame(
    id = c(1, 2, 2, 3),
    value = c(10.5, 20.5, 21.0, 30.5)
  )

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id"),
    regexp = "Duplicate keys detected.*data_reference.*data_candidate",
    ignore.case = TRUE
  )
})

test_that("warning shows multiple duplicate key examples", {
  ref <- data.frame(
    id = c(1, 1, 2, 2, 3, 3, 4),
    value = c(10.0, 11.0, 20.0, 21.0, 30.0, 31.0, 40.0)
  )

  cand <- data.frame(
    id = c(1, 2, 3, 4),
    value = c(10.5, 20.5, 30.5, 40.5)
  )

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id"),
    regexp = "3 duplicate key value.*affecting 6 rows",
    ignore.case = TRUE
  )
})

test_that("warning works with composite keys", {
  ref <- data.frame(
    id1 = c(1, 1, 2, 2),
    id2 = c("A", "A", "B", "C"),
    value = c(10.0, 11.0, 20.0, 30.0)
  )

  cand <- data.frame(
    id1 = c(1, 2, 2),
    id2 = c("A", "B", "C"),
    value = c(10.5, 20.5, 30.5)
  )

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = c("id1", "id2")),
    regexp = "Duplicate keys detected.*key column.*id1.*id2.*data_reference",
    ignore.case = TRUE
  )
})

test_that("no warning when keys are unique", {
  ref <- data.frame(
    id = c(1, 2, 3),
    value = c(10.0, 20.0, 30.0)
  )

  cand <- data.frame(
    id = c(1, 2, 3),
    value = c(10.5, 20.5, 30.5)
  )

  expect_no_warning(
    compare_datasets_from_yaml(ref, cand, key = "id")
  )
})

test_that("compare_datasets_from_yaml uses key parameter over YAML rules", {
  # Create test data with multiple potential key columns
  ref <- data.frame(
    id = 1:3,
    customer_id = c("A", "B", "C"),
    value = c(10.0, 20.0, 30.0),
    name = c("Alice", "Bob", "Charlie")
  )

  # Candidate data - same values but different order to test key-based sorting
  cand <- data.frame(
    id = c(2, 1, 3),
    customer_id = c("B", "A", "C"),
    value = c(20.1, 10.1, 30.1),
    name = c("Bob", "Alice", "Charlie")
  )

  # YAML with customer_id as key
  yaml_content <- '
version: 1
defaults:
  na_equal: yes
  keys: ["customer_id"]
by_type:
  numeric:
    abs: 0.5
'

  writeLines(yaml_content, 'test_key_param.yaml')

  # Test 1: Without key parameter, should use YAML key (customer_id)
  result_yaml <- compare_datasets_from_yaml(ref, cand, path = 'test_key_param.yaml')
  expect_true(result_yaml$all_passed)

  # Test 2: With key parameter "id", should override YAML and use "id"
  result_param <- compare_datasets_from_yaml(ref, cand, key = "id", path = 'test_key_param.yaml')
  expect_true(result_param$all_passed)

  # Test 3: With multiple keys parameter, should work
  result_multi <- compare_datasets_from_yaml(ref, cand, key = c("id", "customer_id"), path = 'test_key_param.yaml')
  expect_true(result_multi$all_passed)

  # Clean up
  unlink('test_key_param.yaml')
})


test_that("warning truncates to 3 examples when more than 3 duplicate key values in reference", {
  # 4 unique duplicate key values -> takes the else branch (n_dup_keys > 3)
  ref <- data.frame(
    id    = c(1, 1, 2, 2, 3, 3, 4, 4, 5),
    value = c(10, 11, 20, 21, 30, 31, 40, 41, 50)
  )
  cand <- data.frame(
    id    = c(1, 2, 3, 4, 5),
    value = c(10.5, 20.5, 30.5, 40.5, 50.5)
  )

  w <- tryCatch(
    compare_datasets_from_yaml(ref, cand, key = "id"),
    warning = \(w) conditionMessage(w)
  )
  expect_match(w, "\\.\\.\\.")
})

test_that("warning truncates to 3 examples when more than 3 duplicate key values in candidate", {
  ref <- data.frame(
    id    = c(1, 2, 3, 4, 5),
    value = c(10, 20, 30, 40, 50)
  )
  # 4 unique duplicate key values in candidate
  cand <- data.frame(
    id    = c(1, 1, 2, 2, 3, 3, 4, 4, 5),
    value = c(10, 11, 20, 21, 30, 31, 40, 41, 50)
  )

  w <- tryCatch(
    compare_datasets_from_yaml(ref, cand, key = "id"),
    warning = \(w) conditionMessage(w)
  )
  expect_match(w, "\\.\\.\\.")
})
