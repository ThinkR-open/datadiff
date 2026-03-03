test_that("add_tolerance_columns works correctly", {
  # Create test data
  cmp <- data.frame(
    value = c(1.1, 2.2, 3.3),
    value__reference = c(1.0, 2.0, 3.0),
    text = c("a", "b", "c"),
    text__reference = c("a", "b", "c")
  )

  col_rules <- list(
    value = list(abs = 0.2, rel = 0.1),
    text = list()
  )

  # Test with NA equal = TRUE
  result <- add_tolerance_columns(cmp, c("value"), col_rules, "__reference", TRUE)

  # Check that tolerance columns were added
  expect_true("value__absdiff" %in% names(result))
  expect_true("value__thresh" %in% names(result))
  expect_true("value__ok" %in% names(result))

  # Check calculations
  expect_equal(result$value__absdiff, c(0.1, 0.2, 0.3))
  expect_equal(result$value__thresh, c(0.3, 0.4, 0.5))  # abs + rel * ref
  expect_equal(result$value__ok, c(TRUE, TRUE, TRUE))  # All within tolerance

  # Test with NA equal = FALSE
  cmp_na <- data.frame(
    value = c(1.1, NA, 3.3),
    value__reference = c(1.0, NA, 3.0)
  )

  result_na_false <- add_tolerance_columns(cmp_na, c("value"), col_rules, "__reference", FALSE)
  expect_false(result_na_false$value__ok[2])  # NA == NA should be FALSE when na_equal = FALSE
  expect_true(result_na_false$value__ok[1])   # Valid comparison should be TRUE
  expect_true(result_na_false$value__ok[3])   # Valid comparison should be TRUE
})

test_that("add_tolerance_columns handles edge cases", {
  # Empty tolerance columns
  cmp <- data.frame(a = 1, a__reference = 1)
  col_rules <- list(a = list())

  result <- add_tolerance_columns(cmp, character(0), col_rules, "__reference", TRUE)
  expect_equal(nrow(result), 1)  # Should return original data unchanged

  # Test with zero tolerances
  col_rules_zero <- list(a = list(abs = 0, rel = 0))
  cmp_zero <- data.frame(a = c(1.0, 2.0), a__reference = c(1.0, 2.0))

  result_zero <- add_tolerance_columns(cmp_zero, c("a"), col_rules_zero, "__reference", TRUE)
  expect_equal(result_zero$a__absdiff, c(0, 0))
  expect_equal(result_zero$a__thresh, c(0, 0))
  expect_equal(result_zero$a__ok, c(TRUE, TRUE))
})

# =============================================================================
# SECTION: Independence de numeric_abs par rapport à la magnitude des valeurs
#
# La formule du seuil est : thresh = abs_tol + rel_tol * |ref_val|
# Avec rel_tol = 0 (pur abs), thresh = abs_tol (constante), quelle que soit
# la magnitude des valeurs comparées. Ces tests vérifient que la décision
# pass/fail dépend uniquement de la différence absolue et du seuil configuré,
# et jamais de l'ordre de grandeur des valeurs elles-mêmes.
# =============================================================================

# --- 1. Le seuil (thresh) est strictement constant quel que soit ref ---

test_that("numeric_abs: thresh est constant quelle que soit la magnitude de ref (rel=0)", {
  abs_tol <- 5.0
  # Valeurs de référence sur 9 ordres de grandeur
  ref_vals <- c(1e-9, 1e-6, 1e-3, 0, 1, 1e3, 1e6, 1e9)
  cmp <- data.frame(
    value            = ref_vals,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = abs_tol, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  # thresh doit être exactement abs_tol pour chaque ligne, sans exception
  expect_equal(result$value__thresh, rep(abs_tol, length(ref_vals)))
})

test_that("numeric_abs: thresh est constant avec un seuil tres petit (rel=0)", {
  abs_tol <- 1e-10
  ref_vals <- c(0, 1e-6, 1e-3, 1.0, 1e3)
  cmp <- data.frame(
    value            = ref_vals,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = abs_tol, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  expect_equal(result$value__thresh, rep(abs_tol, length(ref_vals)))
})

test_that("numeric_abs: thresh est constant avec un seuil tres grand (rel=0)", {
  abs_tol <- 1e8
  ref_vals <- c(0, 1e-9, 1.0, 1e6, 1e9)
  cmp <- data.frame(
    value            = ref_vals,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = abs_tol, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  expect_equal(result$value__thresh, rep(abs_tol, length(ref_vals)))
})

# --- 2. Meme difference < abs_tol => passe a toutes les echelles ---

test_that("numeric_abs: diff < abs_tol passe quelle que soit la magnitude des valeurs", {
  # diff = 0.5 < abs_tol = 1.0 doit passer pour des valeurs tres grandes comme tres petites.
  # Plages choisies de sorte que diff=0.5 soit representable en virgule flottante double
  # a chaque echelle (ULP << 0.5 pour ref <= 1e9).
  abs_tol  <- 1.0
  diff     <- 0.5
  ref_vals <- c(1e-9, 1e-6, 1e-3, 0, 1, 1e3, 1e6, 1e9)
  cmp <- data.frame(
    value            = ref_vals + diff,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = abs_tol, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  # Toutes les lignes doivent passer : diff (0.5) < abs_tol (1.0)
  expect_true(all(result$value__ok))
  # Le seuil ne doit pas varier
  expect_equal(result$value__thresh, rep(abs_tol, length(ref_vals)))
})

# --- 3. Meme difference > abs_tol => echoue a toutes les echelles ---

test_that("numeric_abs: diff > abs_tol echoue quelle que soit la magnitude des valeurs", {
  abs_tol  <- 1.0
  diff     <- 2.0
  ref_vals <- c(1e-9, 1e-6, 1e-3, 0, 1, 1e3, 1e6, 1e9)
  cmp <- data.frame(
    value            = ref_vals + diff,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = abs_tol, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  # Toutes les lignes doivent echouer : diff (2.0) > abs_tol (1.0)
  expect_true(all(!result$value__ok))
  expect_equal(result$value__thresh, rep(abs_tol, length(ref_vals)))
})

# --- 4. Cas limite : diff == abs_tol exactement => passe (condition <=) ---

test_that("numeric_abs: diff exactement egal au seuil passe (<=) a toutes les echelles", {
  abs_tol  <- 2.5
  ref_vals <- c(1e-9, 1e-3, 0, 1.0, 1e3, 1e6, 1e9)
  cmp <- data.frame(
    value            = ref_vals + abs_tol,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = abs_tol, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  # diff == abs_tol => condition <= satisfaite => TRUE
  expect_true(all(result$value__ok))
  expect_equal(result$value__absdiff, rep(abs_tol, length(ref_vals)),
               tolerance = 1e-9)
})

# --- 5. Cas limite : diff legerement au-dessus du seuil => echoue ---

test_that("numeric_abs: diff infinitesimalement superieure au seuil echoue a toutes les echelles", {
  abs_tol  <- 2.5
  epsilon  <- 1e-9   # representable a toutes les echelles choisies (ref <= 1e6)
  ref_vals <- c(1e-6, 1e-3, 0, 1.0, 1e3, 1e6)
  cmp <- data.frame(
    value            = ref_vals + abs_tol + epsilon,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = abs_tol, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  expect_true(all(!result$value__ok))
})

# --- 6. Seuil abs tres petit (1e-10) : discrimine des differences infimes ---

test_that("numeric_abs tres petit (1e-10): diff < seuil passe, diff > seuil echoue", {
  # On reste sur des ref <= 1e3 pour garantir que 5e-11 est representable (ULP << 5e-11)
  abs_tol  <- 1e-10
  ref_vals <- c(0, 1e-6, 1e-3, 1.0, 1e3)
  rules <- list(value = list(abs = abs_tol, rel = 0))

  # diff = 5e-11 < 1e-10 => doit passer
  cmp_pass <- data.frame(
    value            = ref_vals + 5e-11,
    value__reference = ref_vals
  )
  result_pass <- add_tolerance_columns(cmp_pass, "value", rules, "__reference", TRUE)
  expect_true(all(result_pass$value__ok))
  expect_equal(result_pass$value__thresh, rep(abs_tol, length(ref_vals)))

  # diff = 5e-9 > 1e-10 => doit echouer
  cmp_fail <- data.frame(
    value            = ref_vals + 5e-9,
    value__reference = ref_vals
  )
  result_fail <- add_tolerance_columns(cmp_fail, "value", rules, "__reference", TRUE)
  expect_true(all(!result_fail$value__ok))
})

# --- 7. Seuil abs tres grand (1e8) : de grandes differences passent partout ---

test_that("numeric_abs tres grand (1e8): diff de 1e7 passe a toutes les echelles", {
  abs_tol  <- 1e8
  diff     <- 1e7   # << abs_tol
  ref_vals <- c(0, 1e-9, 1.0, 1e6, 1e9)
  cmp <- data.frame(
    value            = ref_vals + diff,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = abs_tol, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  expect_true(all(result$value__ok))
  expect_equal(result$value__thresh, rep(abs_tol, length(ref_vals)))
})

test_that("numeric_abs tres grand (1e8): diff de 1e9 echoue quelle que soit l'echelle", {
  abs_tol  <- 1e8
  diff     <- 1e9   # > abs_tol
  ref_vals <- c(0, 1e-9, 1.0, 1e6)
  cmp <- data.frame(
    value            = ref_vals + diff,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = abs_tol, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  expect_true(all(!result$value__ok))
})

# --- 8. Symetrie : differences negatives (cand < ref) traitees identiquement ---

test_that("numeric_abs: les differences negatives sont traitees symetriquement (abs)", {
  abs_tol  <- 1.0
  ref_vals <- c(1e-6, 1.0, 1e6, 1e9)
  rules <- list(value = list(abs = abs_tol, rel = 0))

  # cand < ref : diff negative de 0.5 => |diff| = 0.5 < 1.0 => passe
  cmp_pass <- data.frame(
    value            = ref_vals - 0.5,
    value__reference = ref_vals
  )
  result_pass <- add_tolerance_columns(cmp_pass, "value", rules, "__reference", TRUE)
  expect_true(all(result_pass$value__ok))

  # cand < ref : diff negative de 2.0 => |diff| = 2.0 > 1.0 => echoue
  cmp_fail <- data.frame(
    value            = ref_vals - 2.0,
    value__reference = ref_vals
  )
  result_fail <- add_tolerance_columns(cmp_fail, "value", rules, "__reference", TRUE)
  expect_true(all(!result_fail$value__ok))
})

# --- 9. Plusieurs colonnes : chaque seuil abs est fixe et independant ---

test_that("numeric_abs: plusieurs colonnes avec seuils differents, chacun independant de l'echelle", {
  # col1 : abs=0.5,  ref ~ 1e9  (grande valeur)
  # col2 : abs=100,  ref ~ 1e-9 (petite valeur)
  # col3 : abs=1e-6, ref ~ 1e3  (valeur normale)
  # Dans les trois cas, la difference est en-dessous du seuil.
  cmp <- data.frame(
    col1            = c(1e9 + 0.3,   1e9 - 0.3),
    col1__reference = c(1e9,         1e9),
    col2            = c(1e-9 + 99,   1e-9 - 99),
    col2__reference = c(1e-9,        1e-9),
    col3            = c(1e3 + 5e-7,  1e3 - 5e-7),
    col3__reference = c(1e3,         1e3)
  )
  rules <- list(
    col1 = list(abs = 0.5,  rel = 0),
    col2 = list(abs = 100,  rel = 0),
    col3 = list(abs = 1e-6, rel = 0)
  )

  result <- add_tolerance_columns(
    cmp, c("col1", "col2", "col3"), rules, "__reference", TRUE
  )

  # Seuils fixes, independants de la magnitude
  expect_equal(result$col1__thresh, c(0.5,  0.5))
  expect_equal(result$col2__thresh, c(100,  100))
  expect_equal(result$col3__thresh, c(1e-6, 1e-6))

  # Toutes les comparaisons passent
  expect_true(all(result$col1__ok))
  expect_true(all(result$col2__ok))
  expect_true(all(result$col3__ok))
})

test_that("numeric_abs: seuils differents par colonne, certaines passent, d'autres echouent", {
  # col_strict : abs=0.1  => diff=0.5 echoue
  # col_large  : abs=1.0  => diff=0.5 passe
  cmp <- data.frame(
    col_strict            = c(1e9 + 0.5, 1e-9 + 0.5),
    col_strict__reference = c(1e9,       1e-9),
    col_large             = c(1e9 + 0.5, 1e-9 + 0.5),
    col_large__reference  = c(1e9,       1e-9)
  )
  rules <- list(
    col_strict = list(abs = 0.1, rel = 0),
    col_large  = list(abs = 1.0, rel = 0)
  )

  result <- add_tolerance_columns(
    cmp, c("col_strict", "col_large"), rules, "__reference", TRUE
  )

  expect_true(all(!result$col_strict__ok))  # 0.5 > 0.1 => echoue
  expect_true(all(result$col_large__ok))    # 0.5 < 1.0 => passe
})

# --- 10. Tests d'integration via compare_datasets_from_yaml (pur abs) ---

test_that("integration numeric_abs: meme diff passe pour grandes et petites valeurs", {
  # diff = 0.5 < abs_tol = 1.0, quelle que soit l'echelle de la valeur
  abs_tol <- 1.0
  ref <- data.frame(
    id    = 1:4,
    value = c(1e-9, 1.0, 1e6, 1e9)
  )
  cand <- data.frame(
    id    = 1:4,
    value = c(1e-9 + 0.5, 1.5, 1e6 + 0.5, 1e9 + 0.5)
  )

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = abs_tol, numeric_rel = 0)

  result <- compare_datasets_from_yaml(ref, cand, key = "id",
                                       path = template_path)

  expect_true(result$all_passed)
  unlink(template_path)
})

test_that("integration numeric_abs: meme diff echoue pour grandes et petites valeurs", {
  # diff = 2.0 > abs_tol = 1.0, quelle que soit l'echelle de la valeur
  abs_tol <- 1.0
  ref <- data.frame(
    id    = 1:4,
    value = c(1e-9, 1.0, 1e6, 1e9)
  )
  cand <- data.frame(
    id    = 1:4,
    value = c(1e-9 + 2.0, 3.0, 1e6 + 2.0, 1e9 + 2.0)
  )

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = abs_tol, numeric_rel = 0)

  result <- compare_datasets_from_yaml(ref, cand, key = "id",
                                       path = template_path)

  expect_false(result$all_passed)
  unlink(template_path)
})

test_that("integration numeric_abs: seuil par colonne via by_name, independant de l'echelle", {
  # col_grande : ref ~ 1e9, abs=0.5 => diff=0.3 passe, diff=1.0 echoue
  # col_petite : ref ~ 1e-9, abs=0.5 => meme diff => meme resultat
  ref <- data.frame(
    id         = 1:2,
    col_grande = c(1e9,   1e9),
    col_petite = c(1e-9,  1e-9)
  )
  cand_pass <- data.frame(
    id         = 1:2,
    col_grande = c(1e9 + 0.3,  1e9 + 0.3),
    col_petite = c(1e-9 + 0.3, 1e-9 + 0.3)
  )
  cand_fail <- data.frame(
    id         = 1:2,
    col_grande = c(1e9 + 1.0,  1e9 + 1.0),
    col_petite = c(1e-9 + 1.0, 1e-9 + 1.0)
  )

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = 0.5, numeric_rel = 0)

  result_pass <- compare_datasets_from_yaml(ref, cand_pass, key = "id",
                                            path = template_path)
  result_fail <- compare_datasets_from_yaml(ref, cand_fail, key = "id",
                                            path = template_path)

  # diff=0.3 < abs_tol=0.5 => passe pour les deux colonnes
  expect_true(result_pass$all_passed)
  # diff=1.0 > abs_tol=0.5 => echoue pour les deux colonnes
  expect_false(result_fail$all_passed)

  unlink(template_path)
})

test_that("integration numeric_abs: distinction pass/fail coherente entre grande et petite valeur", {
  # Ligne 1 (ref grande) et ligne 2 (ref petite) : meme diff => meme verdict
  abs_tol <- 0.5
  ref <- data.frame(
    id    = 1:2,
    value = c(1e9,  1e-9)
  )

  # diff = 0.3 pour les deux lignes => les deux passent
  cand_both_pass <- data.frame(
    id    = 1:2,
    value = c(1e9 + 0.3, 1e-9 + 0.3)
  )

  # diff = 0.7 pour les deux lignes => les deux echouent
  cand_both_fail <- data.frame(
    id    = 1:2,
    value = c(1e9 + 0.7, 1e-9 + 0.7)
  )

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = abs_tol, numeric_rel = 0)

  result_pass <- compare_datasets_from_yaml(ref, cand_both_pass, key = "id",
                                            path = template_path)
  result_fail <- compare_datasets_from_yaml(ref, cand_both_fail, key = "id",
                                            path = template_path)

  expect_true(result_pass$all_passed)
  expect_false(result_fail$all_passed)

  unlink(template_path)
})

# =============================================================================
# SECTION: Differences realistes contre des seuils fins
#
# numeric_abs = 1e-9 est la valeur PAR DEFAUT de write_rules_template.
# Ce seuil correspond a une quasi-egalite exacte. Toute difference "visible"
# dans les donnees (ex. 0.000565) doit IMPERATIVEMENT etre detectee comme
# erreur, quelle que soit la magnitude de la valeur controlee.
# =============================================================================

test_that("seuil 1e-9 (defaut): absdiff=0.000565 est detecte comme erreur", {
  # 0.000565 >> 1e-9 : doit echouer sans ambiguite
  cmp <- data.frame(
    value            = 0.000565,
    value__reference = 0.0
  )
  rules <- list(value = list(abs = 1e-9, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  expect_equal(result$value__absdiff, 0.000565)
  expect_equal(result$value__thresh,  1e-9)
  expect_false(result$value__ok)
})

test_that("seuil 1e-9 (defaut): absdiff=0.000565 detecte sur valeurs grandes et petites", {
  # La difference 0.000565 reste largement > 1e-9 quelle que soit la base
  ref_vals <- c(1e-6, 0.1, 1.0, 1e3, 1e6)
  cmp <- data.frame(
    value            = ref_vals + 0.000565,
    value__reference = ref_vals
  )
  rules <- list(value = list(abs = 1e-9, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  # Seuil constant a 1e-9
  expect_equal(result$value__thresh, rep(1e-9, length(ref_vals)))
  # Toutes les differences sont detectees
  expect_true(all(!result$value__ok))
})

test_that("seuil 1e-9 (defaut): seule une difference < 1e-9 passe", {
  # 5e-10 < 1e-9 => passe ; 0.000565 > 1e-9 => echoue
  cmp <- data.frame(
    value            = c(1.0 + 5e-10,   1.0 + 0.000565),
    value__reference = c(1.0,            1.0)
  )
  rules <- list(value = list(abs = 1e-9, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)

  expect_true(result$value__ok[1])   # 5e-10 < 1e-9 => OK
  expect_false(result$value__ok[2])  # 0.000565 > 1e-9 => KO
})

test_that("integration seuil 1e-9 (defaut): absdiff=0.000565 detecte via compare_datasets_from_yaml", {
  ref <- data.frame(id = 1L, value = 1.0)
  # La difference 0.000565 est caracteristique d'une erreur de calcul ou
  # d'arrondi non negligeable — elle doit etre detectee avec le seuil par defaut
  cand <- data.frame(id = 1L, value = 1.000565)

  template_path <- tempfile(fileext = ".yaml")
  # numeric_abs = 1e-9 est la valeur par defaut de write_rules_template
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = 1e-9, numeric_rel = 0)

  result <- compare_datasets_from_yaml(ref, cand, key = "id",
                                       path = template_path)

  expect_false(result$all_passed)
  unlink(template_path)
})

test_that("integration seuil 1e-9 (defaut): absdiff=0.000565 detecte pour grandes valeurs", {
  # La detection ne depend pas de l'ordre de grandeur de la valeur de reference
  ref  <- data.frame(id = 1:3, value = c(1e3, 1e6, 1e9))
  cand <- data.frame(id = 1:3, value = c(1e3 + 0.000565,
                                          1e6 + 0.000565,
                                          1e9 + 0.000565))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = 1e-9, numeric_rel = 0)

  result <- compare_datasets_from_yaml(ref, cand, key = "id",
                                       path = template_path)

  expect_false(result$all_passed)
  unlink(template_path)
})

test_that("comparaison des seuils: 0.000565 passe ou echoue selon le seuil configure", {
  diff <- 0.000565
  cmp <- data.frame(
    value            = 1.0 + diff,
    value__reference = 1.0
  )

  seuils_qui_echouent <- c(1e-9, 1e-6, 1e-4, 0.0005)
  seuils_qui_passent  <- c(0.001, 0.01, 0.1,  1.0)

  for (s in seuils_qui_echouent) {
    rules  <- list(value = list(abs = s, rel = 0))
    result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)
    expect_false(result$value__ok,
                 label = paste0("diff=0.000565 doit echouer avec abs=", s))
  }

  for (s in seuils_qui_passent) {
    rules  <- list(value = list(abs = s, rel = 0))
    result <- add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)
    expect_true(result$value__ok,
                label = paste0("diff=0.000565 doit passer avec abs=", s))
  }
})
