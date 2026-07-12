# RFC : surface d'API datadiff 0.5

Statut : proposition a discuter (issue #33). Rien ici n'est implemente ;
les quick wins non cassants identifies sont listes en fin de document avec
leur issue de rattachement.

## Constat

La fonction centrale `compare_datasets_from_yaml()` a grossi par accretion :
17 parametres a plat, un nom qui ne reflete plus l'usage (le YAML est
optionnel depuis 0.1.5), un retour a 8 champs partiellement redondants, et un
melange de langues (arguments anglais, champ historique `$reponse` en
francais, renomme `$response` en 0.6.0 avec alias deprecie, rapport par
defaut en francais, messages en anglais).

## Proposition

### 1. Nom et alias

```r
compare_datasets(reference, candidate, key = NULL, rules = NULL, ...)
```

- `compare_datasets()` devient le point d'entree documente.
- `compare_datasets_from_yaml()` reste exporte comme alias retrocompatible
  (one-liner qui delegue), documente dans une section "Legacy".
- `reference`/`candidate` remplacent `data_reference`/`data_candidate`
  (les anciens noms restent acceptes par l'alias).

### 2. Objets d'options pour les passe-plats

Cinq parametres sont du pur passe-plat vers `pointblank::interrogate()` et un
n'agit que sur le chemin Arrow. Regroupement :

```r
# NB : lang/locale montres avec les defauts ACTUELS (0.4.x, fr/fr_FR) ;
# la section 5 propose de basculer le defaut 0.5 vers en/en_US
compare_datasets(
  reference, candidate, key = NULL, rules = NULL,
  extract = extract_opts(failed = TRUE, first_n = NULL, sample_n = NULL,
                         sample_frac = NULL, limit = 5000),
  engine  = engine_opts(duckdb_memory_limit = "8GB"),
  lang    = getOption("datadiff.lang", "fr"),
  locale  = getOption("datadiff.locale", "fr_FR")
)
```

- `extract_opts()` / `engine_opts()` sont des constructeurs valides
  (erreurs franches, defauts documentes en un seul endroit).
- La signature centrale descend de 17 a ~8 parametres.

### 3. Retour : classe `datadiff_result`

Etat actuel : `all_passed` existe en 3 exemplaires (`$all_passed`,
`$summary$all_passed`, `pointblank::all_passed($response)`) ; `$agent` n'est
pas interroge (et il est factice sur le fast-path all-pass) sans usage
utilisateur identifie. Depuis 0.6.0, le champ s'appelle `$response` (la
classe `datadiff_result` et la mecanique de depreciation `$`/`[[` proposees
ici existent deja ; `$reponse` reste lisible avec warning).

Cible :

```r
res$passed      # le verdict, une seule fois
res$coverage    # inchange
res$summary     # inchange (sans all_passed duplique)
res$report      # l'agent interroge (ex-$response), print() paresseux inchange
                # NB : renommer response -> report imposerait une 2e migration
                # aux utilisateurs ; a arbitrer sur l'issue #33
res$applied_rules, res$missing_in_candidate, res$extra_in_candidate
```

- `$response` (et l'alias historique `$reponse`) et `$all_passed` restent
  presents une version avec un warning de depreciation a l'acces (la methode
  `$.datadiff_result` de 0.6.0 fournit deja la mecanique).
- `$agent` est retire du contrat documente (garde interne si necessaire).

### 4. warn_at / stop_at

Les deux valent 1e-14 : WARN et STOP se declenchent toujours ensemble, le
niveau WARN n'apporte rien. Proposition : un unique `fail_at = 1e-14`
(fraction), les deux niveaux pointblank cales dessus ; `warn_at`/`stop_at`
acceptes par l'alias legacy avec warning si differents.

### 5. Langue

- Deux ecoles : (a) tout anglais par defaut (`lang = "en"`), coherent avec
  les messages ; (b) statu quo francais documente. La 0.4.4 avait annonce (a)
  puis un commit l'a re-bascule sans NEWS (issue #26 : la doc est desormais
  alignee sur le defaut reel "fr").
- Proposition : basculer a `"en"` au moment du passage 0.5 (major-ish), via
  `getOption("datadiff.lang", "en")`, avec entree NEWS Breaking changes.

### 6. Messages

Uniformiser sur le style des bons warnings existants (doublons de cles,
type_mismatch : contexte + consequence + action). En particulier remplacer
`message("key is missing")` par une note explicite unique documentant le mode
positionnel, ou la supprimer (le mode positionnel est un choix legitime).

### 7. write_rules_template()

- 19 parametres au nommage incoherent (`na_equal_default` vs `numeric_abs`) :
  harmoniser en `0.5` (`na_equal`, `numeric_abs`, ...) via l'alias.
- Ne plus ecrire par defaut `rules.yaml` dans le repertoire courant :
  `path` obligatoire ou defaut `tempfile()`.
- `ref_suffix` : detail d'implementation, a retirer de la signature publique.

## Cycle de depreciation

1. 0.5.0 : nouvelle surface + alias retrocompatibles complets, warnings de
   depreciation conditionnels (une fois par session), vignette reecrite sur la
   nouvelle surface, annexe migration.
2. 0.6.0 : warnings inconditionnels.
3. 1.0.0 : retrait des alias.

## Quick wins deja traites ou rattaches a d'autres issues

- Precedence argument > YAML (issue #20, fait en 0.4.10).
- Validation label / key / duckdb_memory_limit (issues #16/#20/#30, fait).
- Contradictions NEWS %||% / lang / "comparaison" (issue #26).
- Parametre mort cols_reference (issue #25).
- equal_mode inerte (issue #27).
