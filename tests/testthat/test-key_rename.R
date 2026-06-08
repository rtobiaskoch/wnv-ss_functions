# Tests for key_rename — focus on the whitespace-insensitive matching that
# fixes the real-world failure: a datasheet header "Collection Site       (Trap
# ID)" (7 spaces) vs a lookup entry "Collection Site (Trap ID)" (1 space).

test_that("matches across cosmetic whitespace differences and renames the real column", {
  df  <- data.frame(check.names = FALSE,
    "Collection Site       (Trap ID)" = c("LV-095", "FC-067"),  # 7 spaces (template)
    "Total"                           = c(10, 20)
  )
  key <- data.frame(
    old = c("Collection Site (Trap ID)", "Total"),               # 1 space (lookup)
    new = c("trap_id", "total"),
    stringsAsFactors = FALSE
  )

  out <- key_rename(df, key)

  expect_true("trap_id" %in% names(out))
  expect_equal(out$trap_id, c("LV-095", "FC-067"))  # values untouched
  expect_equal(out$total, c(10, 20))
})

test_that("drop_extra prunes unmapped columns; default keeps them", {
  df  <- data.frame(check.names = FALSE,
    "Trap ID"  = c("a", "b"),
    "Untracked" = c(1, 2)
  )
  key <- data.frame(old = "Trap ID", new = "trap_id", stringsAsFactors = FALSE)

  keep <- key_rename(df, key, drop_extra = FALSE)
  expect_setequal(names(keep), c("trap_id", "Untracked"))

  drop <- key_rename(df, key, drop_extra = TRUE)
  expect_equal(names(drop), "trap_id")
})

test_that("duplicate / alias lookup rows for the same column do not error (first wins)", {
  df  <- data.frame(check.names = FALSE, "Collection Site (Trap ID)" = "a")
  key <- data.frame(
    old = c("Trap Number", "trap_name", "Collection Site (Trap ID)",
            "Collection Site (Trap ID)"),   # duplicate alias
    new = "trap_id",
    stringsAsFactors = FALSE
  )

  out <- key_rename(df, key, drop_extra = TRUE)
  expect_equal(names(out), "trap_id")
  expect_equal(out$trap_id, "a")
})

test_that("collision (two columns -> one name) is a loud error, not a silent drop", {
  df  <- data.frame(check.names = FALSE, "Trap ID" = "a", "Trap Number" = "b")
  key <- data.frame(
    old = c("Trap ID", "Trap Number"),
    new = c("trap_id", "trap_id"),          # both map to trap_id, both present
    stringsAsFactors = FALSE
  )
  expect_error(key_rename(df, key), "duplicate column name")
})

test_that("missing old/new columns in the lookup errors", {
  df <- data.frame(x = 1)
  expect_error(key_rename(df, data.frame(foo = 1, bar = 2)),
               "must contain columns")
})
