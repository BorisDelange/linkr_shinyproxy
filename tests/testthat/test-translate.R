library(cdwtools)

words <- dplyr::tribble(~language, ~reference_word, ~translated_word,
                        "FR", "Settings", "Paramètres",
                        "IT", "Settings", "Impostazioni")

test_that("translate returns a character", {
  expect_type(translate("IT", words, "Settings"), "character")
})

test_that("translate returns the expected result", {
  expect_equal(translate("IT", words, "Settings"), "Impostazioni")
})