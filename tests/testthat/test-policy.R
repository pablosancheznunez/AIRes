# Synthetic policies written for these tests; they paraphrase common
# provisions and do not reproduce any publisher's text.
full_policy <- paste(
  "Generative AI tools cannot be listed as authors, because they cannot take responsibility for the work.",
  "Authors must disclose the use of AI tools in a dedicated statement before the references.",
  "The statement should give the name and version of the tool and explain how it was used.",
  "Using AI only to correct grammar and spelling is allowed and does not need to be declared.",
  "AI must not be used to generate data, results or interpretations.",
  "Images and figures created or altered with generative AI are not permitted.",
  "Reviewers must not upload manuscripts under review to AI tools.",
  "Entering confidential manuscripts into AI tools may breach confidentiality and data protection.",
  "Authors remain fully responsible for the content of their submission."
)

test_that("every dimension of the codebook is recognised in a complete policy", {
  coded <- air_code_policy(full_policy, id = "Full")
  expect_equal(nrow(coded), nrow(air_policy_codebook()))
  expect_true(all(coded$value == "stated"), info = paste(coded$dimension[coded$value != "stated"], collapse = ", "))
  expect_true(all(!is.na(coded$evidence)))
  expect_true(all(coded$codebook_version == "1.0.0"))
})

test_that("unaddressed dimensions are not_stated and missing texts unverifiable", {
  coded <- air_code_policy(c("Submissions are welcome in English.", NA), id = c("Silent", "Missing"))
  expect_true(all(coded$value[coded$id == "Silent"] == "not_stated"))
  expect_true(all(coded$value[coded$id == "Missing"] == "unverifiable"))
})

test_that("partial policies are coded dimension by dimension", {
  coded <- air_code_policy(
    "Authors are required to declare any use of large language models. Responsibility remains with the human authors.",
    id = "Partial")
  stated <- coded$dimension[coded$value == "stated"]
  expect_setequal(stated, c("disclosure_required", "authors_responsible"))
})

test_that("comparison counts only readable policies", {
  coded <- air_code_policy(c(full_policy, "Submissions are welcome.", NA), id = c("A", "B", "C"))
  cmp <- air_compare_policies(coded)
  expect_equal(names(cmp), c("dimension", "group", "A", "B", "C", "n_stated", "n_readable"))
  expect_true(all(cmp$n_stated == 1))
  expect_true(all(cmp$n_readable == 2))
  expect_error(air_compare_policies(data.frame(a = 1)), "air_code_policy")
  expect_error(air_code_policy(c("a", "b"), id = c("x", "x")), "unique")
})
