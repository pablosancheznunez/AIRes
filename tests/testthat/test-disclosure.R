statement <- paste(
  "Results are discussed below.",
  "Declaration of generative AI in the writing process.",
  "During the preparation of this work the authors used ChatGPT and DeepL to improve readability and translate the abstract.",
  "After using these tools, the authors reviewed and edited the content and take full responsibility for the publication."
)

test_that("a full disclosure statement is recognised with tools, purposes and oversight", {
  d <- air_detect_disclosure(statement)
  expect_equal(d$disclosure_statement, "yes")
  expect_equal(d$use_declared, "yes")
  expect_equal(d$non_use_declared, "no")
  expect_equal(d$oversight_stated, "yes")
  expect_equal(d$tools, "ChatGPT; DeepL")
  expect_equal(d$purposes, "language_editing; translation")
  expect_equal(d$ai_use_basis, "declared")
  expect_match(d$evidence, "Declaration of generative AI")
})

test_that("declared non-use is a statement, but not a declaration of use", {
  for (t in c("No AI tools were used in the preparation of this manuscript.",
              "The authors did not use generative AI or large language models at any stage.",
              "This article was written without the use of artificial intelligence.")) {
    d <- air_detect_disclosure(t)
    expect_equal(d$non_use_declared, "yes", info = t)
    expect_equal(d$use_declared, "no", info = t)
    expect_equal(d$disclosure_statement, "yes", info = t)
  }
})

test_that("no statement is 'no' and missing text is unverifiable", {
  d <- air_detect_disclosure(c("We measured soil nitrogen in 40 plots. Data are on Zenodo.", NA, " "))
  expect_equal(d$disclosure_statement, c("no", "unverifiable", "unverifiable"))
  expect_equal(d$ai_mentioned, c("no", "unverifiable", "unverifiable"))
})

test_that("AI as a research topic is a mention, not a disclosure", {
  d <- air_detect_disclosure(paste("Artificial intelligence is transforming radiology.",
                                   "We trained a convolutional network on 2,000 scans and report its accuracy."))
  expect_equal(d$ai_mentioned, "yes")
  expect_equal(d$use_declared, "no")
  expect_equal(d$disclosure_statement, "no")
})

test_that("a declaration needs a generative tool, a use verb and a purpose or statement", {
  expect_equal(air_detect_disclosure("We used Grammarly to check spelling.")$use_declared, "yes")
  expect_equal(air_detect_disclosure("ChatGPT has been widely discussed.")$use_declared, "no")
  expect_equal(air_detect_disclosure("Claude Bernard described the milieu interieur.")$tools, NA_character_)
})

test_that("Spanish statements are recognised", {
  d <- air_detect_disclosure(paste(
    "Declaraci\u00f3n sobre el uso de inteligencia artificial.",
    "Los autores emplearon ChatGPT para la traducci\u00f3n del resumen y asumen la plena responsabilidad del contenido."))
  expect_equal(d$disclosure_statement, "yes")
  expect_equal(d$use_declared, "yes")
  expect_equal(d$purposes, "translation")
  expect_equal(d$oversight_stated, "yes")
})

test_that("custom dictionaries extend the detector", {
  tools <- rbind(air_tools(), data.frame(tool = "LabBot", pattern = "\\blabbot\\b", category = "assistant"))
  d <- air_detect_disclosure("We used LabBot to draft the introduction.", tools = tools)
  expect_equal(d$tools, "LabBot")
  expect_equal(d$purposes, "drafting")
  expect_error(air_detect_disclosure("x", tools = data.frame(a = 1)), "tool")
  expect_error(air_detect_disclosure("x", id = 1:2), "one value per text")
})

test_that("summaries exclude unverifiable texts and give intervals", {
  d <- air_detect_disclosure(c(statement, "No generative AI was used in this work.", "We sampled rivers.", NA))
  s <- air_summarise(d)
  row <- s[s$indicator == "disclosure_statement", ]
  expect_equal(row$n_readable, 3)
  expect_equal(row$n_unverifiable, 1)
  expect_equal(row$n_yes, 2)
  expect_equal(row$share, 2 / 3)
  expect_true(row$lower < row$share && row$upper > row$share)
  g <- air_summarise(d, group = c("A", "A", "B", "B"))
  expect_equal(unique(g$group), c("A", "B"))
  expect_true(is.na(g$share[g$group == "B" & g$indicator == "use_declared"]) == FALSE)
  expect_error(air_summarise(data.frame(x = 1)), "air_detect_disclosure")
})
