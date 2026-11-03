`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

disclosure_version <- "1.0.0"

read_dictionary <- function(file) {
  utils::read.csv(system.file("extdata", file, package = "AIRes"), stringsAsFactors = FALSE,
                  encoding = "UTF-8")
}

#' Dictionaries of AI tools and declared purposes
#'
#' The tools and purposes that [air_detect_disclosure()] recognises, each with
#' its regular expression (Perl syntax, case-insensitive). Pass an extended
#' copy to `air_detect_disclosure()` to recognise more.
#'
#' @return A data frame: `tool`, `pattern`, `category` for tools; `id`,
#'   `label`, `pattern` for purposes.
#' @export
#' @examples
#' air_tools()[, c("tool", "category")]
#' air_purposes()[, c("id", "label")]
air_tools <- function() read_dictionary("ai_tools.csv")

#' @rdname air_tools
#' @export
air_purposes <- function() read_dictionary("ai_purposes.csv")

# Patterns use \x{..} escapes so that the source stays ASCII.
generic_ai <- paste0("(generative (ai|artificial intelligence)|\\bgenai\\b|large language models?|\\bllms?\\b|",
                     "chatbots?|ai[- ](assisted|based|powered|generated) (tools?|technolog\\w*|writing)|",
                     "\\bai (tools?|assistants?|technolog\\w*|chatbots?)\\b|inteligencia artificial generativa|",
                     "herramientas? de (ia|inteligencia artificial))")
any_ai <- paste0("(\\bai\\b|artificial intelligence|inteligencia artificial|\\bia\\b|", substring(generic_ai, 2))
heading_pattern <- paste0(
  "(declaration of (generative )?ai|(generative )?ai (use |usage |disclosure )?(statement|declaration|disclosure)|",
  "use of (generative )?(ai|artificial intelligence)( tools| technologies)? (in|during) (the )?(writing|preparation|research)|",
  "generative ai and ai-assisted technologies|",
  "declaraci\\x{f3}n (de|sobre el) uso de (la )?(ia|inteligencia artificial))"
)
use_verbs <- paste0("\\b(used|use|using|employed|utili[sz]ed|with the (help|assistance|support) of|assisted by|",
                    "relied on|prompted|utiliz\\w*|empleamos|emple\\w*|se us\\x{f3}|con (la )?ayuda de)\\b")
non_use_pattern <- paste0(
  "(\\bno (generative )?(ai|artificial intelligence|ai-assisted|large language model)[^.]{0,60}(was|were|has been|have been) used|",
  "(did not|didn't|have not|has not|were not|was not|do not) (use|used|employ\\w*|utili[sz]\\w*)[^.]{0,80}",
  "(\\bai\\b|artificial intelligence|language model|chat ?gpt|generative)|",
  "(\\bai\\b|artificial intelligence|generative ai|chat ?gpt)[^.]{0,60}(was|were) not used|",
  "without (the )?(use|help|assistance) of (any )?(generative )?(ai|artificial intelligence)|",
  "no se (ha |han )?(utiliz|us)\\w*[^.]{0,60}(ia|inteligencia artificial))"
)
oversight_pattern <- paste0(
  "(reviewed and edited|review(ed)? and revis\\w*|take[s]? (full )?responsibility|",
  "(are|remain|is) (fully )?(responsible|accountable)|checked (and|for) (accuracy|correctness)|",
  "verified (by the authors|all|the (output|content|text))|human (review|oversight|verification)|",
  "asum\\w* (la )?(total |plena )?responsabilidad|revis\\w* y edit\\w*)"
)

split_sentences <- function(text) {
  s <- unlist(strsplit(text, "(?<=[.!?;])\\s+(?=[A-Z(\\[\\x{c1}\\x{c9}\\x{cd}\\x{d3}\\x{da}])", perl = TRUE))
  s[nchar(s) > 0 & nchar(s) <= 800]
}

hits <- function(pattern, x) grepl(pattern, x, ignore.case = TRUE, perl = TRUE)

detect_one <- function(text, tools, purposes) {
  out <- list(ai_mentioned = "unverifiable", disclosure_statement = "unverifiable", use_declared = "unverifiable",
              non_use_declared = "unverifiable", oversight_stated = "unverifiable", tools = NA_character_,
              purposes = NA_character_, evidence = NA_character_)
  if (is.na(text) || !nzchar(trimws(text))) return(out)
  s <- split_sentences(text)
  tool_hit <- vapply(tools$pattern, hits, logical(length(s)), x = s)
  tool_hit <- matrix(tool_hit, nrow = length(s))
  has_tool <- rowSums(tool_hit) > 0
  ai_any <- hits(any_ai, s) | has_tool
  heading <- hits(heading_pattern, s)
  non_use <- hits(non_use_pattern, s)
  purpose_hit <- matrix(vapply(purposes$pattern, hits, logical(length(s)), x = s), nrow = length(s))
  # A declaration of use names a tool or generative AI, uses a use verb, and
  # either sits in an AI statement or states what the tool was used for.
  ai_tool_context <- has_tool | hits(generic_ai, s)
  in_statement <- heading | c(FALSE, heading[-length(heading)])
  declared <- ai_tool_context & hits(use_verbs, s) & !non_use & (in_statement | rowSums(purpose_hit) > 0)
  relevant <- declared | non_use | heading
  # Oversight is usually stated in the sentence or two after the declaration.
  n <- length(s)
  near <- relevant | c(FALSE, relevant[-n])[seq_len(n)] | c(FALSE, FALSE, relevant)[seq_len(n)]
  oversight <- hits(oversight_pattern, s) & (ai_any | near)
  relevant <- relevant | oversight
  yn <- function(x) if (any(x)) "yes" else "no"
  out$ai_mentioned <- yn(ai_any)
  out$disclosure_statement <- yn(heading | declared | non_use)
  out$use_declared <- yn(declared)
  out$non_use_declared <- yn(non_use)
  out$oversight_stated <- yn(oversight)
  used_tools <- tools$tool[colSums(tool_hit[declared | heading, , drop = FALSE]) > 0]
  used_purposes <- purposes$id[colSums(purpose_hit[declared, , drop = FALSE]) > 0]
  if (length(used_tools)) out$tools <- paste(used_tools, collapse = "; ")
  if (length(used_purposes)) out$purposes <- paste(used_purposes, collapse = "; ")
  if (any(relevant)) out$evidence <- substr(paste(s[relevant][seq_len(min(2, sum(relevant)))], collapse = " "), 1, 600)
  out
}

#' Detect AI-use disclosures in article full texts
#'
#' Applies explicit, versioned rules (version 1.0.0) to each text and reports:
#'
#' * `ai_mentioned`: the text mentions AI at all (in any sense, including AI
#'   as a research topic);
#' * `disclosure_statement`: there is a statement about AI use in preparing
#'   the work: a heading such as "Declaration of generative AI...", a
#'   declaration of use, or a declaration of non-use;
#' * `use_declared`: the authors declare having used a generative AI tool,
#'   naming it or its purpose;
#' * `non_use_declared`: the authors declare that no AI was used;
#' * `oversight_stated`: the authors state that they reviewed the output or
#'   take responsibility for it;
#' * `tools`, `purposes`: the tools named and purposes declared in the
#'   statement (see [air_tools()] and [air_purposes()]);
#' * `evidence`: the sentences that support the result.
#'
#' Values are `"yes"`, `"no"` or `"unverifiable"` (no text). `"no"` means no
#' such statement was found in the text, not that AI was or was not used.
#' Every row has `ai_use_basis = "declared"`. English and Spanish are
#' recognised.
#'
#' Known limitation: articles whose *subject* is generative AI ("we used
#' ChatGPT to generate the stimuli of the experiment") can be read as a
#' declaration of use; check the evidence column in such corpora.
#'
#' @param text A character vector of full texts; `NA` when unavailable.
#' @param id Optional identifiers, one per text.
#' @param tools,purposes Dictionaries; by default [air_tools()] and
#'   [air_purposes()].
#' @return A data frame with one row per text.
#' @export
#' @examples
#' air_detect_disclosure(c(
#'   paste("Declaration of generative AI in the writing process.",
#'         "During the preparation of this work the authors used ChatGPT to improve readability.",
#'         "After using this tool, the authors reviewed and edited the content",
#'         "and take full responsibility for the publication."),
#'   "No AI tools were used in the preparation of this manuscript.",
#'   "We measured soil nitrogen in 40 plots.",
#'   NA
#' ), id = c("a", "b", "c", "d"))
air_detect_disclosure <- function(text, id = NULL, tools = air_tools(), purposes = air_purposes()) {
  if (!is.character(text) && !all(is.na(text))) stop("`text` must be a character vector.", call. = FALSE)
  text <- as.character(text)
  id <- id %||% seq_along(text)
  if (length(id) != length(text)) stop("`id` must have one value per text.", call. = FALSE)
  if (!all(c("tool", "pattern") %in% names(tools))) stop("`tools` needs columns `tool` and `pattern`.", call. = FALSE)
  if (!all(c("id", "pattern") %in% names(purposes))) stop("`purposes` needs columns `id` and `pattern`.", call. = FALSE)
  rows <- lapply(seq_along(text), function(i) {
    d <- detect_one(text[i], tools, purposes)
    data.frame(id = as.character(id[i]), rules_version = disclosure_version, ai_use_basis = "declared",
               d, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Summarise disclosures across a corpus
#'
#' Computes, overall or by group (for example journal, field or year), the
#' share of articles with each indicator among the articles whose text could
#' be read, with Wilson confidence intervals, and the number of articles that
#' could not be read. Unverifiable articles never enter the denominator.
#'
#' @param detections A data frame from [air_detect_disclosure()].
#' @param group Optional vector, one value per row of `detections`.
#' @param conf Confidence level.
#' @return A data frame with one row per group and indicator: `group`,
#'   `indicator`, `n_readable`, `n_unverifiable`, `n_yes`, `share`, `lower`,
#'   `upper`.
#' @export
#' @examples
#' d <- air_detect_disclosure(c(
#'   "Declaration of AI use. We used ChatGPT to translate the abstract.",
#'   "No generative AI was used in this work.",
#'   "We sampled 12 rivers.", NA))
#' air_summarise(d)
#' air_summarise(d, group = c("J1", "J1", "J2", "J2"))
air_summarise <- function(detections, group = NULL, conf = 0.95) {
  ind <- c("disclosure_statement", "use_declared", "non_use_declared", "oversight_stated")
  if (!all(ind %in% names(detections))) stop("`detections` must come from air_detect_disclosure().", call. = FALSE)
  group <- group %||% rep("all", nrow(detections))
  if (length(group) != nrow(detections)) stop("`group` must have one value per row.", call. = FALSE)
  z <- stats::qnorm(1 - (1 - conf) / 2)
  out <- do.call(rbind, lapply(sort(unique(group)), function(g) {
    d <- detections[group == g, , drop = FALSE]
    do.call(rbind, lapply(ind, function(k) {
      readable <- d[[k]] != "unverifiable"
      n <- sum(readable)
      y <- sum(d[[k]][readable] == "yes")
      p <- if (n) y / n else NA_real_
      centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
      half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
      data.frame(group = g, indicator = k, n_readable = n, n_unverifiable = sum(!readable), n_yes = y,
                 share = p, lower = if (n) centre - half else NA_real_, upper = if (n) centre + half else NA_real_,
                 stringsAsFactors = FALSE)
    }))
  }))
  rownames(out) <- NULL
  out
}
