policy_version <- "1.0.0"

#' Codebook for AI policies in research
#'
#' The ten dimensions on which [air_code_policy()] codes journal, publisher,
#' funder or institutional AI policies, grouped as authorship, disclosure,
#' permitted uses, peer review and responsibility. Each dimension has a
#' question and the regular expression that recognises it. The codebook is
#' versioned (1.0.0); a changed rule is a new version.
#'
#' @return A data frame with columns `id`, `group`, `question` and `pattern`.
#' @export
#' @examples
#' air_policy_codebook()[, c("id", "group")]
air_policy_codebook <- function() read_dictionary("policy_codebook.csv")

#' Code AI policies on the codebook
#'
#' Reads the text of each policy and records, for every dimension of
#' [air_policy_codebook()], whether the policy addresses it (`"stated"`, with
#' the sentence as evidence) or not (`"not_stated"`). `"not_stated"` means the
#' text does not say it, not that the policy allows or forbids it. A missing
#' text gives `"unverifiable"`.
#'
#' Automatic coding is a first pass: the rules recognise common wordings, and
#' a published analysis should have a person check the evidence of each
#' `"stated"` and a sample of `"not_stated"` (see the `evidence` columns).
#'
#' @param text A character vector of policy texts.
#' @param id Identifiers, one per policy (for example journal or institution
#'   names).
#' @param codebook The codebook; by default [air_policy_codebook()].
#' @return A data frame in long format with one row per policy and dimension:
#'   `id`, `codebook_version`, `dimension`, `group`, `value`, `evidence`.
#' @export
#' @examples
#' coded <- air_code_policy(c(
#'   paste("AI tools cannot be listed as authors.",
#'         "Authors must disclose the use of AI in the methods section."),
#'   "Reviewers must not upload manuscripts to AI tools, as this may breach confidentiality."
#' ), id = c("Journal A", "Journal B"))
#' coded[coded$value == "stated", c("id", "dimension")]
air_code_policy <- function(text, id = NULL, codebook = air_policy_codebook()) {
  if (!is.character(text) && !all(is.na(text))) stop("`text` must be a character vector.", call. = FALSE)
  text <- as.character(text)
  id <- id %||% seq_along(text)
  if (length(id) != length(text)) stop("`id` must have one value per text.", call. = FALSE)
  if (anyDuplicated(id)) stop("`id` values must be unique.", call. = FALSE)
  if (!all(c("id", "pattern") %in% names(codebook))) stop("`codebook` needs columns `id` and `pattern`.", call. = FALSE)
  group <- codebook$group %||% rep(NA_character_, nrow(codebook))
  rows <- lapply(seq_along(text), function(i) {
    readable <- !is.na(text[i]) && nzchar(trimws(text[i]))
    s <- if (readable) split_sentences(gsub("\\s+", " ", text[i])) else character()
    do.call(rbind, lapply(seq_len(nrow(codebook)), function(k) {
      m <- s[hits(codebook$pattern[k], s)]
      data.frame(id = as.character(id[i]), codebook_version = policy_version, dimension = codebook$id[k],
                 group = group[k],
                 value = if (!readable) "unverifiable" else if (length(m)) "stated" else "not_stated",
                 evidence = if (length(m)) substr(m[1], 1, 400) else NA_character_, stringsAsFactors = FALSE)
    }))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Compare coded policies side by side
#'
#' Turns the long output of [air_code_policy()] into a matrix with one row per
#' dimension and one column per policy, and adds how many policies address
#' each dimension. Policies that could not be read are left out of the count.
#'
#' @param coded A data frame from [air_code_policy()].
#' @return A data frame: `dimension`, `group`, one column per policy with its
#'   value, and `n_stated` and `n_readable`.
#' @export
#' @examples
#' coded <- air_code_policy(c(
#'   "AI tools cannot be listed as authors. Authors remain fully responsible for the content.",
#'   "Authors must disclose any use of generative AI. Authors are accountable for the work."
#' ), id = c("A", "B"))
#' air_compare_policies(coded)
air_compare_policies <- function(coded) {
  if (!all(c("id", "dimension", "value") %in% names(coded))) {
    stop("`coded` must come from air_code_policy().", call. = FALSE)
  }
  dims <- unique(coded$dimension)
  ids <- unique(coded$id)
  out <- data.frame(dimension = dims, group = coded$group[match(dims, coded$dimension)], stringsAsFactors = FALSE)
  for (p in ids) out[[p]] <- coded$value[coded$id == p][match(dims, coded$dimension[coded$id == p])]
  vals <- as.matrix(out[, ids, drop = FALSE])
  out$n_stated <- rowSums(vals == "stated")
  out$n_readable <- rowSums(vals != "unverifiable")
  out
}
