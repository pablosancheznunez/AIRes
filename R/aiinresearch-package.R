#' aiinresearch: declared use of AI in research publications and policies
#'
#' * [air_detect_disclosure()]: AI-use disclosures in article full texts;
#' * [air_code_policy()] with [air_policy_codebook()]: what journal and
#'   institutional AI policies require, allow and forbid;
#'   [air_compare_policies()] puts several side by side;
#' * [air_summarise()]: disclosure rates across a corpus, with intervals.
#'
#' # What is measured, and what is not
#'
#' Every result has `ai_use_basis = "declared"` (metascience vocabulary): it
#' reports what authors or policies *say*. The package does not estimate
#' whether a text was written with AI. Automated AI-text detection is not
#' reliable evidence of authorship, so a missing disclosure is reported as
#' "no disclosure found", never as hidden use. Declared non-use ("no AI tools
#' were used") is recorded separately from the absence of any statement.
#'
#' A text that cannot be read is `unverifiable`; a policy dimension that the
#' text does not address is `not_stated`, never "no".
#'
#' Project-level logging, verification and disclosure writing are covered by
#' the aitrail package.
#'
#' @keywords internal
"_PACKAGE"
