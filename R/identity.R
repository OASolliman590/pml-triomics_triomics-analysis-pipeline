triomics_encode_identity_part <- function(value, key) {
  if (is.factor(value) || is.numeric(value)) {
    triomics_abort("A_ID_COERCED", "Identifiers must stay character so leading zeros survive.",
                   key = key, module = "identity", stage = "A",
                   remediation = "Read identifiers as character before constructing keys.")
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    triomics_abort("A_ID_INVALID", "An identifier part is missing or blank.",
                   key = key, module = "identity", stage = "A",
                   remediation = "Supply one non-blank character identifier.")
  }
  paste0(nchar(value, type = "chars"), ":", value)
}

triomics_feature_key <- function(study_id, block_id, source_row) {
  if (is.numeric(source_row) && length(source_row) == 1L && is.finite(source_row) &&
      source_row == trunc(source_row)) {
    source_row <- format(source_row, scientific = FALSE, trim = TRUE)
  }
  paste0(
    "fk1|",
    triomics_encode_identity_part(study_id, "study_id"), "|",
    triomics_encode_identity_part(block_id, "block_id"), "|",
    triomics_encode_identity_part(source_row, "source_row")
  )
}

triomics_preserve_identifier <- function(value, key = "sample_id") {
  if (is.factor(value) || is.numeric(value)) {
    triomics_abort("A_ID_COERCED", "Identifiers must stay character so leading zeros survive.",
                   key = key, module = "identity", stage = "A",
                   remediation = "Read identifier columns as character.")
  }
  if (!is.character(value) || anyNA(value) || any(!nzchar(value))) {
    triomics_abort("A_ID_INVALID", "An identifier is missing or blank.",
                   key = key, module = "identity", stage = "A",
                   remediation = "Supply non-blank character identifiers.")
  }
  value
}
