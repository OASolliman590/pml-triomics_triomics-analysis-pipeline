triomics_report_abort <- function(code, message, key = NULL,
                                   remediation = "Use verified local producer exports and the packaged report template.") {
  triomics_abort(code, message, key = key, module = "report", stage = "report",
                 remediation = remediation)
}

triomics_report_safe_child <- function(root, relative, key, must_exist = TRUE) {
  if (!triomics_d_nonempty_string(root) || !dir.exists(root) || triomics_is_symlink(root) ||
      !triomics_d_nonempty_string(relative) ||
      grepl("^(?:[A-Za-z]:[/\\\\]|/|\\\\)", relative)) {
    triomics_report_abort("D_REPORT_PATH_UNSAFE", "A report path is not local to its verified output root.", key)
  }
  parts <- strsplit(relative, "[/\\\\]+")[[1L]]
  parts <- parts[nzchar(parts)]
  if (!length(parts) || any(parts %in% c(".", ".."))) {
    triomics_report_abort("D_REPORT_PATH_UNSAFE", "A report path contains traversal components.", key)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  path <- triomics_d_secure_path(file.path(root, relative), "D_REPORT_PATH_UNSAFE",
                                 "A report path crosses a symbolic link or escapes its root.", key)
  prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
  if (!(identical(path, root) || startsWith(path, prefix)) ||
      (must_exist && (!file.exists(path) || dir.exists(path)))) {
    triomics_report_abort("D_REPORT_PATH_UNSAFE", "A report path is missing or escapes its verified root.", key)
  }
  path
}

triomics_report_clean_text <- function(value) {
  value <- enc2utf8(as.character(value))
  value[is.na(value)] <- ""
  value <- gsub("[\\r\\n\\t]", " ", value, perl = TRUE)
  value <- gsub("[[:cntrl:]]", "", value, perl = TRUE)
  path_patterns <- c(
    "(?i)\\bfile:(?:///?)[^,;<>\\\"'[:cntrl:]]+",
    "(?i)(?<![[:alnum:]_])[A-Z]:[\\\\/][^,;<>\\\"'[:cntrl:]]+",
    "(?<![[:alnum:]_])\\\\\\\\[^/\\\\[:space:]]+[\\\\/][^,;<>\\\"'[:cntrl:]]+",
    "(?<![:/])//[^/[:space:]]+/[^,;<>\\\"'[:cntrl:]]+",
    "(?<![/:[:alnum:]_.-])/(?:[^/[:space:],;<>\\\"']+/)+[^,;<>\\\"'[:cntrl:]]+"
  )
  for (pattern in path_patterns) {
    value <- gsub(pattern, "[redacted local path]", value, perl = TRUE)
  }
  credential_patterns <- c(
    "(?i)\\b(?:password|passwd|secret|token|api[_-]?key|authorization|credential)\\s*[:=]\\s*(?:bearer\\s+)?[^,;[:space:]<>\\\"']+",
    "(?i)\\bbearer\\s+[A-Za-z0-9._~+/-]+=*"
  )
  for (pattern in credential_patterns) {
    value <- gsub(pattern, "[redacted credential]", value, perl = TRUE)
  }
  value
}

triomics_report_contains_private_text <- function(value) {
  value <- enc2utf8(as.character(value))
  value[is.na(value)] <- ""
  path_patterns <- c(
    "(?i)\\bfile:(?:///?)[^,;<>\\\"'[:cntrl:]]+",
    "(?i)(?<![[:alnum:]_])[A-Z]:[\\\\/][^,;<>\\\"'[:cntrl:]]+",
    "(?<![[:alnum:]_])\\\\\\\\[^/\\\\[:space:]]+[\\\\/][^,;<>\\\"'[:cntrl:]]+",
    "(?<![:/])//[^/[:space:]]+/[^,;<>\\\"'[:cntrl:]]+",
    "(?<![/:[:alnum:]_.-])/(?:[^/[:space:],;<>\\\"']+/)+[^,;<>\\\"'[:cntrl:]]+"
  )
  credential_patterns <- c(
    "(?i)\\b(?:password|passwd|secret|token|api[_-]?key|authorization|credential)\\s*[:=]\\s*(?:bearer\\s+)?[^,;[:space:]<>\\\"']+",
    "(?i)\\bbearer\\s+[A-Za-z0-9._~+/-]+=*"
  )
  vapply(value, function(item) {
    any(vapply(c(path_patterns, credential_patterns), function(pattern) {
      grepl(pattern, item, perl = TRUE)
    }, logical(1)))
  }, logical(1))
}

triomics_report_html_escape <- function(value) {
  value <- triomics_report_clean_text(value)
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub("\"", "&quot;", value, fixed = TRUE)
  gsub("'", "&#39;", value, fixed = TRUE)
}

triomics_report_tsv_text <- function(value) {
  value <- triomics_report_clean_text(value)
  unsafe_formula <- grepl("^[[:space:]]*[=+@-]", value)
  value[unsafe_formula] <- paste0("'", value[unsafe_formula])
  value
}

triomics_report_template_path <- function() {
  namespace_root <- tryCatch(
    normalizePath(getNamespaceInfo(asNamespace("triomics"), "path"),
                  winslash = "/", mustWork = TRUE),
    error = function(error) ""
  )
  source_path <- if (nzchar(namespace_root)) {
    file.path(namespace_root, "inst", "report", "triomics.qmd")
  } else ""
  if (nzchar(source_path) && file.exists(source_path) && !dir.exists(source_path)) {
    return(normalizePath(source_path, winslash = "/", mustWork = TRUE))
  }
  installed_path <- system.file("report", "triomics.qmd", package = "triomics")
  if (nzchar(installed_path) && file.exists(installed_path) && !dir.exists(installed_path)) {
    return(normalizePath(installed_path, winslash = "/", mustWork = TRUE))
  }
  triomics_report_abort("D_REPORT_TEMPLATE_MISSING", "The packaged Quarto report template is unavailable.",
                        "report.template")
}

triomics_report_styles_path <- function() {
  namespace_root <- tryCatch(
    normalizePath(getNamespaceInfo(asNamespace("triomics"), "path"),
                  winslash = "/", mustWork = TRUE),
    error = function(error) ""
  )
  source_path <- if (nzchar(namespace_root)) {
    file.path(namespace_root, "inst", "report", "styles.css")
  } else ""
  if (nzchar(source_path) && file.exists(source_path) && !dir.exists(source_path)) {
    return(normalizePath(source_path, winslash = "/", mustWork = TRUE))
  }
  installed_path <- system.file("report", "styles.css", package = "triomics")
  if (nzchar(installed_path) && file.exists(installed_path) && !dir.exists(installed_path)) {
    return(normalizePath(installed_path, winslash = "/", mustWork = TRUE))
  }
  triomics_report_abort("D_REPORT_TEMPLATE_MISSING", "The packaged report stylesheet is unavailable.",
                        "report.styles")
}

triomics_report_section_template_path <- function(section) {
  if (!is.character(section) || length(section) != 1L ||
      !section %in% c("targets", "clinical")) {
    triomics_report_abort("D_REPORT_TEMPLATE_MISSING", "The requested report section is not registered.",
                          "report.section")
  }
  filename <- paste0(section, ".qmd")
  namespace_root <- tryCatch(
    normalizePath(getNamespaceInfo(asNamespace("triomics"), "path"),
                  winslash = "/", mustWork = TRUE),
    error = function(error) ""
  )
  source_path <- if (nzchar(namespace_root)) {
    file.path(namespace_root, "inst", "report", "sections", filename)
  } else ""
  if (nzchar(source_path) && file.exists(source_path) && !dir.exists(source_path)) {
    return(normalizePath(source_path, winslash = "/", mustWork = TRUE))
  }
  installed_path <- system.file("report", "sections", filename, package = "triomics")
  if (nzchar(installed_path) && file.exists(installed_path) && !dir.exists(installed_path)) {
    return(normalizePath(installed_path, winslash = "/", mustWork = TRUE))
  }
  ""
}

triomics_report_quarto_path <- function() {
  configured <- Sys.getenv("QUARTO_PATH", unset = "")
  candidate <- if (nzchar(configured)) configured else unname(Sys.which("quarto"))
  if (!is.character(candidate) || length(candidate) != 1L || !nzchar(candidate) ||
      !file.exists(candidate) || dir.exists(candidate) || file.access(candidate, 1L) != 0L) {
    triomics_report_abort("D_REPORT_RUNTIME_MISSING", "A local executable Quarto runtime is required.",
                          "QUARTO_PATH",
                          "Install or select the qualified local Quarto runtime without enabling network access.")
  }
  normalizePath(candidate, winslash = "/", mustWork = TRUE)
}

triomics_report_external_reference <- function(html) {
  tags <- regmatches(html, gregexpr("(?is)<[a-z][^>]*>", html, perl = TRUE))[[1L]]
  resource_tag <- grepl("(?is)^<(?:script|link|img|iframe|source|video|audio|object|embed|track|image|use|feimage)\\b",
                        tags, perl = TRUE)
  resource_tags <- tags[resource_tag]
  reference_found <- any(vapply(resource_tags, function(tag) {
    attributes <- regmatches(tag, gregexpr(
      "(?is)\\b(?:src|href|xlink:href|poster|data|srcset)\\s*=\\s*(?:'[^']*'|\"[^\"]*\"|[^\\s>]+)",
      tag, perl = TRUE
    ))[[1L]]
    if (!length(attributes)) return(FALSE)
    any(vapply(attributes, function(attribute) {
      value <- sub("(?is)^[^=]+=[[:space:]]*", "", attribute, perl = TRUE)
      value <- sub("^[\"']", "", value, perl = TRUE)
      value <- sub("[\"']$", "", value, perl = TRUE)
      value <- trimws(value)
      if (!nzchar(value)) return(FALSE)
      if (grepl("(?i)^data:", value, perl = TRUE) || startsWith(value, "#")) return(FALSE)
      TRUE
    }, logical(1)))
  }, logical(1)))
  reference_found ||
    grepl("(?is)@import\\b(?!\\s*(?:url\\()?\\s*['\"]?data:)", html, perl = TRUE) ||
    grepl("(?is)url\\(\\s*['\"]?(?!data:|#)[^)]+\\)", html, perl = TRUE) ||
    grepl("(?is)\\bfetch\\s*\\(|\\bXMLHttpRequest\\b|\\bWebSocket\\s*\\(|\\bEventSource\\s*\\(",
          html, perl = TRUE)
}
