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
  # The shared secure-path helper can spell a nonexistent Unix child as //Users/...
  # because it joins the first component to /. Canonicalize that spelling only
  # when the verified root itself has the ordinary single-root form.
  if (startsWith(root, "/") && !startsWith(root, "//") && startsWith(path, "//")) {
    path <- sub("^/+", "/", path)
  }
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
    "(?<![/:[:alnum:]_.-])/(?:[^/[:space:],;<>\\\"']+/)*[^/[:space:],;<>\\\"'[:cntrl:]]+"
  )
  for (pattern in path_patterns) {
    value <- gsub(pattern, "[redacted local path]", value, perl = TRUE)
  }
  credential_patterns <- c(
    "(?i)\\b(?:password|passwd|secret|token|api[_-]?key|authorization|credential)[\\\"']?\\s*[:=]\\s*(?:(?:bearer\\s+)?\\\"[^\\\"[:cntrl:]]*\\\"|(?:bearer\\s+)?'[^'[:cntrl:]]*'|(?:bearer\\s+)?[^,;[:space:]<>\\\"']+)",
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
    "(?<![/:[:alnum:]_.-])/(?:[^/[:space:],;<>\\\"']+/)*[^/[:space:],;<>\\\"'[:cntrl:]]+"
  )
  credential_patterns <- c(
    "(?i)\\b(?:password|passwd|secret|token|api[_-]?key|authorization|credential)[\\\"']?\\s*[:=]\\s*(?:(?:bearer\\s+)?\\\"[^\\\"[:cntrl:]]*\\\"|(?:bearer\\s+)?'[^'[:cntrl:]]*'|(?:bearer\\s+)?[^,;[:space:]<>\\\"']+)",
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

triomics_report_source_resource_path <- function(relative_path) {
  option_root <- getOption("triomics.source_root", "")
  if (is.character(option_root) && length(option_root) == 1L && !is.na(option_root) &&
      nzchar(option_root)) {
    source_root <- tryCatch(normalizePath(option_root, winslash = "/", mustWork = TRUE),
                            error = function(error) "")
    if (nzchar(source_root) && file.exists(file.path(source_root, "DESCRIPTION")) &&
        dir.exists(file.path(source_root, "R"))) {
      candidate <- file.path(source_root, "inst", "report", relative_path)
      if (file.exists(candidate) && !dir.exists(candidate) && !triomics_is_symlink(candidate)) {
        return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
      }
    }
  }
  namespace_root <- tryCatch(
    normalizePath(getNamespaceInfo(asNamespace("triomics"), "path"),
                  winslash = "/", mustWork = TRUE),
    error = function(error) ""
  )
  namespace_source <- if (nzchar(namespace_root)) {
    file.path(namespace_root, "inst", "report", relative_path)
  } else ""
  if (nzchar(namespace_source) && file.exists(namespace_source) && !dir.exists(namespace_source) &&
      !triomics_is_symlink(namespace_source)) {
    return(normalizePath(namespace_source, winslash = "/", mustWork = TRUE))
  }
  ""
}

triomics_report_installed_resource_path <- function(relative_path,
                                                    package_root = system.file(package = "triomics")) {
  if (!is.character(package_root) || length(package_root) != 1L || is.na(package_root) ||
      !nzchar(package_root) || !dir.exists(package_root)) return("")
  candidate <- file.path(package_root, "report", relative_path)
  if (file.exists(candidate) && !dir.exists(candidate) && !triomics_is_symlink(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }
  ""
}

triomics_report_template_path <- function() {
  source_path <- triomics_report_source_resource_path(file.path("triomics.qmd"))
  if (nzchar(source_path)) return(source_path)
  installed_path <- triomics_report_installed_resource_path("triomics.qmd")
  if (nzchar(installed_path)) return(installed_path)
  triomics_report_abort("D_REPORT_TEMPLATE_MISSING", "The packaged Quarto report template is unavailable.",
                        "report.template")
}

triomics_report_styles_path <- function() {
  source_path <- triomics_report_source_resource_path(file.path("styles.css"))
  if (nzchar(source_path)) return(source_path)
  installed_path <- triomics_report_installed_resource_path("styles.css")
  if (nzchar(installed_path)) return(installed_path)
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
  source_path <- triomics_report_source_resource_path(file.path("sections", filename))
  if (nzchar(source_path)) return(source_path)
  installed_path <- triomics_report_installed_resource_path(file.path("sections", filename))
  if (nzchar(installed_path)) return(installed_path)
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
