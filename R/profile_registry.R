 .triomics_profile_registry <- new.env(parent = emptyenv())

triomics_profile_stage_names <- function() {
  c("validate", "filter", "transform", "normalise", "impute", "derive")
}

triomics_register_profile <- function(profile_id, methods) {
  if (!is.character(profile_id) || length(profile_id) != 1L || is.na(profile_id) || !nzchar(profile_id)) {
    triomics_abort("A_CONFIG_PROFILE", "A profile id must be one non-blank string.",
                   key = "profile_id", module = "profile_registry", stage = "A",
                   remediation = "Register the profile under its provenance id, not a display name.")
  }
  needed <- triomics_profile_stage_names()
  if (!is.list(methods) || is.null(names(methods)) || !all(needed %in% names(methods))) {
    triomics_abort("A_CONFIG_PROFILE",
                   "A profile must register validate, filter, transform, normalise, impute, and derive.",
                   key = "profile", module = "profile_registry", stage = "A",
                   remediation = "Supply all six functions. Derive may be an explicit no-op.")
  }
  for (name in needed) {
    if (!is.function(methods[[name]])) {
      triomics_abort("A_CONFIG_PROFILE", paste0("Profile method '", name, "' must be a function."),
                     key = name, module = "profile_registry", stage = "A",
                     remediation = "Do not register a profile by display name or by a partial function list.")
    }
  }
  assign(profile_id, methods[needed], envir = .triomics_profile_registry)
  invisible(profile_id)
}

triomics_registered_profile <- function(profile_id) {
  if (!is.character(profile_id) || length(profile_id) != 1L || is.na(profile_id) ||
      !exists(profile_id, envir = .triomics_profile_registry, inherits = FALSE)) {
    triomics_abort("A_CONFIG_PROFILE", paste0("Profile '", profile_id, "' is not registered."),
                   key = "profile_id", module = "profile_registry", stage = "A",
                   remediation = "Register the profile id before prepare_fit. Do not dispatch on a block display name.")
  }
  get(profile_id, envir = .triomics_profile_registry, inherits = FALSE)
}

triomics_reset_profiles <- function() {
  remove <- ls(envir = .triomics_profile_registry)
  if (length(remove)) rm(list = remove, envir = .triomics_profile_registry)
  invisible(TRUE)
}
