# Global variables used in NSE (non-standard evaluation) contexts
# Silences "no visible binding for global variable" notes from R CMD check

utils::globalVariables(c(
  "a",
  "b",
  "fontfile",
  "FullName",
  "keep",
  "key",
  "name_new",
  "name_old",
  "new",
  "old",
  "sim"
))
