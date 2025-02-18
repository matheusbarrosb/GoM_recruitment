convert_to_expression <- function(label) {
  label <- gsub("\\[alpha\\]", "alpha", label)
  label <- gsub("\\[beta\\]", "beta", label)
  label <- gsub("\\[gamma\\]", "gamma", label)
  parse(text = label)
}