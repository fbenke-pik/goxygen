#' gamsequation2tex
#'
#' Convert a gams equation into latex code
#'
#'
#' @param x GAMS equation provided as character
#' @return GAMS equation converted to latex code
#' @author Jan Philipp Dietrich
#' @export
#' @importFrom stringi stri_extract_all_regex stri_replace_first_regex
#' stri_replace_first_fixed stri_count_fixed stri_extract_first_regex
#' stri_replace_all_fixed
#' @seealso \code{\link{goxygen}}
#' @examples
#'
#' x <- "eq_1 .. v_a =e= sum(j,v_b(j)*((1-s_c)+sum(cell(i,j),v_d(i)/f_d(i))));"
#' cat(gamsequation2tex(x))
gamsequation2tex <- function(x) {

  if (length(x) > 1) {
    out <- NULL
    for (i in x) out <- c(out, gamsequation2tex(i))
    return(out)
  }

  unconverted <- function(x, name, warning) {
    warning(warning)
    names(x) <- paste(name, "(CONVERSION FAILED!)")
    return(x)
  }

  convertSide <- function(x, eqMap) {

    extractVars <- function(x, variable, code = "v", protected = c("sum", "prod", "power")) {

      vars        <- stri_extract_all_regex(x, variable)[[1]]
      names(vars) <- paste0("#", code, 1:length(vars), "#")
      x           <- stri_replace_all_regex(x, variable, "#:.INSERTHERE.:#")

      for (v in names(vars)) {
        insert <- ifelse(vars[v] %in% protected, paste0("\\", vars[v]), v)
        x <- stri_replace_first_fixed(x, "#:.INSERTHERE.:#", insert)
      }

      vars <- vars[!(vars %in% protected)]

      vars <- sub("^AND$", "\\\\&", vars)
      vars <- sub("^OR$", "\\\\|", vars)
      vars <- sub("^eq$", "=", vars)
      vars <- sub("^ge$", "\\\\geq", vars)
      vars <- sub("^ne$", "\\\\neq", vars)

      if (code == "v") vars <- gsub("\\_", "\\\\_", vars)

      return(list(x = x, vars = vars))
    }

    extractBraceblocks <- function(x, level = 1) {
      braceblock <- "\\([^\\)\\(]*\\)"
      y <- extractVars(x, braceblock, paste0("b", level, "."))
      y$vars <- gsub("[()]", "", y$vars)
      y$x <- gsub("\\((#b[0-9.]*#)\\)", "\\1", y$x)
      z <- c(y$x, y$vars)
      if (grepl("\\(.*\\)", y$x)) {
        tmp <- extractBraceblocks(y$x, level = level + 1)
        z <- c(tmp, z[-1])
      }
      return(z[!is.na(z)])
    }

    convertFunctions <- function(z) {
      if (length(z) == 1) return(z)

      .tmp <- function(z, pattern, replacement, prefix) {
        if (is.null(z)) return(NULL)
        # Code needs to be in \\2!
        # prefixes need to be consistent to ids in replacement pattern
        # prefix "n" can be used to suppress brackets in convertBlocks
        # (brackets only set for ids starting with "b")
        while (grepl(pattern, z[1])) {
          code <- sub("^[^#]*#([^#]*)#.*$", "\\1", stri_extract_first_regex(z[1], pattern))
          id <- which(names(z) == paste0("#", code, "#"))
          if (length(id) != 1) return(NULL)
          split <- strsplit(z[id], ",")[[1]]
          if (length(split) != 2) return(NULL)
          names(split) <- paste0("#", prefix, code, c("a", "b"), "#")
          if (id == length(z)) {
            z <- c(z[1:(length(z) - 1)], split)
          } else {
            z <- c(z[1:(id - 1)], split, z[(id + 1):length(z)])
          }
          z[1] <- sub(pattern, replacement, z[1])
        }
        return(z)
      }
      z <- .tmp(z, "\\\\(sum|prod)#([^#]*)#", "\\\\\\1_{#n\\2a#}#\\2b#", c("n", ""))
      z <- .tmp(z, "\\\\(power)#([^#]*)#", "#\\2a#^{#n\\2b#}", c("", "n"))
      if (length(z) > 3) z <- c(z[1], convertFunctions(z[-1]))
      return(z)
    }

    convertBlocks <- function(x) {
      # reduce number of blocks by substituting blocks which
      # only contain one other block
      reduce <- grep("^ *#[^v^#]*# *$", x)
      for (r in reduce) {
        id <- gsub(" ", "", x[r])
        x[r] <- x[id]
        x[id] <- ".:|DELETEME|:."
      }
      x <- x[x != ".:|DELETEME|:."]
      names <- names(x)
      # handle exponents
      x <- gsub("\\*\\*([^<>=*+/-]+)", "^{\\1}", x)
      # handle divisions
      steps <- 1
      while (any(grepl("/", x)) || steps > 10) {
        x <- gsub("([^<>=*+/-]+)/([^<>=*+/-]+)", "\\\\frac{\\1}{\\2}", x) # nolint absolute_path_linter
        steps <- steps + 1
      }
      # handle multiplications
      x <- gsub("*", " \\cdot ", x, fixed = TRUE)
      # add braces back, if required
      # required, if
      # 1) id starts with a "b" (for braket)
      # 2) block contains more than just a single variable/block
      add <- (substr(names(x), 2, 2) == "b")
      add <- add & !grepl("^ *#[^#]*# *$", x)
      x[add] <- paste0("\\left(", x[add], "\\right)")
      names(x) <- names
      return(x)
    }

    convertVars <- function(var, eqMap) {

      if (is.null(eqMap)) {
        return(var)
      }

      for (i in names(var)) {

        # split elements into parts only existing of word character or backslash,
        # as this is the expected granularity in the equation mapping
        parts <- stri_extract_all_regex(var[i], "[\\w\\\\]{1,}")[[1]]
        for (p in parts) {
          if (p %in% eqMap$variable) {
            var[i] <- gsub(
              gsub("\\_", "\\\\_", p),
              eqMap[eqMap$variable == p, "symbol"],
              var[i]
            )
          }
        }
      }
      return(var)
    }

    mergeBack <- function(x) {
      vars <- x[-1]
      x <- x[1]
      for (i in names(vars)) x <- sub(i, vars[i], x, fixed = TRUE)
      return(x)
    }

    variable <- "[\\w.]{1,}(\\([\\w,\"'+-]*\\)|)"
    y <- extractVars(x, variable, "v")

    z <- extractBraceblocks(y$x)

    z <- convertFunctions(z)
    if (is.null(z)) return("#FAILED#")
    z <- convertBlocks(z)

    y$vars <- convertVars(y$vars, eqMap)

    x <- mergeBack(c(z, y$vars))

    return(x)
  }

  # remove comments at the end of the line
  x <- gsub(" *!![^\n]*(\n|$)", "\\1", x)

  # split name and equation
  pattern <- "^\n*(.*?) *\\.\\. *(.*?);?$"
  eqMap <- NULL

  if (grepl(pattern, x)) {
    name <- stri_replace_all_fixed(sub(pattern, "\\1", x), " ", "")

    # TODO: make the path a variable
    mapPath <- file.path("doc", "latexVariables", paste0(sub("\\(.*\\)", "", name), ".csv"))

    if (file.exists(mapPath)) {
      eqMap <- read.csv2(mapPath, sep = ",", header = FALSE, col.names = c("variable", "symbol"))

      # escape underscore in existing variable names
      eqMap[, "variable"] <- gsub("\\_", "\\\\_", eqMap$variable)

      # escape backslash in symbols
      eqMap[, "symbol"] <- gsub("\\\\", "\\\\\\\\", eqMap$symbol)
    }

    eq <- sub(pattern, "\\2", x)
  } else  {
    name <- "undefined"
    eq <- x
  }

  if (grepl("(^\\$|\n\\$)", x)) {
    return(unconverted(x, name, "Cannot handle equations with preceeding dollar conditions! Return original code!"))
  }

  # split sides
  pattern <- "^(.*)(=[lLgGeEnN]=)(.*)$"
  if (!grepl(pattern, eq)) {
    return(unconverted(x, name, "Cannot handle equations without relational operator! Return original code!"))
  }
  left <- sub(pattern, "\\1", eq)
  middle <- sub(pattern, "\\2", eq)
  right <- sub(pattern, "\\3", eq)

  middle <- switch(tolower(middle),
                   "=e=" = "=",
                   "=l=" = "\\leq",
                   "=g=" = "\\geq",
                   "=n=" = "\\neq")

  left <- convertSide(left, eqMap)
  right <- convertSide(right, eqMap)

  out <- paste(left, middle, right)
  out <- gsub(" +", " ", out)
  out <- gsub("([$%])", "\\\\\\1", out)
  out <- gsub(">=", "\\geq", out, fixed = TRUE)
  out <- gsub("<=", "\\leq", out, fixed = TRUE)

  out <- paste("\\begin{multline*}\n", out, "\n\\end{multline*}")
  out <- gsub("\t", " ", out)
  out <- gsub("\n[\n ]*\n", "\n", out)

  if (!is.null(eqMap)) {

    eqMap[, "symbol"] <- paste0("\\\\[", eqMap[, "symbol"], "\\\\]")
    t <- pandoc.table.return(eqMap)
    out <- paste0(out, "\n", t)
  }

  names(out) <- name

  if (grepl("#", out)) {
    return(unconverted(x, name, paste0("Equation ", name, " could not be converted! Return original code!")))
  }

  return(out)
}
