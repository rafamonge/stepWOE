# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' @export
woeTransformDefault = function(x){
  if(is.factor(x)){
    as.numeric(x) -1
  }
  else{
    as.numeric(x)
  }

}

#' Centering Numeric Data
#'
#' `step_WOE` creates a *specification* of a recipe
#'  step that will return WOES for factor variables.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param woes An object returned by the Information package.
#' @param outcome Specifes the name of the column to use as the outcome when calculatint the WOEs
#' @param outcomeTransformtion a function to process the outcome variable before passing it to the Information pacakge
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the means).
#'
#' @keywords datagen
#' @concept preprocessing WOEs IV
#' @export
#' @examples
#' rec = recipe(Status ~ Seniority + Marital + Records + Job, data = credit_data)
#' rec = rec %>% step_WOE(Marital,Records,Job,outcome="Status")
#' rec =rec %>% prep(training=credit_data)
#' bake(rec, newdata = credit_data)
#'
#' @seealso [recipe()] [prep.recipe()]
#'   [bake.recipe()]


step_WOE <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           woes = NULL,
           outcome = outcome,
           skip = FALSE,
           id = rand_id("WOE")) {
    add_step(
      recipe,
      step_WOE_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        woes = woes,
        outcome = outcome,
        outcomeTransform = woeTransformDefault,
        skip = skip,
        id = id
      )
    )
  }


# Initializes a new object
#' @export
step_WOE_new <-
  function(terms, role, trained, woes,outcome,outcomeTransform, skip, id) {
    step(
      subclass = "WOE",
      terms = terms,
      role = role,
      trained = trained,
      woes = woes,
      outcome = outcome,
      outcomeTransform = outcomeTransform,
      skip = skip,
      id = id
    )
  }


#' @export
prep.step_WOE <- function(x, training, info = NULL, ...) {

  x_names <- recipes::terms_select(x$terms, info = info)
  y_names <- recipes::terms_select(x$outcome, info = info)
  col_names <- c(x_names, y_names)
  numberOfFactors = sapply(training %>% select(!!x_names), is.factor)
  if(sum(numberOfFactors) != length(numberOfFactors)){
    stop("All terms must be factors. If you require calculating woes for numeric variables discretize them before this step")
  }

  #print(quo(x$outcome))
  #print(x$outcomeTransform)
  tmp = training %>%
    select(!!col_names) %>%
    rename(tempWoeOutcome := x$outcome) %>%
    mutate(tempWoeOutcome= sapply(tempWoeOutcome,x$outcomeTransform))
  #check_type(training[, col_names], )

  woes <- create_infotables(tmp,y="tempWoeOutcome",parallel=FALSE)
  step_WOE_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    woes = woes,
    outcome=x$outcome,
    outcomeTransform=x$outcomeTransform,
    skip = x$skip,
    id = x$id
  )
}
#' @export
bake.step_WOE <- function(object, newdata, ...) {
  #print("BAking!")
  for(val in names(object$woes$Tables)){
    print(val)
    woeTable = object$woes$Tables[val][[1]] %>% as.tibble() %>% select_at(c(val, "WOE"))
    newdata = newdata %>%
      left_join(woeTable, by=val) %>%
      mutate((!!(val)) :=WOE) %>%
      select(-WOE)
  }
  newdata


}



#' @export
print.step_WOE <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("WOE for ", sep = "")
    printer(names(x$woes), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_WOE
#' @param x A `step_WOE` object.
#' @export
tidy.step_WOE <- function(x, ...) {
  if (is_trained(x)) {
    res <- map2_df( rec$steps[[1]]$woes$Tables, names(rec$steps[[1]]$woes$Tables), function(x, y){
      colnames(x)[1] ="Value"
      x["Term"] = y
      x %>% select(Term,everything())
    })
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(Term = term_names,
                  Value = NA,
                  N = NA,
                  Percent = NA,
                  WOE = NA,
                  IV = NA
    )
  }
  res$id <- x$id
  res
}

rand_id <- function(prefix = "step", len = 5) {
  candidates <- c(letters, LETTERS, paste(0:9))
  paste(prefix,
        paste0(sample(candidates, len, replace = TRUE), collapse = ""),
        sep = "_"
  )
}


ellipse_check <- function(...) {
  terms <- quos(...)
  if (is_empty(terms))
    stop("Please supply at least one variable specification.",
         "See ?selections.",
         call. = FALSE)
  terms
}
