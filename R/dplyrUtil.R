#' @importFrom rlang sym syms UQ :=
TRUE

#' @importFrom dplyr left_join select one_of %>%
#' @export
left_joinReplace <- function(
  ### \code{\link{left_join}} with before dropping columns that would be duplicated
  x,y   ##<< data.frames to join
  , by  ##<< here must be a character vector
  , ... ##<< further arguments to \code{\link{left_join}}
){
  ##details<< During joing existing columns are duplicated with a
  ## different name.
  ## This function supports replacing the original columns instead,
  ## allowing for repeated join of similar data.
  ##
  ## If x is a list of 2 data.frames then these will be assigned to x and y
  if (length(x) == 2 && is.data.frame(x[[1]])){
    y <- x[[2]]
    x <- x[[1]]
  }
  # columns that will be created and need to be dropped from x before
  addCols <- setdiff(names(y),by)
  delCols <- intersect(names(x), addCols)
  xD <- if (length(delCols)) select(x, -one_of(delCols)) else x
  ##value<< result of \code{\link{left_join}}
  left_join(xD, y, by = by, ...)
}

#' @export
#' @importFrom dplyr %>% group_vars select group_indices
#' @importFrom purrr map_dfr
mapGroups <- function(
  ### split-map-combine
  data  ##<< groped data.frame
  , ...  ##<< function or formula passed to \code{\link{map}}
  ## that receives a data.frame as first argument
  , drop = TRUE  ##<< logical indicating if levels that do not occur should
  ## be dropped. Set to FALSE if FUN returns a data.frame also
  ## for zero-row inputs.
  , .isFactorReleveled = FALSE
  , .omitWarning = FALSE
){
  if( !isTRUE(.omitWarning)) warning(
    'expecting calls to mapGroups replaced by ',
    'ds %>% split(group_indices(.)) %>% map_dfr(...). (see ?mapGroups)')
  ##details<<
  ## https://stackoverflow.com/questions/53855897/accessing-grouping-variables-in-purrrmap-with-nested-dataframes
  ## https://coolbutuseless.bitbucket.io/2018/03/03/split-apply-combine-my-search-for-a-replacement-for-group_by---do/
  ## if you create factors inside map_df, one can use map_dfrFactor.
  groupVars <- group_vars(data)
  if (!length(groupVars)) {
    #ans <- list(data) %>% map(...) %>% "[["(1)
    ans <- data %>% as_mapper(...)()
    return(ans)
  }
  data %>%
    split(group_indices(.)) %>%
    #split(select(.,groupVars), drop = drop) %>%
    map_dfrFactor(..., .isFactorReleveled = .isFactorReleveled,
                  .omitWarning = .omitWarning)
}

#' @importFrom purrr as_mapper map
#' @importFrom dplyr bind_rows
#' @export
map_dfrFactor <- function(
  ### variant of map_dfr that releveles unequal factor levels before binding
  .x                ##<< list to map over
  , ...             ##<< further arguments to \code{purrr::map}
  , .id = NULL      ##<< argument to \code{\link{bind_rows}}
  , .isFactorReleveled = TRUE ##<< set to FALSE to avoid releveling
  , .noWarningCols = character(0)  ##<< argument
  ## to \code{\link{expandAllInconsistentFactorLevels}}
  , .omitWarning = FALSE
){
  if( !isTRUE(.omitWarning)) warning(
    'expected calls to map_dfrFactor to be obsolete ',
    'with recent versions of bind_rows and joins in dplyr')
  res <- map(.x, ...)
  if (isTRUE(.isFactorReleveled))
    res <- res %>% expandAllInconsistentFactorLevels(
      .noWarningCols = .noWarningCols, .omitWarning = TRUE)
  bind_rows(res, .id = .id)
}


#' @export
mutate_cond <- function(
  ###  mutate acting only on the rows satisfying the condition
  .data           ##<< data.frame to be modified
  , condition     ##<< condition for selecting rows to be modified
  , ...           ##<< further arguments to \code{\link{mutate}}
  , envir = parent.frame()  ##<< the frame where the condition is evaluated
  , na.rmCond = FALSE ##<< set to TRUE to set NA values in cond to FALSE.
  ## By default an error is thrown
) {
  # credits to G. Grothendieck
  # https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
  ##details<< condition is evaluated in .data inside the parent frame.
  ## Hence column names can be used.
  ## \code{.data} refers to the subset group of the data.frame processed.
  backupData <- envir$.data
  on.exit(envir$.data <- backupData)
  condSubst <- substitute(condition) # call to be evaluated in data.frame
  #.data %>% mapGroups(function(dss){
  .data %>% split(group_indices(.)) %>% map_dfr(function(dss){
    envir$.data <- dss
    isCond <- eval(condSubst, dss, envir)
    if (isTRUE(na.rmCond)) isCond[is.na(isCond)] <- FALSE
    dss[isCond, ] <- dss[isCond, ] %>% mutate(...)
    dss
  })
}
attr(mutate_cond,"ex") <- function(){
  if (require(dplyr)) {
    ans <- iris %>%
      mutate_cond(
        Species == "setosa"
        , Petal.Length = 1.0
      )
    ans %>% filter(Species == "setosa") %>% select(Petal.Length) %>% head() # 1.0
    ans %>% filter(Species != "setosa") %>% select(Petal.Length) %>% head() # orig
  }
}

#----------- expand factor levels ------------
# exported from equidIO

#' @importFrom purrr map map_dbl map_lgl map_dfr
#' @importFrom dplyr mutate
#' @importFrom forcats lvls_union
#' @export
expandAllInconsistentFactorLevels <- function(
  ### expand a factor variables in all dataset to encompass levels of all sets
  ...  ##<< list of data.frames or several data.frames separated by comma
  , .noWarningCols = character(0)  ##<< string vector: do not warn for the these
  ## columns
  , .omitWarning = FALSE
){
  if( !isTRUE(.omitWarning)) warning(
    'expected calls to expandAllInconsistentFactorLevels to be obsolete ',
    'with recent versions of bind_rows and joins in dplyr')
  dots <- list(...)
  ##details<< If no data.frame is provided, its returns an empty list.
  if (!length(dots)) return(list())
  ##details<< Instead of providing several arguments, one can provide a
  ## a single list of data.frames
  datasets <- if (length(dots) == 1 && !is.data.frame(dots[[1]])) dots[[1]] else dots
  # if provided a single data.frame return list with single data.frame
  if (length(datasets) == 1) return(datasets)
  if (!length(datasets)) return(list())
  commonCols <- intersect(names(datasets[[1]]),names(datasets[[2]]))
  colsToCheck <- commonCols[map_lgl(commonCols, function(
    colToCheck){is.factor(datasets[[1]][[colToCheck]])})]
  if (!length(colsToCheck)) return(datasets)
  # col <- colsToCheck[1]
  isInconsistentFactor <- sapply( colsToCheck,  function(col){
    any(map_lgl(
      datasets, ~is.factor(.[[col]]))) &&
      any(map_lgl(
        datasets, ~!identical(levels(.[[col]]), levels(datasets[[1]][[col]]))))
  })
  colNamesInc <- colsToCheck[isInconsistentFactor]
  colNamesWarn <- setdiff(colNamesInc, .noWarningCols)
  if (length(colNamesWarn)) warning(
    "releveling factors ", paste(colNamesWarn , collapse = ","))
  for (col in colNamesInc) datasets <- expandFactorLevels(
    datasets, col, .omitWarning = TRUE)
  ##value<< \code{datasets} with updated factor columns
  datasets
}
attr(expandAllInconsistentFactorLevels,"ex") <- function(){
  if (exists("expandAllInconsistentFactorLevels")) {
    df1 <- data.frame(f = factor(c("D","D","C")))
    df2 <- data.frame(f = factor(c("C","C","A"))
                      , desc = c("forC1","forC2","forA1"))
    if (requireNamespace("dplyr"))
      dplyr::bind_rows(expandAllInconsistentFactorLevels(df1,df2))
    left_joinFactors(df1,df2)
  }
}

#' @export
expandFactorLevels <- function(
  ### expand a factor in all dataset to encompass levels of all sets
  datasets    ##<< list of data.frames
  , varName   ##<< scalar string of variable holding the factor
  , .omitWarning = FALSE
){
  if( !isTRUE(.omitWarning)) warning(
    'expected calls to expandFactorLevels to be obsolete ',
    'with recent versions of bind_rows and joins in dplyr')
  #https://stackoverflow.com/questions/46876312/how-to-merge-factors-when-binding-two-dataframes-together/50503461#50503461
  groupLevels <- lvls_union(lapply(datasets, "[[", varName))
  force(varName)
  ans <- map(datasets, function(dss){
    #mutate(dss, !!varName := fct_expand(!!sym(varName), groupLevels))
    mutate(dss, !!varName := factor(!!sym(varName), levels = groupLevels))
  })
  ##value<< list of datasets with each entries column releveled
  ans
}

#' @export
left_joinFactors <- function(
  ### left join with homogenizing factors before
  x     ##<< left tbl to join
  , y   ##<< right tbl to join
  , ... ##<< further arguments to \code{\link{left_join}} or \code{fJoin}
  , fJoin = left_join  ##<< join function(x,y,...) to use
  , .noWarningCols = character(0)  ##<< string vector: do not warn for the these
  ## columns
  , .omitWarning = FALSE
){
  if( !isTRUE(.omitWarning)) warning(
    'expected calls to left_joinFactors to be obsolete ',
    'with recent versions of bind_rows and joins in dplyr')
  dfs <- expandAllInconsistentFactorLevels(
    x,y, .noWarningCols = .noWarningCols, .omitWarning = TRUE)
  ##value<< results of \code{\link{left_join}} or given alternative join function
  fJoin(dfs[[1]], dfs[[2]], ...)
}

