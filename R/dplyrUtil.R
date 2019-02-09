#' @importFrom rlang sym syms UQ :=
NULL

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
  , FUN  ##<< function(data.frmae, ...) -> data.frame to apply to subsets
  , ...  ##<< further arguments to FUN
  , drop = TRUE  ##<< logical indicating if levels that do not occur should 
  ## be dropped. Set to FALSE if FUN returns a data.frame also 
  ## for zero-row inputs.
  , .isFactorReleveled = FALSE
){
  # https://coolbutuseless.bitbucket.io/2018/03/03/split-apply-combine-my-search-for-a-replacement-for-group_by---do/
  groupVars <- group_vars(data)
  if (!length(groupVars)) return(FUN(data,...))
  data %>% 
    split(group_indices(.)) %>% 
    #split(select(.,groupVars), drop = drop) %>% 
    map_dfrFactor(FUN,...,.isFactorReleveled = .isFactorReleveled)
}

#' @importFrom purrr as_mapper map
#' @importFrom dplyr bind_rows
#' @export
map_dfrFactor <- function(
  ### variant of map_dfr that releveles unequal factor levels before binding
  .x                ##<< list to map over
  , .f              ##<< function to apply
  , ...             ##<< further arguments to \code{purrr::map}
  , .id = NULL      ##<< argument to \code{\link{bind_rows}}
  , .isFactorReleveled = TRUE ##<< set to FALSE to avoid releveling
  , .noWarningCols = character(0)  ##<< argument 
  ## to \code{\link{expandAllInconsistentFactorLevels}}
  
) {
  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...) 
  if (isTRUE(.isFactorReleveled))
    res <- res %>% expandAllInconsistentFactorLevels(
      .noWarningCols = .noWarningCols) 
  bind_rows(res, .id = .id)
}


#' @export
mutate_cond <- function(
  ###  mutate acting only on the rows satisfying the condition
  .data           ##<< data.frame to be modified
  , condition     ##<< condition for selecting rows to be modified
  , ...           ##<< further arguments to \code{\link{mutate}}
  , envir = parent.frame()  ##<< the frame where the condition is evaluated
) {
  # credits to G. Grothendieck
  # https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows 
  ##details<< condition is evaluated in .data inside the parent frame.
  ## Hence column names can be used. 
  ## \code{.data} refers to the subset group of the data.frame processed. 
  backupData <- envir$.data
  on.exit(envir$.data <- backupData)
  condSubst <- substitute(condition) # call to be evaluated in data.frame
  .data %>% mapGroups(function(dss){
    envir$.data <- dss
    isCond <- eval(condSubst, dss, envir)
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
) {
  dots <- list(...)
  datasets <- if (length(dots) == 1) dots[[1]] else dots
  colsToCheck <- intersect(names(datasets[[1]]),names(datasets[[2]]))
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
  for (col in colNamesInc) datasets <- expandFactorLevels(datasets, col)
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
) {
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
){
  dfs <- expandAllInconsistentFactorLevels(
    x,y, .noWarningCols = .noWarningCols)
  ##value<< results of \code{\link{left_join}} or given alternative join function
  fJoin(dfs[[1]], dfs[[2]], ...)
}

