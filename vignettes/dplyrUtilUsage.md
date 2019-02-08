<!-- 
README.md is generated from README.Rmd. Please edit that file
rmarkdown::render(
  "dplyrUtilUsage.Rmd", output_format = rmarkdown::md_document()) 
-->
Common tasks when using dplyr
=============================

Introduction
------------

The [tidyverse](https://r4ds.had.co.nz/transform.html) with the dplyr
package in combination with purrr mapping and tidyr data.frames is
agreat way of managing data in R.

Using the package, however, require repeated coding for some common
issues. This package provides routines for some of those issues.

    require(dplyr, quietly = TRUE)
    require(dplyrUtil, quietly = TRUE)

split-map-combine
-----------------

One common operation is the [split-map-combine](TODO) pattern A tbl,
i.e. tabular data such as a data.frame or a tibble, is subset, then for
each subset a function is called that returns a tbl, and finally the
resulting tbls are row-bind again.

While the tidyverse favours the [group\_by-nest-map-unnest](TODO) idiom
of this pattern, the resulting code is hard to understand for people not
familiar with this idiom. Moreover, the nested data.frames does not
contain the grouping columns which are sometimes required by the
function working on the subset.

`dplyUtil` provides function `mapGroups` that provides a dplyr like call
interface for the base split function.

The following example applies a function on each kind of food subset
that uses the number of records in the subset and the grouping variable.

    ds <- tibble(
      food = c("banana","strawberry","pear","bread","corn flakes")
      , kind = factor(c(rep("fruit",3), rep("cereal",2)))
      )
    ans <- ds %>% group_by(kind) %>% mapGroups(function(dss){
      dss %>% mutate(
        countInKind = paste(first(kind),1:n())
      )
    })
    ans
    #> # A tibble: 5 x 3
    #> # Groups:   kind [2]
    #>   food        kind   countInKind
    #>   <chr>       <fct>  <chr>      
    #> 1 bread       cereal cereal 1   
    #> 2 corn flakes cereal cereal 2   
    #> 3 banana      fruit  fruit 1    
    #> 4 strawberry  fruit  fruit 2    
    #> 5 pear        fruit  fruit 3

If the data.frames to combine hold inconsistent factor levels, by
default a warning is shown and the column is converted to character. If
instead, one wants to combine all factor levels, there is argument
`.isFactorReleveled`

    ans <- ds %>% group_by(kind) %>% mapGroups(function(dss){
      dss %>% mutate(
        countInKind = factor(paste(first(kind),1:n()))
      )
    }
    , .isFactorReleveled = TRUE
    , .noWarningCols = c("countInKind"))
    ans
    #> # A tibble: 5 x 3
    #> # Groups:   kind [2]
    #>   food        kind   countInKind
    #>   <chr>       <fct>  <fct>      
    #> 1 bread       cereal cereal 1   
    #> 2 corn flakes cereal cereal 2   
    #> 3 banana      fruit  fruit 1    
    #> 4 strawberry  fruit  fruit 2    
    #> 5 pear        fruit  fruit 3

mutate acting only on the rows satisfying the condition
-------------------------------------------------------

Default `mutate` acts on all rows. For each modified variable one needs
to use `ifelse`. Function `mutate_cond` helps in those cases.

The following example modifies the Petal.Length column, but only for a
given Species.

    ans <- iris %>% as_tibble() %>% 
      mutate_cond(
        Species == "setosa"
        , Petal.Length = 1.0
      )
    rbind(
    ans %>% filter(Species == "setosa") %>% 
      select(Species, Petal.Length) %>% head(3) # 1.0
    , ans %>% filter(Species != "setosa") %>% 
      select(Species, Petal.Length) %>% head(3) # orig
    ) 
    #> # A tibble: 6 x 2
    #>   Species    Petal.Length
    #>   <fct>             <dbl>
    #> 1 setosa              1  
    #> 2 setosa              1  
    #> 3 setosa              1  
    #> 4 versicolor          4.7
    #> 5 versicolor          4.5
    #> 6 versicolor          4.9

Joining with Replacement
------------------------

A join is often used to add new columns to a dataset. If the same
information is joined several times, by default the column is creted
with a modified name. If one instead wants to replace the joined column,
function `left_joinReplace` helps with that.

The following example joines the info column again, because it was
modified after the first join. It demonstrates the different results
between `left_join` and `left_joinReplace`.

    kindInfo <- tibble(
      kind = factor(c("fruit","cereal"))
      ,info = c("rich in vitamins","rich in carbohydrates")
    )
    ans1 <- ds %>% left_join(kindInfo, by = "kind")
    kindInfo$info[1] <- "juicy"
    ans1 %>% left_join(kindInfo, by = "kind") # duplicated info column
    #> # A tibble: 5 x 4
    #>   food        kind   info.x                info.y               
    #>   <chr>       <fct>  <chr>                 <chr>                
    #> 1 banana      fruit  rich in vitamins      juicy                
    #> 2 strawberry  fruit  rich in vitamins      juicy                
    #> 3 pear        fruit  rich in vitamins      juicy                
    #> 4 bread       cereal rich in carbohydrates rich in carbohydrates
    #> 5 corn flakes cereal rich in carbohydrates rich in carbohydrates
    ans1 %>% left_joinReplace(kindInfo, by = "kind") # modified info column
    #> # A tibble: 5 x 3
    #>   food        kind   info                 
    #>   <chr>       <fct>  <chr>                
    #> 1 banana      fruit  juicy                
    #> 2 strawberry  fruit  juicy                
    #> 3 pear        fruit  juicy                
    #> 4 bread       cereal rich in carbohydrates
    #> 5 corn flakes cereal rich in carbohydrates

Dealing with unequal factor levels
----------------------------------

In case of unequal factor levels in row-binding tbls or joins, factors
are converted to character. Often one wants to combine the factor levels
instead. Function `expandAllInconsistentFactorLevels` helps with that.

In the following example, the levels of factor `f` do not match.
Therefore, `bind_rows` warns and coerces to character. With expanding
the factors before, the combined data.frame factor `f` is still a factor
but with modified levels.

    df1 <- tibble(f = factor(c("D","D","C")))
    df2 <- tibble(f = factor(c("C","C","A"))
                      , desc = c("forC1","forC2","forA1"))
    bind_rows(df1,df2) # f is converted to character
    #> Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character
    #> Warning in bind_rows_(x, .id): binding character and factor vector,
    #> coercing into character vector

    #> Warning in bind_rows_(x, .id): binding character and factor vector,
    #> coercing into character vector
    #> # A tibble: 6 x 2
    #>   f     desc 
    #>   <chr> <chr>
    #> 1 D     <NA> 
    #> 2 D     <NA> 
    #> 3 C     <NA> 
    #> 4 C     forC1
    #> 5 C     forC2
    #> 6 A     forA1
    expandAllInconsistentFactorLevels(df1,df2) %>% bind_rows()
    #> Warning in expandAllInconsistentFactorLevels(df1, df2): releveling factors
    #> f
    #> # A tibble: 6 x 2
    #>   f     desc 
    #>   <fct> <chr>
    #> 1 D     <NA> 
    #> 2 D     <NA> 
    #> 3 C     <NA> 
    #> 4 C     forC1
    #> 5 C     forC2
    #> 6 A     forA1

Function `expandFactorLevels` helps to relevel a specific column across
several tbls.

Both functions proviede the result as a list of tbls. This type of
result can be handled by `bind_rows`, but other functions require single
data.frames, i.e. the components of the result (`[[1]]`, `[[2]]`).

For convenience function `left_joinFactors` helps joining tbls with
different factor levels.

    left_joinFactors(df1,df2, by = "f", .noWarningCols = c("f"))
    #> # A tibble: 4 x 2
    #>   f     desc 
    #>   <fct> <chr>
    #> 1 D     <NA> 
    #> 2 D     <NA> 
    #> 3 C     forC1
    #> 4 C     forC2
