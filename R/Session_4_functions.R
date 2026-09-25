library(dplyr)

run_test_on_groups <-
  function(x,
           group_var,
           test_var,
           test_f,
           margin = NULL,
           ...) {

    if(!is.data.frame(x)) {

      if(length(intersect(class(x),
                          c("table","matrix","array"))
               ) == 0) {
        stop("Incompatible class(x) !")
      }

      if(margin == "rows") margin <- 1
      if(margin == "cols") margin <- 2

      x <- x |> apply(margin,c,simplify=FALSE) |> utils::stack()
      colnames(x) <- c(test_var, group_var)

    # If it IS a data.frame or tibble,
    # ASSUME(!) that it is already in stacked form!
    }

    x |>
    summarize( ( {{ test_f }}(x= !!rlang::sym(test_var)) |> broom::tidy()),
              .by= !!rlang::sym(group_var)
             ) |> mutate(test_var = test_var) |> data.frame() #tibble::tibble()

  }


run_levene <-
    function(x,group_var,test_var) {
        char_f <- paste(test_var,"~",group_var)

        car::leveneTest(y = eval(as.formula(char_f)),
                        data = x) |>
        broom::tidy() |>
        mutate(group_var = group_var) |>
        mutate(test_var = test_var)
    }


run_formula_test <-
    function(x,group_var,test_var, ndigits=1, test_f) {
        char_f <- paste(test_var,"~",group_var)
        
        test_f(eval(as.formula(char_f)),
                        data = x)  |>
        broom::tidy() |>
        mutate(group_var = group_var) |>
        mutate(test_var = test_var) |>
        mutate(p.value = formatC(p.value, 
                                 format = "e", 
                                 digits = ndigits)
               )
    }
     




