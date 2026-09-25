## ----QRcode, echo=FALSE, fig.width=2, fig.height=2, fig.align='left'----------
#fig.width=2, fig.height=2,
#plot(qrcode::qr_code("https://cecadbioinformaticscorefacility.github.io/Intermediate_R_Course_2025/"))


## -----------------------------------------------------------------------------
source("R/functions.R")


## ----categorical_data_example_matrix_1----------------------------------------
#| echo: TRUE
#| eval: TRUE
#| output-location: column

mouse_data <- readRDS("mouse_data.rds")

# Have look:
mouse_data



## ----categorical_data_example_matrix_2----------------------------------------
#| echo: TRUE
#| eval: TRUE
#| output-location: column

genotype_frequencies <- table(mouse_data[,1])
genotype_frequencies


## ----categorical_data_example_matrix_3----------------------------------------
#| echo: TRUE
#| eval: TRUE
#| output-location: column

symptom_frequencies <- table(mouse_data[,2])
symptom_frequencies


## ----categorical_data_example_matrix_4----------------------------------------
#| echo: TRUE
#| eval: TRUE
#| output-location: column

genotype_symptoms_crossed <-
  table(mouse_data[,1],mouse_data[,2])

genotype_symptoms_crossed



## ----categorical_data_example_matrix_5----------------------------------------
#| echo: TRUE
#| eval: TRUE
#| output-location: column

# If produced directly from the dataframe, 
# the variable names appear in the output:
table(mouse_data)



## ----echo=FALSE, results='asis'-----------------------------------------------
## write the HTML file .. 
tools:::Rd2HTML(utils:::.getHelpFile(help(chisq.test)), 
                out = "chisq.test_help.html", stylesheet = "")

## .. and render it  below:


## ----csq1.1-------------------------------------------------------------------
#| echo: TRUE
#| output-location: column

# Test the white genotype's frequencies against uniform
chisq.test(x = table(mouse_data)[1,])


## ----csq1.2-------------------------------------------------------------------
#| echo: TRUE
#| output-location: column

# Test the black genotype's frequencies against uniform
chisq.test(x = table(mouse_data)[2,])



## ----csq2.1-------------------------------------------------------------------
#| echo: TRUE
#| output-location: column

#| test white against an arbitrary distribution
chisq.test(x = table(mouse_data)[1,],
           p = c(0.1,0.2,0.3,0.4))





## ----csq2.2-------------------------------------------------------------------
#| echo: TRUE
#| output-location: column

#| test black against an arbitrary distribution
chisq.test(x = table(mouse_data)[2,],
           p = c(0.1,0.2,0.3,0.4))





## ----csq4---------------------------------------------------------------------
#| echo: true
#| warning: true
#| output-location: column

# Are the black genotype's frequencies 
# compatible with the white's probabilities?
tbl <- table(mouse_data)
chisq.test(x = tbl[2,], p = tbl[1,]/sum(tbl[1,])) 



## ----csq3.2-------------------------------------------------------------------
#| echo: TRUE
#| output-location: column

chisq.test(x = mouse_data[,1],
           y = mouse_data[,2])


## ----echo=FALSE, results='asis'-----------------------------------------------
## write the HTML file .. 
tools:::Rd2HTML(utils:::.getHelpFile(help(fisher.test)), 
                out = "fisher.test_help.html", stylesheet = "")

## .. and render it  below:


## ----fisher-------------------------------------------------------------------
#| echo: TRUE

fisher.test(x=mouse_data[,1], y=mouse_data[,2])

# As with the chisq.test, one can get the same result
# by specifying the pre-computed contingency table:
fisher.test(x=table(mouse_data))



## ----set_width----------------------------------------------------------------
#| echo: FALSE
#| eval: TRUE

options(width=150)



## ----iris02-------------------------------------------------------------------
#| echo: TRUE

data(iris)
head(iris)

summary(iris)


## ----set_transparency---------------------------------------------------------

#| echo: FALSE  

library(Cairo)

#Sys.setenv(bitmapType="cairo")
options(bitmapType="cairo")

source("R/functions.R")



## ----normality_qq_SL----------------------------------------------------------
#| echo: true
#| output-location: column

use_trait <- "Sepal.Length"
ggpubr::ggqqplot(iris, 
                 use_trait,
                 facet.by="Species",
                 ylab = use_trait
                )


## ----prep_normality_shapiro_SL------------------------------------------------
#| echo: FALSE
#| eval: TRUE
shapiroSL<- 
  run_test_on_groups(iris,
                     "Species",
                     use_trait,
                     shapiro.test)



## ----echo_normality_shapiro_SL------------------------------------------------
#| echo: TRUE
#| eval: FALSE

# # This function  is defined in R/functions.R
# run_test_on_groups(iris,
#                   "Species",
#                   use_trait,
#                   shapiro.test)
# 
# 


## ----print_normality_shapiro_SL-----------------------------------------------
#| echo: FALSE
#| eval: TRUE

print(shapiroSL)



## ----normality_qq_SW----------------------------------------------------------
#| echo: true
#| output-location: column

use_trait <- "Sepal.Width"
ggpubr::ggqqplot(iris, 
                 use_trait,
                 facet.by="Species",
                 ylab = use_trait
                )


## ----prep_normality_shapiro_SW------------------------------------------------
#| echo: FALSE
#| eval: TRUE
shapiroSW<- 
  run_test_on_groups(iris,
                     "Species",
                     use_trait,
                     shapiro.test)



## ----echo_normality_shapiro_SW------------------------------------------------
#| echo: TRUE
#| eval: FALSE

# # This function  is defined in R/functions.R
# run_test_on_groups(iris,
#                   "Species",
#                   use_trait,
#                   shapiro.test)
# 
# 


## ----print_normality_shapiro_SW-----------------------------------------------
#| echo: FALSE
#| eval: TRUE

print(shapiroSW)



## ----normality_qq_PL----------------------------------------------------------
#| echo: true
#| output-location: column

use_trait <- "Petal.Length"
ggpubr::ggqqplot(iris, 
                 use_trait,
                 facet.by="Species",
                 ylab = use_trait
                )


## ----prep_normality_shapiro_PL------------------------------------------------
#| echo: FALSE
#| eval: TRUE
shapiroPL<- 
  run_test_on_groups(iris,
                     "Species",
                     use_trait,
                     shapiro.test)



## ----echo_normality_shapiro_PL------------------------------------------------
#| echo: TRUE
#| eval: FALSE

# # This function  is defined in R/functions.R
# run_test_on_groups(iris,
#                   "Species",
#                   use_trait,
#                   shapiro.test)
# 
# 


## ----print_normality_shapiro_PL-----------------------------------------------
#| echo: FALSE
#| eval: TRUE

print(shapiroPL)



## ----normality_qq_PW----------------------------------------------------------
#| echo: true
#| output-location: column

use_trait <- "Petal.Width"
ggpubr::ggqqplot(iris, 
                 use_trait,
                 facet.by="Species",
                 ylab = use_trait
                )


## ----prep_normality_shapiro_PW------------------------------------------------
#| echo: FALSE
#| eval: TRUE
shapiroPW<- 
  run_test_on_groups(iris,
                     "Species",
                     use_trait,
                     shapiro.test)



## ----echo_normality_shapiro_PW------------------------------------------------
#| echo: TRUE
#| eval: FALSE

# # This function  is defined in R/functions.R
# run_test_on_groups(iris,
#                   "Species",
#                   use_trait,
#                   shapiro.test)
# 
# 


## ----print_normality_shapiro_PW-----------------------------------------------
#| echo: FALSE
#| eval: TRUE

print(shapiroPW)



## ----prep_equal_variance------------------------------------------------------
#| echo: TRUE

# run_levene() is defined in R/functions.R
purrr::map(colnames(iris)[-5],
           \(x) run_levene(iris,"Species",x)
) |> bind_rows()


## ----echo=FALSE, results='asis'-----------------------------------------------
## write the HTML file .. 
tools:::Rd2HTML(utils:::.getHelpFile(help(t.test)), 
                out = "t.test_help.html", stylesheet = "")

## .. and render it  below:


## ----one_sample_t_1-----------------------------------------------------------
#| echo: TRUE
#| output-location: column

# Is Sepal.Width > 2 in all species jointly?
t.test(x  = iris$Sepal.Width,
       mu = 2)


## ----one_sample_t_2-----------------------------------------------------------
#| echo: TRUE
#| output-location: column

s <- "virginica"
t.test(x  = iris$Sepal.Width[ iris$Species == s ],
       mu = 2) # Can you express it in dplyr style?


## ----two_sample_t_1-----------------------------------------------------------
#| echo: TRUE
#| output-location: column

s1 <- "setosa"
s2 <- "versicolor"
t.test(x = iris$Sepal.Width[ iris$Species == s1],
       y = iris$Sepal.Width[ iris$Species == s2],
       paired = FALSE,
       var.equal = TRUE)


## ----two_sample_t_2-----------------------------------------------------------
#| echo: TRUE
#| output-location: column

s <- "setosa"
t.test(x = iris$Sepal.Width[ iris$Species == s],
       y = iris$Sepal.Length[ iris$Species == s],
       paired = TRUE,
       var.equal = FALSE)


## ----ggbp_helper--------------------------------------------------------------
#| echo: true

## Define a wrapper function around ggboxplotplot 
## (slightly simpler: It computes all possible pairs of group_var's levels and passes them 
## (as the list of comparisons to draw) on to stat_compare_means()

require(dplyr)
require(ggplot2)
require(ggpubr)

ggbp <- 
     function(data, group_var, test_var, color, palette,
              var.equal=FALSE,
              paired = FALSE) {

         # Construct a list, containing all
         # possible pairs of "group_var"'s levels:
         cmb <-utils::combn(data |> 
                            dplyr::pull(group_var) |>
                            levels(),
                            2,
                            # insist to return a *list*!
                            simplify=FALSE 
                      ) 
         
         ggboxplot(data=data, x = group_var, y = test_var,
                   color = group_var, palette = palette,
                   order = levels(data |> pull(group_var)),
                   ylab = test_var, xlab = group_var) +
         
             # pass this list to stat_compare:    
             stat_compare_means(method = "t.test",
                                paired = paired,
                                method.args = 
                                  list(var.equal = var.equal),
                                comparisons = cmb
                                            
             )
     } 


## ----ggbp_SW_test1------------------------------------------------------------
#| echo: true
#| output-location: column

ggbp(data = iris,
     group_var = "Species",
     test_var = "Sepal.Width",
     color = "Species",
     palette = c("blue","orange","yellow"),
     paired = FALSE,
     
     var.equal = TRUE
     )


## ----echo=FALSE, results='asis'-----------------------------------------------
## write the HTML file .. 
tools:::Rd2HTML(utils:::.getHelpFile(help(aov)), 
                out = "aov_help.html", stylesheet = "")

## .. and render it  below:


## ----oneway-------------------------------------------------------------------
#| echo: true
#| output: true

# run_formula_test is defined in R/functions.R
lapply(colnames(iris)[1:4], ## run oneway.test() for each trait 
           \(x)run_formula_test(iris, "Species",x,test_f=oneway.test)
  )|> 
  bind_rows() |> ## concatenate the results by row
  select(statistic,p.value,group_var,test_var) ## filter outputs to keep


## ----oneway2------------------------------------------------------------------
#| echo: TRUE
#| output-location: column
oneway.test(Petal.Width ~ Species, data=iris)


## ----echo=FALSE, results='asis'-----------------------------------------------
## write the HTML file .. 
tools:::Rd2HTML(utils:::.getHelpFile(help(oneway.test)), 
                out = "oneway.test_help.html", stylesheet = "")

## .. and render it  below:


## ----echo=FALSE, results='asis'-----------------------------------------------
## write the HTML file .. 
tools:::Rd2HTML(utils:::.getHelpFile(help(lm)), 
                out = "lm_help.html", stylesheet = "")

## .. and render it  below:


## ----prep_ttest0--------------------------------------------------------------
#| echo: FALSE
ttest0 <- iris |> dplyr::reframe(t.test(Sepal.Width, 
                                        var.equal = TRUE
                                       ) |> broom::tidy(),
                                 .by=Species)


## ----echo_ttest0--------------------------------------------------------------
#| echo: TRUE
#| eval: FALSE

# iris |>
#   dplyr::reframe(t.test(Sepal.Width, var.equal = TRUE) |> broom::tidy(),
#                 .by=Species)


## ----print_ttest0-------------------------------------------------------------
#| echo: FALSE
#| eval: TRUE

options(width=150)

print(ttest0)



## ----aov_summary--------------------------------------------------------------
#| echo: TRUE

aov_res <- aov(Sepal.Width ~ Species, data=iris) # keep it for later use 
aov_res |> summary() # output the summary only


## ----Tukey_HSD_test-----------------------------------------------------------
#| echo: TRUE

TukeyHSD(aov_res)



## ----model_W.L_S1-------------------------------------------------------------
#| echo: TRUE
mod <- lm(Sepal.Width ~ Sepal.Length + Species, data=iris)


## ----model_W.L_S2-------------------------------------------------------------
#| class-output: long-output
#| echo: TRUE
mod_sum <- summary(mod) # This produces an output in the style of the previous slide


## ----model_W.L_S3-------------------------------------------------------------
#| class-output: long-output
#| echo: TRUE
mod_sum


## ----model_estimates----------------------------------------------------------
#| echo: TRUE

# The t-test compares the individual estimates 
# against zero
mod_sum$coefficients



## ----model_compute_t----------------------------------------------------------
#| echo: TRUE

# Note: The numerator is really Estimate - Mean, but here we assume Mean = 0
mod_sum$coefficients[,"Estimate"] / mod_sum$coefficients[,"Std. Error"]


## ----model_get_fstat----------------------------------------------------------
#| echo: TRUE
#| output-location: column-fragment

mod_sum$fstatistic # lazy style -- just copy it


## ----model compute_fstat------------------------------------------------------
#| echo: TRUE

# Or do it yourself: The total significance of the model is the significance gained 
# when comparing to the NULL model (without any predictors):
anova(lm(Sepal.Width ~ 1, data = iris),  
      mod)



## ----model_mat_head-----------------------------------------------------------
#| echo: TRUE

model.matrix(mod) |> head() ## first 6 lines


## ----model_mat_middle---------------------------------------------------------
#| echo: TRUE
#| 
model.matrix(mod)[50:55,]  ## middle 6 lines


## ----model_mat_tail-----------------------------------------------------------
#| echo: TRUE
#| 
model.matrix(mod) |> tail() ## last 6 lines


## ----model_W.L_S4-------------------------------------------------------------
#| class-output: long-output
#| echo: TRUE
mod_sum


## -----------------------------------------------------------------------------
#| echo: true
#| eval: true
#| output-location: column

ggplot2::ggplot(
  iris,
  aes(y=Sepal.Width,
      x=Sepal.Length
    )
)+geom_point()+
  geom_smooth(method="lm")
  


## -----------------------------------------------------------------------------
#| echo: true
#| eval: false

# lm(Sepal.Width ~ Sepal.Length, data = iris) |> summary()
# 


## -----------------------------------------------------------------------------
#| echo: true
#| eval: false

# lm(Sepal.Width ~ Sepal.Length + Species, data = iris) |> summary()
# 


## -----------------------------------------------------------------------------
#| echo: true
#| eval: true
#| output-location: column

ggplot2::ggplot(
  iris,
  aes(y=Sepal.Width,
      x=Sepal.Length,
      color = Species 
    )
)+geom_point()+
  geom_smooth(method="lm")
  


## -----------------------------------------------------------------------------
#| echo: true
#| eval: true
#| output-location: column

mdl <- 
  lm(formula = Sepal.Width ~ Sepal.Length + Species,
     data=iris)

ggplot2::ggplot(
  iris,
  aes(y=Sepal.Width,
      x=Sepal.Length,
      color = Species 
    )
)+geom_point()+
  geom_smooth(method="lm",
              mapping=aes(y=predict(mdl,iris))
  )
  


## -----------------------------------------------------------------------------
#| echo: true
#| eval: true
#| output-location: column

mdl <- 
  lm(formula = Sepal.Width ~ Sepal.Length * Species,
     data=iris)

ggplot2::ggplot(
  iris,
  aes(y=Sepal.Width,
      x=Sepal.Length,
      color = Species 
    )
)+geom_point()+
  geom_smooth(method="lm",
              mapping=aes(y=predict(mdl,iris))
  )
  


## -----------------------------------------------------------------------------
#| echo: true
#| output-location: column

# "Sum Sq" (Sum of Squares) is the amount of variance
# explained by adding the predictor to the model, 
# given all other predictors already added. 
anova(lm(Sepal.Width ~ Sepal.Length + Species, 
         data=iris)) 


## -----------------------------------------------------------------------------
#| echo: true

anova(lm(Sepal.Width ~ Sepal.Length + Species, 
         data=iris)) 



## -----------------------------------------------------------------------------
#| echo: true
#| output-location: column-fragment

# "Sum of Sq" (Sum of Squares) is the amount of variance 
# explained by adding the predictor to the model, 
# given all other predictors already added. 

anova(lm(Sepal.Width ~ 1, # means: "Intercept only"             
         data=iris), 
      lm(Sepal.Width ~ Species, 
         data=iris),
      lm(Sepal.Width ~ Species + Sepal.Length ,                
         data=iris))



## -----------------------------------------------------------------------------
#| echo: true
#| output-location: column-fragment

anova(lm(Sepal.Width ~ Species + Sepal.Length ,                
         data=iris))



## ----echo=FALSE, results='asis'-----------------------------------------------
## write the HTML file .. 
tools:::Rd2HTML(utils:::.getHelpFile(help(anova)), 
                out = "anova_help.html", stylesheet = "")

## .. and render it  below:


## ----stats.anova--------------------------------------------------------------
#| echo: TRUE
anova(lm(Sepal.Width ~ Sepal.Length + Species, 
         data=iris))


## ----car.Anova----------------------------------------------------------------
#| echo: TRUE
car::Anova(lm(Sepal.Width ~ Sepal.Length + Species, 
              data=iris))

