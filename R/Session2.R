library(readr)
library(tidyr)
library(dplyr)

## readr
#-------

df <- read.csv("data/rna_results.csv")


df <- read_csv("data/rna_results.csv")

## tibble
#--------
iris

tibble::tibble(iris)

## tidyr
#-------


# == dadaset ==
iris_filtered <- iris |>
  filter(Sepal.Length > 7) |>
  mutate(ID = row_number())

 iris_filtered

# == base r ==
long_base <- reshape(
  iris_filtered,
  varying = c(
    "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"
  ),
  v.names = "Value",
  timevar = "Measurement",
  times = c(
    "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"
  ),
  idvar = "ID",
  direction = "long"
)

long_base

wide_base <- reshape(
  long_base,
  idvar = "ID",
  timevar = "Measurement",
  direction = "wide"
)

wide_base |> head()

# == tidyr ==
long <- iris_filtered |>
  pivot_longer(
    cols = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
    names_to = "Measurement",
    values_to = "Value"
  )

long

wide <- long |>
  pivot_wider(
    names_from = Measurement,
    values_from = Value
  )

wide |> head()

## pipe
#------

res |>
  filter(!is.na(padj),
         padj < 0.05) |>
  mutate(
    direction = if_else(
      log2FoldChange > 0,
      "Up",
      "Down"
    )
  ) |>
  arrange(desc(abs(log2FoldChange))) |>
  select(gene, log2FoldChange, padj, direction)




## dplyr
#------

# == base r ==

# 1. Filter rows, select columns, and create the new column
sub_iris <- subset(iris,
  Sepal.Length > 5,
  select = c(Sepal.Length, Petal.Length, Species)
  )
sub_iris$SePa.Length <- sub_iris$Sepal.Length + sub_iris$Petal.Length

# 2. Sort rows in descending order of the new column
sub_iris <- sub_iris[order(-sub_iris$SePa.Length), ]

# 3. Group by Species and calculate mean and count (n)
result <- aggregate(SePa.Length ~ Species,
  data = sub_iris,
  FUN = function(x) {
  c(mean = mean(x), n = length(x))
})

# 4. Flatten the matrix column produced by aggregate into a clean data frame
result <- do.call(data.frame, result)
names(result) <- c("Species", "mean", "n")

print(result)

# == dplyr ==

iris |>
   filter(Sepal.Length > 5) |>                          # filter rows
   select(Sepal.Length, Petal.Length, Species) |>       # select columns
   mutate(SePa.Length=Sepal.Length+Petal.Length) |>     # add new column
   arrange(desc(SePa.Length)) |>                        # sort rows
   group_by(Species) |>                                 # group by Species
   summarise(mean = mean(SePa.Length), n = n())         # summarise



dplyr::select()
dplyr::slice()
dplyr::filter()
dplyr::mutate() & dplyr::transmute()
dplyr::arrange ()

aggregate(cbind(Petal.Length, Sepal.Length) ~ Species + Petal.Width,
data = iris,
FUN = function(x) { c(mean = mean(x), n = length(x))
})

iris %>%
  group_by(Species,Petal.Width) %>%
  summarise(mean = mean(Petal.Length), mean2 = mean(Sepal.Length) , n = n()) %>%
  ungroup()



# Left table (xdf)
xdf <- data.frame(id = c(1, 2, 3), name = c("Alice", "Bob", "Charlie"))

# Right table (ydf) Top table
ydf <- data.frame(id = c(1, 1, 4), score = c(90, 95, 80))

# Right table (zdf) Bottom
zdf <- data.frame(id = c(1, 1, 4), score = c(40, 15, 70))



dplyr::distinct()
dplyr::bind_cols()
dplyr::bind_rows()



geom_errorbar(aes(ymin = len, ymax = len + sd), width = 0.2) +


dplyr::left_join()
dplyr::right_join()
dplyr::inner_join()
dplyr::full_join()
dplyr::semi_join()
dplyr::anti_join()
