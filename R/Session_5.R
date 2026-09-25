# =============================================================================
#  R Intermediate Course 2026 — Session 5
#  PCA & Hierarchical Clustering
#
#  Companion script for the slides "Session_5.html" about
#  PCA and hierarchical clustering.
#
#  How to use this file:
#  ---------------------
#   * Open in RStudio. Use Ctrl/Cmd+Shift+O to see the section outline.
#   * Run one line  : Cursor in the line and then Ctrl/Cmd + Enter
#   * Run a section : Highlight the section + Ctrl/Cmd+Enter
#   * Each section matches one slide, in the same order as the deck, so you
#     can follow along live during the explanation.
#
#  Dataset: built-in iris (150 x 4 numeric measurements + Species).
#  Package: cluster (for silhouette()) — install.packages("cluster") if needed.
# =============================================================================


# ---- 0. Setup ---------------------------------------------------------------

library(ggplot2)
theme_set(theme_minimal(base_size = 13))


# =============================================================================
#  PART I — Principal Component Analysis
# =============================================================================


# ---- 1. Step 1 — center, and usually scale ----------------------------------

iris_num <- iris[, 1:4]
apply(iris_num, 2, var)                 # variances differ across columns

iris_z <- scale(iris_num)               # center + divide by SD
round(apply(iris_z, 2, mean), 10)       # ~0
round(apply(iris_z, 2, sd),   10)       # 1


# ---- 2. Step 2 — prcomp() ---------------------------------------------------

pca <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
names(pca)

# pca$sdev     — standard deviation of each PC (sqrt of eigenvalue)
# pca$rotation — the loadings: how variables combine into PCs
# pca$x        — the scores: where each observation lands


# ---- 3. The summary() table -------------------------------------------------

pca <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
summary(pca)


# ---- 4. Variance explained — scree plot -------------------------------------

# Base R — screeplot()
pca <- prcomp(iris[, 1:4], scale. = TRUE)
screeplot(pca, type = "lines",
          main = "")
abline(h = 1, lty = 2, col = "#D55E00")

# ggplot2 — a bit more control
df <- data.frame(
  PC  = factor(paste0("PC", 1:4),
               levels = paste0("PC", 1:4)),
  eig = pca$sdev^2)

ggplot(df, aes(PC, eig, group = 1)) +
  geom_col(fill = "#0072B2", alpha = 0.7) +
  geom_line() + geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = 2,
             colour = "#D55E00") +
  labs(y = "Eigenvalue")


# ---- 5. Loadings — what each PC means ---------------------------------------

pca <- prcomp(iris[, 1:4], scale. = TRUE)
round(pca$rotation, 2)


# ---- 6. The biplot -----------------------------------------------------------

# Base R — biplot()
pca <- prcomp(iris[, 1:4], scale. = TRUE)
biplot(pca, cex = 0.6,
       col = c("grey60", "#D55E00"))

# ggplot2 — coloured by species
sc <- as.data.frame(pca$x)
sc$Species <- iris$Species
L <- as.data.frame(pca$rotation) * 2.5
L$var <- rownames(L)

ggplot(sc, aes(PC1, PC2)) +
  geom_point(aes(colour = Species), alpha = .8) +
  geom_segment(data = L,
    aes(0, 0, xend = PC1, yend = PC2),
    arrow = arrow(length = unit(.15, "cm")),
    colour = "#D55E00") +
  geom_text(data = L,
    aes(PC1 * 1.1, PC2 * 1.1, label = var),
    colour = "#D55E00", size = 3)


# =============================================================================
#  PART II — Hierarchical Clustering
# =============================================================================


# ---- 7. Distance — the input to hclust() ------------------------------------

iris_z <- scale(iris[, 1:4])
d      <- dist(iris_z, method = "euclidean")

as.matrix(d)[1:5, 1:5] |> round(2)      # first 5 x 5 block

# Same scaling point returns here: with very different units, dist() is
# dominated by whichever variable has the largest range.
cor(as.vector(dist(iris[, 1:4])),
    as.vector(dist(scale(iris[, 1:4]))))


# ---- 8. Linkage — distance between clusters ---------------------------------

d <- dist(scale(iris[, 1:4]))

op <- par(mfrow = c(1, 2), mar = c(2, 3, 2, 1))
plot(hclust(d, method = "single"),
     labels = FALSE, main = "single",
     xlab = "", sub = "")
plot(hclust(d, method = "complete"),
     labels = FALSE, main = "complete",
     xlab = "", sub = "")
par(op)

op <- par(mfrow = c(1, 2), mar = c(2, 3, 2, 1))
plot(hclust(d, method = "average"),
     labels = FALSE, main = "average",
     xlab = "", sub = "")
plot(hclust(d, method = "ward.D2"),
     labels = FALSE, main = "ward.D2",
     xlab = "", sub = "")
par(op)


# ---- 9. Cutting the tree -----------------------------------------------------

iris_z <- scale(iris[, 1:4])
hc <- hclust(dist(iris_z), method = "ward.D2")
cl <- cutree(hc, k = 3)

table(cluster = cl, species = iris$Species)


# ---- 10. Checking your clusters ----------------------------------------------

library(cluster)
d  <- dist(scale(iris[, 1:4]))
hc <- hclust(d, method = "ward.D2")
cl <- cutree(hc, k = 3)

mean(silhouette(cl, d)[, "sil_width"])   # -1 to 1, higher = more separated
cor(d, cophenetic(hc))                    # 0 to 1, tree vs. original distances


# ---- 11. Colouring the dendrogram --------------------------------------------

hc <- hclust(dist(scale(iris[, 1:4])), method = "ward.D2")
plot(hc, labels = FALSE, main = "iris — ward.D2",
     xlab = "", sub = "")
rect.hclust(hc, k = 3,
            border = c("#D55E00", "#0072B2", "#009E73"))


# ---- 12. Heatmap: matrix and dendrogram together ------------------------------

iris_z  <- scale(iris[, 1:4])
sp_col  <- c(setosa = "#D55E00", versicolor = "#0072B2", virginica = "#009E73")

heatmap(iris_z,
        hclustfun = function(d) hclust(d, method = "ward.D2"),
        Colv = NA, labRow = NA, margins = c(6, 2),
        RowSideColors = sp_col[iris$Species])


# ---- 13. PCA + clustering together --------------------------------------------

iris_z <- scale(iris[, 1:4])
pca <- prcomp(iris_z)
hc  <- hclust(dist(iris_z), method = "ward.D2")
cl  <- factor(cutree(hc, k = 3))

df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                 cluster = cl, species = iris$Species)

ggplot(df, aes(PC1, PC2)) +
  geom_point(aes(colour = cluster, shape = species), size = 3, alpha = 0.8) +
  stat_ellipse(aes(colour = cluster), level = 0.68) +
  labs(subtitle = "colour = cluster, shape = true species")


# ---- 14. Session info ---------------------------------------------------------

sessionInfo()
