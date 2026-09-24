# =============================================================================
#  R Intermediate Course 2026 — Session 3
#  Data Visualization with ggplot2
#
#  Companion script for the slides "IntermediateDay1_ggplot2.html"
#
#  How to use this file:
#  ---------------------
#   * Open in RStudio. Use Ctrl/Cmd+Shift+O to see the section outline.
#   * Run one line  : Cursor in the line and then Ctrl/Cmd + Enter
#   * Run a section : Highlight the section + Ctrl/Cmd+Enter
#   * Each section matches one slide, in the same order as the deck, so you
#     can follow along live during the explanation.
#
#  Datasets: built-in iris (150 x 5) and mtcars (32 x 11).
# =============================================================================


# ---- 0. Setup ---------------------------------------------------------------

library(ggplot2)
theme_set(theme_minimal(base_size = 13))


# =============================================================================
#  PART I — Why ggplot2?
# =============================================================================


# ---- 1. The same scatter plot, two ways -------------------------------------

# Base R
plot(Sepal.Width ~ Sepal.Length, data = iris)

# ggplot2
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point()


# ---- 2. Quick check — fixed colour vs mapped colour -------------------------

# Compare these two base R calls before moving on:
cols <- c("#E69F00", "#0072B2", "#009E73")

# A — fixed colour: every point is the same blue, no matter what
plot(iris$Sepal.Length, iris$Sepal.Width, col = "blue")

# B — mapped colour: looks up a colour per observation from Species
plot(iris$Sepal.Length, iris$Sepal.Width, col = cols[iris$Species])

# Which one would need you to build a legend by hand? (Answer: B)


# ---- 3. Anatomy of a ggplot() call ------------------------------------------

ggplot(data = iris,                                  # 1. the data
       mapping = aes(x = Sepal.Length,               # 2. map columns
                     y = Sepal.Width,                 #    to visual
                     colour = Species)) +              #    properties
  geom_point() +                                      # 3. draw points
  labs(x = "Sepal length (cm)",                       # 4. labels
       y = "Sepal width (cm)")

# Notes:
#  * data and mapping set once in ggplot() are inherited by every layer,
#    unless a layer overrides them.
#  * aes() is for MAPPINGS FROM DATA. A fixed value (all points blue) goes
#    OUTSIDE aes().


# =============================================================================
#  PART II — Feature by feature: base R on the left, ggplot2 on the right
# =============================================================================


# ---- 4. Colour and legend from a variable -----------------------------------

# Base R — formula interface, own legend
cyl_f <- factor(mtcars$cyl)
cols  <- c("#E69F00", "#0072B2", "#009E73")
plot(mpg ~ wt, data = mtcars,
     col = cols[cyl_f], pch = 16)
legend("topright", legend = levels(cyl_f),
       col = cols, pch = 16,
       title = "cyl", bty = "n")

# ggplot2 — one mapping, legend appears
ggplot(mtcars, aes(wt, mpg,
                   colour = factor(cyl))) +
  geom_point() +
  labs(colour = "cyl")


# ---- 5. Bar chart of counts -------------------------------------------------

# Base R — tabulate first, then plot
tab <- table(mtcars$cyl)
barplot(tab,
        xlab = "Cylinders",
        ylab = "Count")

# ggplot2 — counting is built in
ggplot(mtcars, aes(factor(cyl))) +
  geom_bar() +
  labs(x = "Cylinders", y = "Count")

# geom_bar() counts the rows in each category for you.
# If you already have the numbers, use geom_col() instead.


# ---- 6. Histogram ------------------------------------------------------------

# Define the bin boundaries once for both plots
brks <- seq(10, 35, by = 2.5)

# Base R
hist(mtcars$mpg, breaks = brks,
     xlab = "MPG", main = "")

# ggplot2
ggplot(mtcars, aes(mpg)) +
  geom_histogram(breaks = brks)


# ---- 7. Boxplot by group -----------------------------------------------------

# Base R — formula interface
boxplot(mpg ~ cyl, data = mtcars,
        xlab = "Cylinders", ylab = "MPG")

# ggplot2 — x and y as a mapping
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  labs(x = "Cylinders", y = "MPG")


# ---- 8. Small multiples: base R ----------------------------------------------

op <- par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
xr <- range(iris$Sepal.Length)
yr <- range(iris$Sepal.Width)
for (sp in levels(iris$Species)) {
  plot(Sepal.Width ~ Sepal.Length, data = iris,
       subset = Species == sp,
       xlim = xr, ylim = yr, main = sp, pch = 16)
}
par(op)


# ---- 9. Small multiples: ggplot2 ---------------------------------------------

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  facet_wrap(~ Species)


# ---- 10. A fitted line per group ---------------------------------------------

# Base R — fit and draw each line
cols  <- c("#E69F00", "#0072B2", "#009E73")
sp    <- iris$Species
plot(Sepal.Width ~ Sepal.Length, data = iris,
     col = cols[sp], pch = 16)
for (i in levels(sp)) {
  d <- iris[sp == i, ]
  abline(lm(Sepal.Width ~ Sepal.Length, d),
         col = cols[which(levels(sp) == i)])
}

# ggplot2 — add a smooth layer
ggplot(iris, aes(Sepal.Length, Sepal.Width,
                 colour = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# ---- 11. Styling and themes --------------------------------------------------

# Base R — share settings with par()
op <- par(cex.lab = 1.2, cex.axis = 1.1, las = 1)
plot(mpg ~ wt, data = mtcars,
     pch = 16, col = "#0072B2")
par(op)

# ggplot2 — a named, reusable theme
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(colour = "#0072B2") +
  theme_classic(base_size = 14)


# ---- 12. A plot you can keep, not just draw ----------------------------------

p  <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p2 <- p + facet_wrap(~ cyl)   # p2 is p, extended
p                              # p itself is unchanged


# ---- 13. Save a plot object --------------------------------------------------

# Base R — open a device, plot, close it
png("figure.png", width = 8, height = 5,
    units = "in", res = 300)
plot(mpg ~ wt, data = mtcars)
dev.off()

# ggplot2 — save the plot object p
p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

ggsave("figure.png", p,
       width = 8, height = 5,
       units = "in", dpi = 300)


# ---- 14. Session info ---------------------------------------------------------

sessionInfo()
