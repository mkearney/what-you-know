
## load ggplot2
suppressPackageStartupMessages(library(ggplot2))

## set base size params
cex <- 2
base_size <- 14

## set min and max for x-axis
min <- -3
max <- 3
by <- .01

## specify font
base_family <- "Avenir Next"

## create x (horizontal) data points
x <- seq(min, max, by)

## use normal dist for y (vertical) data points
y <- dnorm(x)

## combine as data frame
df <- data.frame(x = x, y = y)

## set ggplot theme
theme_set(
 theme_minimal(base_size = base_size, base_family = base_family) + 
  theme(axis.text = element_blank(),
        plot.title = element_text(face = "bold"))
)

## range and other calculations
xrange <- do.call("-", as.list(rev(range(x))))
xbump <- xrange / 15.78
yrange <- do.call("-", as.list(rev(range(y))))
ymax <- max(y)
ymin <- min(y)
ybump <- yrange / 15.78

## horizontal arrow
xdat <- data.frame(
 x = c(min + xrange * .25, max - xrange * .25),
 y = c(ybump, ybump)
)
xdat_top_arrow <- data.frame(
 x = c((max - xrange * .25) - .025 * xrange, max - xrange * .25),
 y = c(ybump + (.6 * ybump), ybump)
)
xdat_bot_arrow <- data.frame(
 x = c(max * .5 - .025 * xrange, max * .5),
 y = c(ybump - (.6 * ybump), ybump)
)

ybot <- ymin + yrange * .225
ytop <- ymax - yrange * .225

## vertical arrow
ydat <- data.frame(
 x = c(min + xbump, min + xbump),
 y = c(ybot, ytop)
)

ydat_l_arrow <- data.frame(
 x = c(min + xbump - (xrange * .025), min + xbump),
 y = c(ytop * .95, ytop)
)
ydat_r_arrow <- data.frame(
 x = c(min + xbump + (xrange * .025), min + xbump),
 y = c(ytop * .95, ytop)
)


## plot base layer
p <- ggplot(df, aes(x, y)) + 
 geom_line(size = cex) + 
 labs(
  x = "Actual knowledge", 
  y = "Perceived knowledge",
  title = "Smart enough to know you're not",
  subtitle = "Perceived knowledge vs. actual knowledge"
 )

## add x arrow
p <- p +  
 geom_line(data = xdat, colour = "#dd0000", size = cex * .5) + 
 geom_line(data = xdat_top_arrow, colour = "#dd0000", size = cex * .5, lineend = "square") + 
 geom_line(data = xdat_bot_arrow, colour = "#dd0000", size = cex * .5, lineend = "square")

## add text for x arrow
p <- p + 
 annotate("text", 0, .04, label = "The more you know", size = cex * 1.83, fontface = "bold", colour = "#dd0000")

## add y arrow
p <- p + 
 geom_line(data = ydat, colour = "#dd0000", size = cex * .5) + 
 geom_line(data = ydat_l_arrow, colour = "#dd0000", size = cex * .5, lineend = "square") + 
 geom_line(data = ydat_r_arrow, colour = "#dd0000", size = cex * .5, lineend = "square")

## ada text for y arrow
p <- p + 
 annotate("text", min + xbump, median(y) + yrange * .175, 
          label = "The more you\nTHINK you know", size = cex * 1.83, angle = 90, colour = "#dd0000",
          fontface = "bold")

## save as .png file
png("../image.png", 6, 5, "in", res = 214)
p
dev.off()
