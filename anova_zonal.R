# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, reproducible, RColorBrewer, ggspatial, 
               ggpubr, gridExtra, stringr, glue, sf, tidyverse, fasterize,
               RStoolbox, fs, fst, trend, colorspace, hrbrthemes,exactextractr, furrr, future, spatialEco)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# Function to use ---------------------------------------------------------
logZonalDF <- function(spc){
    
    #spc <- spcs[1]
    cat('-------------------------------------------------------------\n')
    cat('To start ', spc, '\n')
    cat('-------------------------------------------------------------\n')
    
    dir <- grep(spc, dirs, value = TRUE)
    fle <- fs::dir_ls(dir, regexp = '.qs')
    tbl <- qs::qread(file = glue('./qs/zonal/{spc}_logZonal2.qs'))
    tbl <- tbl %>%  mutate(specie = spc)
    return(tbl)
}   

# Apply the function ------------------------------------------------------

zonalDF <- map(.x = spcs, .f = logZonalDF)

znl <- bind_rows(zonalDF) 


#qs::qsave(x = znl, file = glue('./qs/zonal/allSpp_logZonal2_region2.qs'))
df <- qs::qread(file = glue('./qs/zonal/allSpp_logZonal2_region2.qs'))
df <- df %>% group_by(specie, model) %>% mutate(region2 = recode(region, 'High Boreal' = 'South',
                        'Mid-Boreal' = 'South', 'High Subarctic' = 'North',
                        'Low Arctic north' = 'North',
                        'Low Subarctic' = 'Middle'
                        ) )
df <- df %>% group_by(specie, model) %>% 
    mutate(region2 =factor(region2, levels = c('South', 'Middle', 'North')),
           model = factor(model))

df$region2 <- relevel(df$region2, ref = 'Middle')
# Calculate test statistics using aov function
mod1 <- lm(average~ region2 + model, data = df)
anova(mod1)
print(summary(mod1))
coef(summary.lm(mod1))
leastSquare <- emmeans(mod1, pairwise~region2, adjust = 'tukey')

# display the results by grouping using letters
pairs(leastSquare)
plot(leastSquare, comparisons = TRUE)
pwpp(mod1)

library("ggpubr")
ggboxplot(df, x = "region2", y = "average", fill = 'model', xlab = 'Region', ylab = 'Mean proportional change', order = c('South', 'Middle', 'North'))

# compute summary statistics ----------------------------------------------
group_by(df, region2, model) %>%
    summarise(
        count = n(),
        mean = mean(average, na.rm = TRUE),
        sd = sd(average, na.rm = TRUE)
    )

plot(mod1, 1)
library(car)
#homogeneity of Variance
leveneTest(average ~ region2*model, data = df)
# 2. Normality
plot(mod1, 2)
# Extract the residuals
aov_residuals <- residuals(object = mod1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
attach(df)
friedman.test(average, groups = region2, blocks = model)

# ANOVA procedure on rank-transformed data
aov.rnk <- aov(
    rank(average) ~ region2 + model, data = df,
    contrasts = list(
      region2 = 'contr.sum',
        model = 'contr.sum'
    )
)
Anova(aov.rnk, type = 'III')

res.rnk = aov.rnk$resid
qqnorm(
    res.rnk, pch = 20, main = "Rank-Transformed",
    cex.lab = 1, cex.axis = 0.7, cex.main = 1
)
qqline(res.rnk)

plot(aov.rnk, 1, main = "Rank-Transformed")

# Compute estimated marginal means for factor combinations
library(emmeans)
emmeans(aov.rnk, pairwise ~ region2| model)

em_out_category <- emmeans(aov.rnk,  ~ region2 | model) 
print(em_out_category)
em_out_category %>% 
    pairs() %>% 
    test(joint = TRUE)
pairs(em_out_category)

library(multcomp)
summary(glht(aov.rnk, linfct = mcp(region2 = "Tukey")))

ggplot(df, aes(x = region2, y = average)) +
    geom_boxplot(size = .75) +
    geom_jitter(alpha = .5) +
    facet_grid(region2 ~ model, margins = TRUE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
## fit ordered logit model and store results 'm'
m <- polr(average~ region2 + model, data = df, Hess=TRUE)

# Load libraries
library(dplyr)
library(rms)

# Select only columns needed
dat <- df[, c('region2', 'model', 'average')]
dd <- datadist(dat); options(datadist = 'dd')
f <- orm(average ~ region2 + model, data = dat)
library(depigner)
data.frame(tidy_summary(summary(f)))

anova(f)

plot(anova(f))

alphas <- coef(f)[1 : num.intercepts(f)]
yunique <- f$yunique[-1]
par(mfrow = c(1,2))
plot(yunique, alphas)
plot(ecdf(resid(ols(average ~ region2 * model, data = dat))), main = '')

M <- Mean(f)
Predict(f, region2, model, fun = M)

plot(Predict(f, fun = M), ylab = 'Predicted Mean')
with(dat, summarize(average, llist(region2, model), smean.cl.normal))

contrast(
    f,
    list(
        region2 = c('South', 'Middle', 'North'),
        model = c('CanESM2', 'CCSM4', 'INM-CM4')
    )
)

Ecdf( ~ average, group = region2, data = dat, fun = qlogis,
      xlab = 'log(Result)', ylab = expression(logit~(F[n](x))))
Ecdf( ~ average, group = model, data = dat, fun = qlogis,
      xlab = 'log(Result)', ylab = expression(logit~(F[n](x))))
