# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(dplyr,emmeans,glue,multcomp, qs, tidyr,tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
spcs <- spcs[1:72]

# Function to use ---------------------------------------------------------
logZonalDF <- function(spc){
    
    spc <- spcs[1]
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
df <- qs::qread(file = glue('./qs/zonalOccPi/all_sp_occPiZonal1191.qs'))

df <- df %>% group_by(specie, model) %>% 
    mutate(region =as.factor(region),
           model = as.factor(model))

df$region <- relevel(df$region, ref = 'Middle')

# Calculate test statistics using aov function
mod1 <- lm (average~ region + model, data = df)
mod2 <- lm (average~ region * model, data = df)
anova(mod1)
print(summary(mod1))
coef(summary.lm(mod1))
leastSquare2 <- emmeans::emmeans(mod1, pairwise~ region, adjust = 'tukey')

#all pairwise comparison 
m1_emm <- emmeans::emmeans(mod1, specs = pairwise ~ region:model)
marginal <- emmeans::emmeans(mod1, ~ region)
CLD = multcomp::cld(marginal, alpha = 0.05,
                    letters = letters,
                    adjust = 'tukey')
pairs(marginal, adjust = 'tukey')
marginal2 <- marginal %>% as.data.frame()

#put results in a df
meansDT<-m1_emm$emmeans %>% as.data.frame()
m1_simple <- emmeans::contrast(m1_emm,
                      method = "pairwise",
                      simple = "each",
                      combine = TRUE,
                      adjust = "tukey") %>%
  summary(infer = TRUE)

##now create plot of the mean effects 
my_colors <- c("#FF6A00", "#C15CCB",  "#00868B")

meanPlot<- ggplot() + 
  geom_point(data = meansDT, aes(x = region, y = emmean, group = model, color = model),
  position = position_dodge(width = 0.8)) +
  geom_errorbar(data = meansDT, aes(x = region, ymin = lower.CL, ymax = upper.CL, 
                                    color = model), width = 0.2,
                position = position_dodge(width = 0.8)) +
  labs(x = "Zone", y = "Estimate", color = "") +
  scale_x_discrete(limits = c('North', 'Middle', 'South')) +
  scale_color_manual(values = my_colors) +  # Assign custom colors
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.text.y = element_text( vjust = 0.5,  hjust = 0.5, size = 11),
        axis.text.x = element_text( vjust = 0.5,  hjust = 0.5, size = 11),
        aspect.ratio = 1,
        legend.position = 'right',
        legend.text = element_text(size= 11)) 

meanPlot2<- ggplot() + 
  geom_point(data = marginal2, aes(x = region, y = emmean),
             position = position_dodge(width = 0.8)) +
  geom_errorbar(data = marginal2, aes(x = region, ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(width = 0.8)) +
  labs(x = "Zone", y = "Estimate", color = "") +
  scale_x_discrete(limits = c('North', 'Middle', 'South')) +
  scale_color_manual(values = my_colors) +  # Assign custom colors
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.text.y = element_text( vjust = 0.5,  hjust = 0.5, size = 11),
        axis.text.x = element_text( vjust = 0.5,  hjust = 0.5, size = 11),
        aspect.ratio = 1,
        legend.position = 'right',
        legend.text = element_text(size= 11)) 
  
out <- glue('./graphs/figs/zonal_emmeans/effectsregion_GCM.png')
ggsave(plot = meanPlot, filename = out, units = 'in', width = 5, height = 4, dpi = 300)


m1_response_fac <- ggstripchart(
  data = df,
  x = "model",
  y = "average",
  color = "region",
  fill = "region",
  ylab = 'Mean proportional change',
 # palette = pal_okabe_ito_blue,
  #position = position_dodge(width = dodge_width),
  position = position_jitterdodge(dodge.width = dodge_width,
                                  jitter.width = jitter_width)
) 

m1_response_fac + 
  
# add layer containing means
 gg <- ggplot(data = m1_emm_dt, (aes(x = model, y = emmean), group = region)) +
 gg + geom_point(aes(x = model, y = emmean,
                 fill = model), 
             size = 3) 

m1_response_fac  +
  
  # add layer containing error bars
  geom_errorbar(data = m1_emm_dt, 
                aes(y = emmean,
                    ymin = lower.CL, 
                    ymax = upper.CL,
                  color = region),
                width = 0.05)
               
  
  # add p-value brackets
  stat_pvalue_manual(
    data = m1_simple_dt,
    label = "p_pretty",
    xmin = "xmin",
    xmax = "xmax",
    y.position = c(3300, 3300, 3450, 3600),
    tip.length = 0.01,
    size = 3) +
  







plot(leastSquare[[1]],
     CIs = TRUE, 
     PIs = TRUE, 
     comparisons = TRUE,  
     colors= c("black","dark grey","grey","green"),
     alpha=0.05,
     adjust="tukey") +
  theme_bw() +
 coord_flip()

# display the results by grouping using letters
pairs(leastSquare)
plot(leastSquare, comparisons = TRUE)

emmeans::emmip(leastSquare, average ~ model * region, CIs = TRUE)

emeansPlot <- leastSquare[[1]] %>%  as.data.frame()


library("ggpubr")
ggboxplot(df, x = "model", y = "average", fill = 'region', xlab = 'Model', 
          ylab = 'Mean proportional change')

# compute summary statistics ----------------------------------------------
group_by(df, region, model) %>%
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
