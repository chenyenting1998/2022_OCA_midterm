# prep ----
pacman::p_load(dplyr, 
               readxl,
               writexl,
               tidyr, 
               vegan,
               ggplot2, 
               ggrepel,
               ggdendro,
               patchwork,
               pairwiseAdonis,
               cluster,
               Polychrome, 
               extrafont)
# source
source("source/msjh.R")
source("source/map_func.R")

# load data 
load("data/2022_OCA_midterm_size.RData")
load("data/color_code.RData")
load("data/den_bio_wide.RData")


# density ----

## permdisp ----
dwd_p <- 
  dw[dw$Location != "Taoyuan", -(1:2)]^0.25 %>%
  vegdist()

den_disp <- 
  betadisper(dwd_p,
             dw[dw$Location != "Taoyuan",]$Location,
             type = 'median',
             sqrt.dist = FALSE)  
set.seed(1)
den_disp_perm <-
  permutest(den_disp, permutations = 9999)

## permanova ----
set.seed(7)
den_perm <-
  adonis2(dwd_p~Location, dw[dw$Location != "Taoyuan",], permutations = 9999)

## pairwise ----
set.seed(8)
pairwise_den_perm <- 
  pairwise.adonis2(dwd_p~Location, 
                   dw[dw$Location != "Taoyuan",], 
                   nperm = 9999)

# biomass ----
## permdisp ----
bwd_p <- 
  bw[bw$Location != "Taoyuan", -(1:2)]^0.25 %>%
  vegdist()

bio_disp <- 
  betadisper(bwd_p,
             bw[bw$Location != "Taoyuan",]$Location,
             type = 'median',
             sqrt.dist = FALSE)  
set.seed(1)
bio_disp_perm <-
  permutest(bio_disp, permutations = 9999)

## permanova ----
set.seed(7)
bio_perm <-
  adonis2(bwd_p~Location, bw[bw$Location != "Taoyuan",], permutations = 9999)

## pairwise ----
set.seed(8)
pairwise_bio_perm <- 
  pairwise.adonis2(bwd_p~Location, 
                   bw[bw$Location != "Taoyuan",], 
                   nperm = 9999)


permdisp_table <- function(permdisp_object) cbind(rownames(as.data.frame(permdisp_object$tab)),as.data.frame(permdisp_object$tab))
permanova_table <- function(permanova_object) cbind(rownames(as.data.frame(permanova_object)), as.data.frame(permanova_object))


write_xlsx(list(density_permdisp = permdisp_table(den_disp_perm),
                density_permanova = permanova_table(den_perm),
                biomass_permdisp = permdisp_table(bio_disp_perm),
                biomass_permanova = permanova_table(bio_perm) ),
           path = "tab/permutation_test.xlsx")

pairwise_bio_perm[[1]] <- NULL
pairwise_den_perm[[1]] <- NULL

pairwise_bio_table <- 
  lapply(pairwise_bio_perm, function(x) { 
  x$p.adjust <- p.adjust(x$`Pr(>F)`, method = "bonferroni", n = 6)
  name <- rownames(x)
  x <- cbind(name, x)
  return(x)})

pairwise_den_table <- 
  lapply(pairwise_den_perm, function(x) { 
  x$p.adjust <- p.adjust(x$`Pr(>F)`, method = "bonferroni", n = 6)
  name <- rownames(x)
  x <- cbind(name, x)
  return(x)})

write_xlsx(pairwise_den_table,
           path = "tab/pairwise_permutation_den.xlsx")
write_xlsx(pairwise_bio_table,
           path = "tab/pairwise_permutation_bio.xlsx")
