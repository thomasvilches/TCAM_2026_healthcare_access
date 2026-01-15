#* pacotes e funcoes -------------------------------------------------------

source("packages.R", encoding = "UTF-8")
source("functions.R", encoding = "UTF-8")


library(glmmTMB)
library(kableExtra)
library(broom)
#* reading DB -----------------------------------------------------------

db <- conecta_base()


#* IBGE --------------------------------------------------------------------
##* correlation ibge -------------------------------------------------------------


# is there correlation?

idade <- read_any(db, "ibge_idade") #age
raca <- read_any(db, "ibge_raca") #race
alf <- read_any(db, "ibge_alfabetizacao") #literacy
renda <- read_any(db, "ibge_renda") #income

mun_int <- read_any(db, "mun_interesse") # municipalities

#calculating proportions
idade <- idade %>%
  select(-1, -total) %>%
  pivot_longer(starts_with("ag"),
    names_to = "group_age",
    values_to = "population_age"
  ) %>%
  group_by(id_mun) %>%
  mutate(
    prop_age = population_age / sum(population_age)
  ) %>%
  select(-population_age) %>%
  ungroup()

raca <- raca %>%
  select(-1, -total) %>%
  pivot_longer(., 2:ncol(.), names_to = "group_race", values_to = "population_race") %>%
  mutate(population_race = replace_na(population_race, 0)) %>%
  filter(group_race != "total") %>%
  group_by(id_mun) %>%
  mutate(
    prop_race = population_race / sum(population_race)
  ) %>%
  select(-population_race) %>%
  ungroup()

alf <- alf %>%
  select(id_mun, total, alf_total) %>%
  mutate(prop_alf = alf_total / total) %>%
  select(id_mun, prop_alf)



ref_idade <- read_any(db, "ref_idades") %>%
  select(groups, idade_pt)
idade_int <- idade %>%
  filter(id_mun %in% mun_int$id_mun) %>%
  left_join(ref_idade, by = c("group_age" = "groups")) %>%
  select(-group_age) %>%
  pivot_wider(names_from = idade_pt, values_from = prop_age)



ref_raca <- read_any(db, "ref_raca") %>%
  select(groups, raca_pt)
raca_int <- raca %>%
  filter(id_mun %in% mun_int$id_mun) %>%
  left_join(ref_raca, by = c("group_race" = "groups")) %>%
  select(-group_race) %>%
  pivot_wider(names_from = raca_pt, values_from = prop_race)

alf_int <- alf %>%
  filter(id_mun %in% mun_int$id_mun)

ref_renda <- read_any(db, "ref_renda") %>% select(groups, renda_pt)

renda_int <- renda %>%
  filter(id_mun %in% mun_int$id_mun) %>%
  pivot_longer(., 3:ncol(.), names_to = "Grupo", values_to = "Valor") %>%
  left_join(ref_renda, by = c("Grupo" = "groups")) %>%
  group_by(id_mun) %>%
  filter(Grupo != "total") %>%
  mutate(
    p = Valor / sum(Valor)
  ) %>%
  select(id_mun, renda_pt, p) %>%
  pivot_wider(names_from = renda_pt, values_from = p)

file_path <- "plots/correlation_ibge.png"
png(height = 8, width = 8, units = "in", file = file_path, type = "cairo", res = 300)

# Your function to plot image goes here

idade_int %>%
  left_join(raca_int) %>%
  left_join(alf_int) %>%
  left_join(renda_int) %>%
  rename(`Proporção de alfabetizados` = prop_alf) %>%
  select(-1) %>%
  as.matrix() %>%
  cor() %>%
  corrplot("pie", type = "lower", tl.col = "black")


# Then
dev.off()


idade_int %>%
  left_join(raca_int) %>%
  left_join(alf_int) %>%
  left_join(renda_int) %>%
  rename(`Proporção de alfabetizados` = prop_alf) %>%
  select(-1) %>%
  names() %>%
  paste(collapse = ", ")

r <- idade_int %>%
  left_join(raca_int) %>%
  left_join(alf_int) %>%
  left_join(renda_int) %>%
  rename(`Proporção de alfabetizados` = prop_alf) %>%
  select(-1) %>%
  as.matrix() %>%
  cor(method = "pearson")

a <- determinant(r)$modulus

log(a[1])

r <- idade_int %>%
  left_join(raca_int) %>%
  left_join(alf_int) %>%
  left_join(renda_int) %>%
  rename(`Proporção de alfabetizados` = prop_alf) %>%
  select(-1) %>% 
# select(-all_of(c("0 a 4 anos", "Branca", "Sem rendimento"))) %>%
  as.matrix() %>%
  rcorr(type = "pearson")

determinant(as.matrix(r$r))$modulus

R <- structure(as.vector(r$r), .Dimnames = names(r$r), .Dim = dim(r$r))
cortest.bartlett(as.matrix(r$r), n = 39) # bartlett test


##* K-means -----------------------------------------

dados <- idade_int %>%
  left_join(raca_int) %>%
  left_join(alf_int) %>%
  left_join(renda_int) %>%
  rename(`Proporção de alfabetizados` = prop_alf)

dados %>% 
  select(-1) %>% 
  apply(2, shapiro.test)

df <- dados  %>%
  select(-all_of(c("0 a 4 anos", "Branca", "Sem rendimento")))%>%
  mutate_at(2:26, ~ as.vector(scale(.))) # applying zscore

df %>%
  select(-1) %>%
  fviz_nbclust(kmeans, method = "wss")

p1 <- df %>%
  select(-1) %>%
  fviz_nbclust(kmeans, method = "silhouette") + # using silhouette method
  # labs(
  #   x = "Número de agrupamentos", y = "Largura média da silhueta",
  #   title = "Número ótimo de agrupamentos"
  # ) +
  geom_vline(xintercept = 6, linetype = "dotted") +
  labs(title = "A")+
  #scale_y_continuous(labels = virgula) +
  tema +
  theme(
    plot.title = element_text(size = 16, face = "bold")
  )

set.seed(123)
# testing 2 clusters
k2 <- df %>%
  select(-1) %>%
  kmeans(2)
k2$cluster

set.seed(54623)
# Teste com 6
k6 <- df %>%
  select(-1) %>%
  kmeans(6, iter.max = 500, nstart = 50) # running kmeans
k6$cluster

## PCA ---------------------------------------------------------------------

names(dados)

# running PCA
pca <- dados  %>%
  select(-all_of(c("0 a 4 anos", "Branca", "Sem rendimento")))%>% 
  select(-id_mun) %>%
  as.matrix() %>% 
  psych::principal(
    r = .,
    nfactors = ncol(.),
    scores = TRUE,
    rotate = "none"
  )

pca$Vaccounted
dl  <- data.frame(Valores = pca$values,  Var = as.vector(pca$Vaccounted[4,]),
PC = as.character(seq(1,25)))

dl %>%
pivot_longer(1:2, values_to = "valor", names_to = "variavel") %>%
mutate(
  PC = factor(PC, levels = as.character(seq(1,25))),
  variavel = ifelse(variavel == "Var", "Proportion of variance explained", "Eigenvalue")
) %>%
ggplot()+
geom_col(aes(x = PC, y = valor, fill = variavel))+
geom_hline(data = data.frame(variavel = "Eigenvalue", t = 1), aes(yintercept = t),
linewidth = 0.9, linetype = "dashed")+
facet_wrap(.~variavel, nr = 2, scales = "free_y")+
scale_fill_manual(values = cores)+
labs(x = "Principal Component")+
tema+
theme(
  legend.position = "none",
  axis.title.y = element_blank(),
  strip.text = element_text(size = 14, face = "bold")
)
ggsave("plots/Figure1.svg", device = "svg", dpi = 300, width = 6, height = 4)


k6 <- df %>%
  select(-1) %>%
  kmeans(6, iter.max = 500, nstart = 50)
k6$cluster

# 5 variáveis
df_f <- as.data.frame(pca$scores[, 1:5])
df_f$id_mun <- dados$id_mun
df_f$cluster <- as.factor(k6$cluster)


hull <- df_f %>%
  group_by(cluster) %>%
  slice(chull(PC1, PC2))


p2 <- df_f %>%
  as.data.frame() %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2, color = cluster, fill = cluster), shape = 21, size = 2, stroke = 1) +
  scale_color_manual(values = paleta) +
  geom_polygon(data = hull, aes(x = PC1, y = PC2, color = cluster, fill = cluster), alpha = 0) +
  scale_fill_manual(values = scales::alpha(c(paleta), 0.5)) +
  labs(color = "Cluster", fill = "Cluster", title = "B") +
  tema+
  theme(
    plot.title = element_text(size = 16, face = "bold")
  )




# joining silhouette e PCA visualization
ggpubr::ggarrange(p1,p2)
ggsave("plots/Figure2.png", device = "png", dpi = 300, width = 8, height = 4)

df_mun <- df_f

dir.create("outputs")
write.csv(df_mun, "outputs/result_pca_mun.csv")

## Plot standardized differences     ------------------------

pca_mun <- read.csv("outputs/result_pca_mun.csv")

dados$cluster <- as.factor(pca_mun$cluster)
dim(dados)

niveis <- names(dados)[-c(1, 30)]

df_aux <- dados %>%
  pivot_longer(2:29, names_to = "Variável", values_to = "Valor") %>%
  group_by(Variável) %>%
  summarise(Média = mean(Valor), desv = sd(Valor))


dfref <- read_any(db, "ref_idades") %>%
  select(idade_pt, idade_en) %>%
  rename(PT = 1, EN = 2) %>%
  bind_rows(
    read_any(db, "ref_raca") %>%
    select(raca_pt, raca_en) %>%
    rename(PT = 1, EN = 2)
  ) %>%
    bind_rows(
      read_any(db, "ref_renda") %>%
      select(renda_pt, renda_en) %>%
      rename(PT = 1, EN = 2)
    ) %>%
  bind_rows(
    data.frame(
      PT = "Proporção de alfabetizados", EN = "Proportion of literate"
    )
  )

  levels_r = dfref$EN
  names(levels_r) <- dfref$PT

p <- dados %>%
  pivot_longer(2:29, names_to = "Variável", values_to = "Valor") %>%
  group_by(cluster, Variável) %>%
  summarise(Média_grupo = mean(Valor)) %>%
  left_join(df_aux) %>%
  left_join(dfref, by = c("Variável" = "PT")) %>%
  mutate(
    Desvio = (Média_grupo - Média) / desv
  ) %>%
  mutate(
    EN = factor(EN, levels = levels_r[niveis])
  ) %>%
  ggplot() +
  geom_col(aes(x = Desvio, y = EN, fill = cluster), color = "black") +
  facet_wrap(. ~ cluster, scales = "free_x", nrow = 1) +
  labs(color = "Cluster", fill = "Cluster", x = "Standardized difference of means", y = "Variable")+
  #scale_x_continuous(labels = virgula) +
  rcartocolor::scale_color_carto_d(palette = "Bold")+
  rcartocolor::scale_fill_carto_d(palette = "Bold")+
  tema +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1.0, vjust = 0.5),
    strip.text = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )
ggsave("plots/Figure3.svg", plot = p, device = "svg", dpi = 300, width = 9, height = 6)

## Mapa grupos   ------------------------


br <- sf::read_sf("../Dados/GIS/BRMUE250GC_SIR.shp")
br

sp <- br %>%
  filter(grepl("^35", CD_GEOCMU)) %>%
  right_join(mun_int, by = c("CD_GEOCMU" = "codibge"))

plot(sp$geometry)

names(df)


dados %>%
  select(id_mun, cluster) %>%
  left_join(sp) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = cluster)) +
  rcartocolor::scale_fill_carto_d(palette = "Bold")+
  labs(fill = "Cluster", x = "Longitude", y = "Latitude") +
  #scale_x_continuous(labels = virgula) +
  #scale_y_continuous(labels = virgula) +
  # scale_fill_manual(values = scales::alpha(c(paleta), 1))+
  tema +
  # guides(fill = guide_colourbar(
  #   title.position = "top",
  #   title.hjust = 0.5, barwidth = 10, barheight = 1
  # )) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )



ggsave("plots/Figure4.svg", device = "svg", dpi = 300, width = 6, height = 4)



dados %>%
  select(id_mun, cluster) %>%
  left_join(sp) %>%
  select(cidade, cluster) %>% View






# CNES --------------------------------------------------------------------
# reading facility data
odbc::dbListTables(db)

cities <- read_any(db, "ibge_cidades")
estabelecimento <- read_any(db, "estabelecimento")
cnes <- read_any(db, "cnes_data_year")

pop <- odbc::dbGetQuery(db, "select id_mun, total from ibge_idade")

glimpse(cnes)



df1 <- cnes %>% 
  group_by(id_cnes) %>% 
  summarise_at(vars(starts_with("qt")), list(~ mean(., na.rm = TRUE)))


df2 <- cnes %>% 
  group_by(id_cnes) %>%
  summarise_at(vars(c(vinc_sus)),
               list(~ max(as.integer(.), na.rm = TRUE))
  )


cnes <- full_join(df1, df2)


nrow(cnes)

names(cities)
names(estabelecimento)
names(cnes)


df <- cnes %>%
  left_join(estabelecimento, by = c("id_cnes" = "idcnes")) %>%
  left_join(cities, by = c("id_mun" = "idmun"))

names(df)

df %>%
  group_by(id_mun, cidade, ano) %>%
  summarise(
    n = length(unique(id_cnes))
  ) %>%
  left_join(pop, by = "id_mun") %>%
  ungroup() %>%
  mutate(
    prop = n * 100000 / total,
    ano = factor(ano)
  ) %>%
  ggplot() +
  geom_histogram(aes(x = prop, fill = ano), color = "black") +
  labs(x = "Número de estabelecimentos por 100 mil habitantes", y = "Contagem") +
  facet_wrap(. ~ ano) +
  scale_fill_manual(values = scales::alpha(c(paleta), 1)) +
  tema +
  theme(
    legend.position = "none"
  )


## GIS ---------------------------------------------------------------------
# 
# sp <- sf::read_sf("../Dados/GIS/cidades_SP.shp")
# 
# plot(sp$geometry)
# 
# br <- sf::read_sf("../Dados/GIS/BRMUE250GC_SIR.shp")
# br
# 
# sp <- br %>%
#   filter(grepl("^35", CD_GEOCMU))
# 
# mun_int

sp <- sf::read_sf("GIS/SP_cities.shp")
plot(sp$geometry)
# In case some one wants t oreproduce that
# dir.create("GIS")
# sf::st_write(sp, "GIS/SP_cities.shp")



names(df)

pm <- df %>%
  filter(id_mun %in% mun_int$id_mun) %>% 
  group_by(id_mun, cidade, codibge, ano) %>%
  summarise(
    n = length(unique(id_cnes))
  ) %>%
  left_join(pop, by = "id_mun") %>%
  ungroup() %>%
  left_join(sp, by = c("codibge" = "CD_GEOCMU")) %>%
  mutate(
    prop = n * 100000 / total,
    ano = factor(ano)
  ) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) +
  facet_wrap(. ~ ano) +
  scale_fill_viridis_c() +
  labs(fill = "Número de estabelecimentos por 100 mil habitantes") +
  # scale_fill_manual(values = scales::alpha(c(paleta), 1))+
  tema +
  guides(fill = guide_colourbar(
    title.position = "top",
    title.hjust = 0.5, barwidth = 10, barheight = 1
  )) +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.direction = "horizontal",
    strip.text = element_text(size = 14, face = "bold")
  )

# 
# 
# ggsave("plots/map_cnes.png", device = "png", dpi = 300, width = 9, height = 6)
# 



df %>%
  group_by(id_mun, cidade, codibge, ano) %>%
  summarise(
    n = length(unique(id_cnes))
  ) %>%
  left_join(pop, by = "id_mun") %>%
  ungroup() %>%
  left_join(sp, by = c("codibge" = "CD_GEOCMU")) %>%
  mutate(
    prop = n * 100000 / total,
    ano = factor(ano)
  ) %>%
  arrange(desc(prop))

## Série temporal ----------------------------------------------------------



df %>%
  filter(id_mun %in% mun_int$id_mun) %>% 
  group_by(id_mun, cidade, codibge, ano) %>%
  summarise(
    n = length(unique(id_cnes))
  ) %>%
  left_join(pop, by = "id_mun") %>%
  left_join(
    pca_mun %>%
      select(id_mun, cluster)
  ) %>%
  ungroup() %>%
  mutate(
    prop = n * 100000 / total,
    ano = factor(ano)
  ) %>%
  ggplot(aes(x = factor(cluster), y = prop, color = factor(cluster)))+
  geom_jitter(alpha = 0.5, width = 0.2)+
  geom_boxplot(width = 0.2, linewidth = 1.1, alpha = 0.0)+
  labs(x = "Cluster", y = "Number of facilities\nper\n100,000 inhabitants")+
  rcartocolor::scale_color_carto_d(palette = "Bold")+
  tema+
  theme(
    legend.position = "none"
  )

  ggsave("plots/Figure5.png", device = "png", dpi = 300, width = 5, height = 4.0)
  ggsave("plots/Figure5.svg", device = "svg", dpi = 300, width = 5, height = 4.0)



# performing some statistical tests

df %>%
  filter(id_mun %in% mun_int$id_mun) %>% 
  group_by(id_mun, cidade, codibge, ano) %>%
  summarise(
    n = length(unique(id_cnes))
  ) %>%
  left_join(pop, by = "id_mun") %>%
  ungroup() %>%
  mutate(
    prop = n * 100000 / total,
    ano = factor(ano)
  ) %>%
  select(prop, ano) %>% 
  group_by(ano) %>% 
  summarise(
    pvalue = shapiro.test(prop)$p.value
  )

dd <- df %>%
  filter(id_mun %in% mun_int$id_mun) %>% 
  group_by(id_mun, cidade, codibge, ano) %>%
  summarise(
    n = length(unique(id_cnes))
  ) %>%
  left_join(pop, by = "id_mun") %>%
  ungroup() %>%
  mutate(
    prop = n * 100000 / total,
    ano = factor(ano)
  ) %>%
  select(cidade, prop, ano) %>% 
  pivot_wider(names_from = "ano", values_from = prop)

# matrixTests::col_wilcoxon_twosample(dd[,c(2:(ncol(dd)-1))], dd[,c(3:ncol(dd))])

n <- names(dd)[-1]

wilcox.test(dd[["2015"]], dd[["2020"]], paired = TRUE)
