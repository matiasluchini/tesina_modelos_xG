#---- Establezco el tema ----
library(tidyverse)
library(ggthemes)
library(paletteer)
library(patchwork)
library(ggdist)

tema_mio <- function() {
  theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_blank()
    )
}

W <- 6
H <- 4

cols <- c("#9BBB59", "#8064A2", "#F3A447","#4BACC6")

#---- Efecto de la distancia sobre log-odds, odds y probs ---- 

x1 <- seq(0, 100, by = 1)
beta0 <- 4
beta1 <- -0.1

df <- data.frame(x1 = x1,
                 log_odds = beta0 + beta1 * x1)

df$odds <- exp(df$log_odds)
df$pi <- 1 / (1 + exp(-df$log_odds))

p1 <- ggplot(df, aes(x1, log_odds)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Distancia",
       y = "Log odds de gol") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), 
                     labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 12))

p2 <- ggplot(df, aes(x1, odds)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Distancia",
       y = "Odds de gol") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), 
                     labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 12))

p3 <- ggplot(df, aes(x1, pi)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Distancia",
       y = "Probabilidad de gol") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), 
                     labels = scales::label_number(decimal.mark = ",")) +
  tema_mio() + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 12))

grafico_combinado <- p1 | p2 | p3

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/relacion_distancia_logit.pdf", 
       plot = grafico_combinado, 
       width = 12, 
       height = 4, 
       units = "in")

#---- Ejemplos de distribuciones Beta ----

params <- tibble(alpha = c(1, 1, 2, 1, 5, 20, 5, 2, 4),
                 beta  = c(4, 2, 5, 1, 5, 20, 2, 1, 1))

df <- expand_grid(x = seq(0, 1, length.out = 1000),
                  params) %>%
  mutate(densidad = dbeta(x, alpha, beta),
         modelo = paste0("Beta(", alpha, ", ", beta, ")"))

df <- df %>%
  mutate(modelo = factor(modelo,
                         levels = c("Beta(1, 4)",
                                    "Beta(1, 2)",
                                    "Beta(2, 5)",
                                    "Beta(5, 5)",
                                    "Beta(1, 1)",
                                    "Beta(20, 20)",
                                    "Beta(5, 2)",
                                    "Beta(2, 1)",
                                    "Beta(4, 1)")))

betas <- ggplot(df, aes(x, densidad)) +
  geom_line(linewidth = 1, color = "black") +
  facet_wrap(~ modelo, ncol = 3) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0","0,25", "0,5", "0,75","1")) +
  tema_mio() + 
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text  = element_text(size = 8)) +
  labs(x = expression(pi), y = expression(p(pi)))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/posibles_beta.pdf", 
       plot = betas, 
       width = W, 
       height = H, 
       units = "in")

#---- Traceplots de ejemplo ----

set.seed(123)

n_iter   <- 4000
n_chains <- 4

chains_top <- replicate(n_chains,
                        rnorm(n_iter, mean = 0.6, sd = 0.15),
                        simplify = FALSE)

means_middle <- c(0.70, 0.75, 0.80, 0.85)

chains_middle <- lapply(means_middle,
                        function(mu) rnorm(n_iter, mean = mu, sd = 0.01))

chains_bottom <- list(cumsum(rnorm(n_iter, mean =  0.002, sd = 0.05)),
                      cumsum(rnorm(n_iter, mean = -0.002, sd = 0.05)),
                      sin(seq(0, 20, length.out = n_iter)) + rnorm(n_iter, 0, 0.1),
                      cumsum(rnorm(n_iter, mean = 0, sd = 0.08)) + seq(-2, 2, length.out = n_iter))

to_df <- function(chains) {
  data.frame(iter  = rep(seq_len(n_iter), times = n_chains),
             value = unlist(chains),
             chain = factor(rep(seq_len(n_chains), each = n_iter)))
}

df_top <- to_df(chains_top)
df_middle <- to_df(chains_middle)
df_bottom <- to_df(chains_bottom)

p_top <- ggplot(df_top, aes(iter, value, color = chain)) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(values = cols) +
  labs(y = expression(theta)) +
  tema_mio() +
  theme(legend.position = "none",
        axis.title.x    = element_blank(),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())

p_middle <- ggplot(df_middle, aes(iter, value, color = chain)) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(values = cols) +
  labs(y = expression(theta)) +
  tema_mio() +
  theme(legend.position = "none",
        axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x    = element_blank()) 

p_bottom <- ggplot(df_bottom, aes(iter, value, color = chain)) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(values = cols) +
  labs(x = "Iteración",
       y = expression(theta)) +
  tema_mio() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

traceplots <- p_top / p_middle / p_bottom

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/traceplots.pdf", 
       plot = traceplots, 
       width = W, 
       height = H, 
       units = "in")

#---- Grafico de Autocorrelacion de ejemplo ----
set.seed(123)

n_iter <- 1000

acf_low <- rnorm(n_iter)

phi <- 0.95
acf_high <- numeric(n_iter)
acf_high[1] <- rnorm(1)

for (t in 2:n_iter) {
  acf_high[t] <- phi * acf_high[t - 1] + rnorm(1, sd = 0.3)
}

acf_low_obj  <- acf(acf_low, lag.max = 22, plot = FALSE)
acf_high_obj <- acf(acf_high, lag.max = 22, plot = FALSE)

df_low <- tibble(lag = acf_low_obj$lag[, , 1],
                 acf = acf_low_obj$acf[, , 1])

df_high <- tibble(lag = acf_high_obj$lag[, , 1],
                  acf = acf_high_obj$acf[, , 1])

conf <- 1.96 / sqrt(n_iter)

g1 <- ggplot(df_low, aes(x = lag, y = acf)) +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-conf, conf), linetype = "dashed") +
  labs(x = "Rezago",
       y = "ACF") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  coord_cartesian(ylim = c(-0.025, 1)) +
  tema_mio() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))

g2 <- ggplot(df_high, aes(x = lag, y = acf)) +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-conf, conf), linetype = "dashed") +
  labs(x = "Rezago",
       y = NULL) +
  coord_cartesian(ylim = c(-0.025, 1)) +
  tema_mio() + 
  theme(axis.text.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.ticks.y = element_blank())

acfs <- g1 + g2

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/acf_ejemplos.pdf", 
       plot = acfs, 
       width = W+2, 
       height = H, 
       units = "in")

#---- Posibles betas para priors ----

x <- seq(0, 1, length.out = 2000)

df_priors <- bind_rows(tibble(beta = x,
                              densidad = dunif(x, min = 0, max = 1),
                              tipo = "Uniforme"),
                       tibble(beta = x,
                              densidad = dbeta(x, 2, 2),
                              tipo = "Campana\nalta variabilidad"),
                       tibble(beta = x,
                              densidad = dnorm(x, mean = 0.65, sd = 0.075),
                              tipo = "Poca variabilidad\n(media 1.2)"),
                       tibble(beta = x,
                              densidad = dnorm(x, mean = 0.3, sd = 0.075),
                              tipo = "Poca variabilidad\n(media 2)")) %>%
  mutate(tipo = factor(tipo,
                       levels = c("Uniforme",
                                  "Campana\nalta variabilidad",
                                  "Poca variabilidad\n(media 2)",
                                  "Poca variabilidad\n(media 1.2)")))

priors <- ggplot(df_priors, aes(x = beta, y = densidad)) +
  geom_area(fill = "#4F81BD",
            alpha = 0.7) +
  geom_line(linewidth = 0,
            color = "#4F81BD") +
  facet_wrap(~ tipo, ncol = 2) +
  labs(x = expression(theta),
    y = "Credibilidad") +
  tema_mio() +
  theme(strip.text = element_blank(),      
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank(),
        text = element_text(size = 8),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(size = 12),  
        axis.text = element_text(size = 8)) + 
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "0,25", "0,5", "0,75", "1"))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/posibles_priors_beta.pdf", 
       plot = priors, 
       width = W, 
       height = H, 
       units = "in")

#---- Compromiso entre prior y likelihood ----

dbeta2 <- function(x, mu, phi, ncp = 0, log = FALSE){
  var <- mu*(1-mu)/(1+phi)
  shape1 <- mu * (mu*(1-mu)/var - 1)
  shape2 <- (1-mu)*(mu*(1-mu)/var - 1)
  return(dbeta(x, shape1, shape2, ncp, log))
}

plot_bayes <- function(data){
  data %>%
    pivot_longer(cols = c(Prior, Likelihood, starts_with("Posterior"))) %>%
    arrange(name, x) %>%
    separate(name, into = c("facet","color"), sep = "_") %>%
    mutate(color = ifelse(is.na(color), facet, color)) %>%
    mutate(alpha = as.factor(paste0(facet, color))) %>% 
    mutate(color = factor(color,
                          levels = c("Prior", "Likelihood", "Posterior"),
                          labels = c("Prior", "Likelihood", "Posterior")),
           facet = factor(facet,
                          levels = c("Prior", "Likelihood", "Posterior"),
                          labels = c("Prior", "Likelihood", "Posterior"))) %>%
    ggplot() +
    geom_line(aes(x = x, y = value, col = color, group = color, alpha = alpha),
              size = 0) +
    geom_ribbon(aes(x = x, ymin = 0, ymax = value, fill = color, group = color, alpha = alpha)) +
    scale_color_manual(values = c("#4F81BD","#9BBB59","#C0504D")) +
    scale_fill_manual(values = c("#4F81BD","#9BBB59","#C0504D")) +
    scale_alpha_manual(values = c(0.6, 0.2, 0.8, 0.2, 0.6)) +
    facet_wrap(. ~ facet) +
    ylab("Credibilidad") +
    xlab(expression(theta)) +
    tema_mio() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          text = element_text(size = 20),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x  = element_blank(),
          strip.text = element_text(size = 12))
}

# Ejemplo urna
data <- crossing(u = 0:10, nN = 0:10)
N <- 10

data <- data %>%
  mutate(Prior = 1/11,
         Likelihood = choose(10, nN) * (u/10)^nN * (1 - u/10)^(N - nN),
         joint = Prior * Likelihood)

p1 <- tibble(x = seq(0, 1, length.out = 500),
             Prior = dbeta(x, 5, 3),
             Likelihood = dbeta2(x, mu = 0.2, phi = 10),
             Posterior_Posterior = Prior * Likelihood) %>%
  mutate(Prior = Prior / sum(Prior),
         Likelihood = Likelihood / sum(Likelihood),
         Posterior_Posterior = Posterior_Posterior / sum(Posterior_Posterior),
         Posterior_Likelihood = Likelihood,
         Posterior_Prior = Prior) %>%
  plot_bayes() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

p2 <- tibble(x = seq(0, 1, length.out = 500),
             Prior = dbeta(x, 1, 1),
             Likelihood = dbeta(x, 2, 5),
             Posterior_Posterior = Prior * Likelihood) %>%
  mutate(Prior = Prior / sum(Prior),
         Likelihood = Likelihood / sum(Likelihood),
         Posterior_Posterior = Posterior_Posterior / sum(Posterior_Posterior),
         Posterior_Likelihood = Likelihood,
         Posterior_Prior = Prior) %>%
  plot_bayes() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

p3 <- tibble(x = seq(0, 1, length.out = 500),
             Prior = dbeta(x, 3, 2),
             Likelihood = dbeta2(x, mu = 0.6, phi = 15)) %>%
  mutate(Prior = ifelse(x > 0.5 & x < 0.7, 0, Prior),
         Posterior_Posterior = Prior * Likelihood) %>%
  mutate(Prior = Prior / sum(Prior),
         Likelihood = Likelihood / sum(Likelihood),
         Posterior_Posterior = Posterior_Posterior / sum(Posterior_Posterior),
         Posterior_Likelihood = Likelihood,
         Posterior_Prior = Prior) %>%
  plot_bayes()

p1 <- p1 +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

p2 <- p2 +
  theme(strip.text = element_blank(),
        axis.title = element_text(size = 12),
        strip.background = element_blank())

p3 <- p3 +
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 14),
        strip.text = element_blank(),
        strip.background = element_blank())

compromiso <- p1 / p2 / p3

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/compromiso_distribuciones.pdf", 
       plot = compromiso, 
       width = W, 
       height = H, 
       units = "in")

#---- Interaccion entre Distancia y Angulo ----
get_midpoint <- function(x) {
  as.numeric(sub("\\((.+),(.+)\\]", "\\1", x)) + 
    (as.numeric(sub("\\((.+),(.+)\\]", "\\2", x)) - 
       as.numeric(sub("\\((.+),(.+)\\]", "\\1", x))) / 2
}

interaccion_dist_ang <- datos %>%
  mutate(distancia_bin = cut(distancia, breaks = 35),
         angulo_bin = cut(angulo, breaks = 35)) %>%
  group_by(distancia_bin, angulo_bin) %>%
  summarise(total = n(),
            goles = sum(gol),
            tasa_gol = goles / total,
            .groups = "drop") %>%
  mutate(distancia_mid = get_midpoint(as.character(distancia_bin)),
         angulo_mid = get_midpoint(as.character(angulo_bin))) %>% 
  ggplot(aes(x = distancia_mid, y = angulo_mid, fill = tasa_gol)) +
  geom_tile() +
  scale_fill_gradient2(high = "#C0392B", mid = "#F7DC6F", low = "#1F6B3A",
                       midpoint = 0.5,
                       name = "Tasa de gol", 
                       labels = scales::label_number(decimal.mark = ",")) +
  labs(x = "Distancia (en metros)", y = "Ángulo (en grados)") +
  tema_mio() +
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10),
        legend.title = element_text(size = 13, hjust = 0.5, vjust = 1,
                                    margin = margin(b = 8)),
        legend.text = element_text(size = 10))

rm(get_midpoint)

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/interaccion_dist_ang.pdf", 
       plot = interaccion_dist_ang, 
       width = W + 1, 
       height = H, 
       units = "in")

#---- Histogramas para mostrar efecto del expit ----

expit <- function(x) exp(x) / (1 + exp(x))

set.seed(1234)

prior_b0  <- rnorm(10000, mean = 0, sd = 5)
prior_pi  <- expit(prior_b0)

prior_b02 <- rnorm(10000, mean = 0, sd = 0.5)
prior_pi2 <- expit(prior_b02)

pi_breaks <- c(0, 0.25, 0.5, 0.75, 1)
pi_labels <- c("0", "0,25", "0,5", "0,75", "1")

b0_breaks_arriba <- c(-20, -10, 0, 10, 20)
b0_labels_arriba <- c("-20", "-10", "0", "10", "20")

p_b0_1 <- data.frame(b0 = prior_b0) |>
  ggplot(aes(x = b0)) +
  geom_histogram(bins = 25, 
                 fill = "#C0504D", 
                 color = "#C0504D", 
                 alpha = 0.7, 
                 linewidth = 0.2) +
  geom_hline(yintercept = 0, color = "grey90", linewidth = 0.3) +
  labs(x = expression(alpha)) +
  scale_y_continuous(limits = c(0, 1400)) +
  scale_x_continuous(limits = c(-20, 20),
                     breaks = b0_breaks_arriba,
                     labels = b0_labels_arriba) +
  tema_mio() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.text.y  = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p_pi_1 <- data.frame(pi = prior_pi) |>
  ggplot(aes(x = pi)) +
  geom_histogram(bins = 25, 
                 fill = "#F3A447",
                 color = "#F3A447",
                 alpha = 0.7, 
                 linewidth = 0.2) +
  labs(x = "Probabilidad de gol") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = pi_breaks,
                     labels = pi_labels) +
  scale_y_continuous(limits = c(0, 1400)) +
  geom_hline(yintercept = 0, color = "grey90", linewidth = 0.3) +
  tema_mio() + 
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 8),
        #axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

p_b0_2 <- data.frame(b0 = prior_b02) |>
  ggplot(aes(x = b0)) +
  geom_histogram(bins = 25, 
                 fill = "#C0504D", 
                 color = "#C0504D", 
                 alpha = 0.7, 
                 linewidth = 0.2) +
  labs(x = expression(alpha)) +
  scale_y_continuous(limits = c(0, 1400)) +
  scale_x_continuous(limits = c(-2, 2)) +
  geom_hline(yintercept = 0, color = "grey90", linewidth = 0.3) +
  tema_mio() + 
  theme(axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.title.x = element_text(size = 12),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

p_pi_2 <- data.frame(pi = prior_pi2) |>
  ggplot(aes(x = pi)) +
  geom_histogram(bins = 25, 
                 fill = "#F3A447",
                 color = "#F3A447", 
                 alpha = 0.7, 
                 linewidth = 0.2) +
  labs(x = "Probabilidad de gol") +
  scale_y_continuous(limits = c(0, 1400)) +
  geom_hline(yintercept = 0, color = "grey90", linewidth = 0.3) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = pi_breaks,
                     labels = pi_labels) +
  tema_mio() + 
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x  = element_text(size = 8),
        axis.title.x = element_text(size = 12),
        axis.ticks.y = element_blank())

final_plot <- (p_b0_1 | p_pi_1) / (p_b0_2 | p_pi_2)

final_plot <- wrap_elements(final_plot) +
  labs(tag = "Frecuencia") +
  theme(plot.tag = element_text(size = 12, 
                                angle = 90),
        plot.tag.position = c(-0.02, 0.55),
        plot.margin = margin(l = 15))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/efecto_expit.pdf",
       plot = final_plot,
       width = W ,
       height = H,
       units = "in")

#---- Distribuciones para sigma_jugador ----

sigma <- seq(0.001, 3, length.out = 1000)

densidades <- tibble(sigma_jugador = sigma,
                     Exponencial = dexp(sigma, rate = 1),
                     `Media-normal` = 2 * dnorm(sigma, mean = 0, sd = 1),
                     `Gamma inversa` = {
                       alpha <- 5
                       beta  <- 3
                       (beta^alpha / gamma(alpha)) * sigma^(-alpha - 1) * exp(-beta / sigma)
                     }) %>%
  pivot_longer(cols = -sigma_jugador,
               names_to = "Distribucion",
               values_to = "Densidad") %>%
  mutate(Distribucion = factor(Distribucion,
                               levels = c("Exponencial",
                                          "Media-normal",
                                          "Gamma inversa")))

sigmas <- ggplot(densidades, aes(x = sigma_jugador, y = Densidad)) +
  geom_area(fill = "#4F81BD",
            color = "#4F81BD",
            linewidth = 0,
            alpha = 0.7) +
  facet_wrap(~Distribucion,
             nrow = 1,
             scales = "free_y",
             labeller = labeller(
               Distribucion = as_labeller(
                 c("Exponencial"   = "Exp(1)",
                   "Media-normal"  = 'N^{"+"}*group("(", list(0,1), ")")',
                   "Gamma inversa" = 'paste("Inv-Gamma", group("(", list(5,3), ")"))'),
                 label_parsed))) +
  labs(x = expression(sigma[u]^2),
       y = "Densidad") +
  tema_mio() +
  theme(strip.text = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        text = element_text(size = 12))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/posibles_distrib_sigma.pdf",
       plot = sigmas,
       width = W+3,
       height = H*0.9,
       units = "in")

#---- Ubicacion de los disparos ----
ubicacion_disparos <- datos %>% 
  mutate(x = x - 1, 
         x_std = x / 105 * 100,
         y_std = y / 68 * 100) %>% 
  ggplot(aes(x = x_std, y = y_std)) +
  annotate_pitch(dimensions = pitch_opta, colour = "black", fill = "white") +
  theme_pitch() +
  stat_bin2d(binwidth = c(2.5, 2.5), aes(fill = after_stat(count)), alpha = 0.8) +
  scale_fill_gradient2(name = "Frecuencia",
                       high = "#C0392B", mid = "#F7DC6F", low = "#1F6B3A", 
                       midpoint = 125, 
                       limits = c(0, NA),
                       breaks = c(0, 50, 100, 150, 200, 250)) +
  #coord_fixed() + 
  theme(legend.title = element_text(size = 12, hjust = 0.5, vjust = 1,
                                    margin = margin(b = 8)),
        legend.text = element_text(size = 10), 
        plot.margin = margin(t = 0, b = 0, l = 5, r = 5))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/ubicacion_disparos.pdf",
       plot = ubicacion_disparos,
       width = W,
       height = H,
       units = "in")

library(magick)
install.packages("pdftools")
img <- image_read_pdf("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/ubicacion_disparos.pdf", density = 300)
img_trimmed <- image_trim(img)
image_write(img_trimmed, "C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/ubicacion_disparos.pdf", format = "pdf")

rm(ubicacion_disparos, 
   img, 
   img_trimmed)

#---- Ejemplo distribuciones bimodales ----

library(distributional)

set.seed(1910)

jugadores <- c("Jugador F",
               "Jugador E",
               "Jugador D",
               "Jugador C",
               "Jugador B",
               "Jugador A")

means <- c(0.08, 0.10, 0.12, 0.23, 0.28, 0.27)

draws <- data.frame(
  region = rep(jugadores, each = 2000),
  value = c(rnorm(2000, means[1], 0.03),
            rnorm(2000, means[2], 0.03),
            rnorm(2000, means[3], 0.03),
            rnorm(2000, means[4], 0.03),
            rnorm(2000, means[5], 0.03),
            rnorm(2000, means[6], 0.03)))

ejemplos_bimodales <- ggplot(draws, aes(x = value, y = fct_rev(region))) +
  stat_halfeye(normalize = "groups",
               fill = "#F3A447",
               color = "#F3A447",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  labs(x = "Probabilidad de gol",
       y = NULL) +
  coord_cartesian(xlim = c(-0.06, 0.405)) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(margin = margin(t = 10),
                                    size = 12),
        axis.text.x = element_blank())

draws_combined <- draws %>%
  reframe(value = value, .groups = "drop") %>%
  mutate(grupo = "Nuevo jugador")

normal_ref <- data.frame(
  grupo = unique(draws_combined$grupo),
  dist  = dist_normal(mean(draws_combined$value), 0.07)
)

bimodal <- ggplot(draws_combined, aes(x = value, y = grupo)) +
  stat_halfeye(normalize = "groups",
               fill = "#F3A447",
               color = "#F3A447",
               alpha = 0.6,
               .width = 0.9,
               point_interval = mean_qi,
               interval_color = "black",
               point_color = "black") +
  stat_slab(data = normal_ref,
            aes(y = grupo, dist = dist),
            inherit.aes = FALSE,
            normalize = "groups",
            fill = NA,
            color = "grey50",
            linetype = "dashed",
            linewidth = 0.7,
            alpha = 1) +
  labs(x = "Probabilidad de gol",
       y = NULL) +
  coord_cartesian(xlim = c(-0.06, 0.405)) +
  scale_y_discrete(expand = expansion(add = c(0.1, 0.2))) +
  tema_mio() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 8))

combinado <- (ejemplos_bimodales + labs(x=NULL)) / bimodal +
  plot_layout(heights = c(2, 1.3)) 

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/bimodales.pdf",
       plot = combinado,
       width = W,
       height = H*1.2,
       units = "in")

rm(jugadores, 
   means, 
   draws, 
   draws_combined, 
   normal_ref, 
   ejemplos_bimodales,
   bimodal, 
   combinado)

#---- CRPS teorico ----

set.seed(123)

pred <- round(rgamma(10000, shape = 6, scale = 1.8))
obs  <- 15

cdf <- tibble(x = pred) %>%
  count(x) %>%
  complete(x = min(x):max(x), fill = list(n = 0)) %>%
  mutate(p = n / sum(n),
         F = cumsum(p),
         x_next = lead(x, default = max(x) + 1))

p1 <- ggplot(tibble(pred = pred), aes(pred)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 2,
                 boundary = -0.5,
                 fill = "#F3A447",
                 alpha = 0.7,
                 color = "#F3A447") +
  geom_hline(yintercept = 0, color = "grey90", linewidth = 0.3) +
  geom_vline(xintercept = obs,
             colour = "gray50",
             linetype = "dashed",
             linewidth = 1) +
  scale_x_continuous(breaks = seq(0, max(pred), length.out = 6),
                     labels = scales::number(seq(0, 0.5, length.out = 6),
                                             decimal.mark = ",")) +
  labs(y = NULL,
       x = NULL) +
  tema_mio() + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.ticks.y = element_blank())

p2 <- ggplot() +
  geom_rect(data = filter(cdf, x < obs),
            aes(xmin = x,
                xmax = pmin(x_next, obs),
                ymin = 0,
                ymax = F),
            fill = "#F3A447",
            alpha = 0.45) +
  geom_rect(data = filter(cdf, x >= obs),
            aes(xmin = x,
                xmax = x_next,
                ymin = F,
                ymax = 1),
            fill = "#F3A447",
            alpha = 0.45) +
  geom_step(data = cdf,
            aes(x, F),
            direction = "hv",
            colour = "#F3A447",
            linewidth = 1) +
  geom_vline(xintercept = obs,
             colour = "gray50",
             linetype = "dashed",
             linewidth = 1) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, max(pred), length.out = 6),
                     labels = scales::number(seq(0, 0.5, length.out = 6),
                                             decimal.mark = ",")) +
  labs(y = NULL,
       x = NULL) + 
  tema_mio() + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.ticks.y = element_blank())

library(cowplot)

g <- plot_grid(p1, p2, ncol = 2, align = "h")

combinado <- ggdraw() +
  draw_plot(g, y = 0.08, height = 0.92) +
  draw_text("X",
            x = 0.5,
            y = 0.035,
            size = 16,
            fontface = "italic")

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/crps_teorico.pdf",
       plot = combinado,
       width = W + 2,
       height = H,
       units = "in")

rm(combinado, 
   g, 
   p1, 
   p2, 
   pred, 
   obs, 
   cdf)

#---- Ver si poner esto para los intervalos -----

expit <- function(x) exp(x) / (1 + exp(x))

set.seed(1234)

prior_b0  <- rnorm(10000, mean = -2, sd = 0.5)
prior_pi  <- expit(prior_b0)

pi_breaks <- c(0, 0.25, 0.5, 0.75, 1)
pi_labels <- c("0", "0,25", "0,5", "0,75", "1")

b0_breaks_arriba <- c(-4, -3, -2, -1, 0)
b0_labels_arriba <- c("-4", "-3", "-2", "-1", "0")

p_b0_1 <- data.frame(b0 = prior_b0)  %>% 
  ggplot(aes(x = b0)) +
  geom_histogram(bins = 25, 
                 fill = "#C0504D", 
                 color = "#C0504D", 
                 alpha = 0.7, 
                 linewidth = 0.2) +
  geom_hline(yintercept = 0, color = "grey90", linewidth = 0.3) +
  labs(x = expression(alpha)) +
  scale_x_continuous(limits = c(-4, 0),
                     breaks = b0_breaks_arriba,
                     labels = b0_labels_arriba) +
  scale_y_continuous(limits = c(0, 1400)) +
  tema_mio() + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 11),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

p_pi_1 <- data.frame(pi = prior_pi) %>% 
  ggplot(aes(x = pi)) +
  geom_histogram(bins = 25, 
                 fill = "#F3A447",
                 color = "#F3A447",
                 alpha = 0.7, 
                 linewidth = 0.2) +
  labs(x = "Probabilidad de gol") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = pi_breaks,
                     labels = pi_labels) +
  geom_hline(yintercept = 0, color = "grey90", linewidth = 0.3) +
  tema_mio() + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x  = element_text(size = 11),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

final_plot <- (p_b0_1 | p_pi_1) %>% 
  wrap_elements() +
  labs(tag = "Frecuencia") +
  theme(plot.tag = element_text(size = 16, 
                                angle = 90),
        plot.tag.position = c(-0.01, 0.55),
        plot.margin = margin(l = 15))

ggsave("C:/Users/Usuario/Desktop/Tesis/Imagenes y Figuras/mostrar_intervalo.pdf",
       plot = final_plot,
       width = W + 2,
       height = H,
       units = "in")

