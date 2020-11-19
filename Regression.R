library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(egg)

load("regression.rda")

social_regression2 <- social_regression %>%
  bind_rows

solitary_regression2 <- solitary_regression %>%
  bind_rows

social_regression3 <- social_regression2 %>%
  filter(beta!="(Intercept)") %>%
  mutate(
    est=ifelse(p.value < 0.05/(17*16*2), est, NA),
    beta=gsub("TRUE", "", beta)
  )
  
social_regression3_self <- social_regression3 %>%
  filter(!grepl("partner", beta))

social_regression3_partner <- social_regression3 %>%
  filter(grepl("partner", beta)) %>%
  mutate(
    beta=gsub("partner", "", beta)
  )

g1 <- ggplot(social_regression3_self) +
  geom_tile(aes(beta, key, fill=est)) +
  scale_x_discrete("Self behavior at time t-1", expand=c(0, 0)) +
  scale_y_discrete("Self behavior at time t", expand=c(0, 0)) +
  scale_fill_gradientn("Regression\ncoefficient", colors=c("red", "gray", "blue"),
                       limits=c(-13, 13),
                       na.value = "white") +
  ggtitle("A. Social") +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    panel.grid = element_blank(),
    
  )
g1

g2 <- ggplot(social_regression3_partner) +
  geom_tile(aes(beta, key, fill=est)) +
  scale_x_discrete("Partner behavior at time t-1", expand=c(0, 0)) +
  scale_y_discrete("Self behavior at time t", expand=c(0, 0)) +
  scale_fill_gradientn("Regression\ncoefficient", colors=c("red", "gray", "blue"),
                       limits=c(-13, 13),
                       na.value = "white") +
  ggtitle("B. Social") +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    panel.grid = element_blank(),
    
  )

g2

solitary_regression3 <- solitary_regression2 %>%
  filter(beta!="(Intercept)") %>%
  mutate(
    est=ifelse(p.value < 0.05/(17*16*2), est, NA),
    beta=gsub("TRUE", "", beta)
  )

solitary_regression3_self <- solitary_regression3 %>%
  filter(!grepl("partner", beta))

solitary_regression3_partner <- solitary_regression3 %>%
  filter(grepl("partner", beta)) %>%
  mutate(
    beta=gsub("partner", "", beta)
  )


g3 <- ggplot(solitary_regression3_self) +
  geom_tile(aes(beta, key, fill=est)) +
  scale_x_discrete("Self behavior at time t-1", expand=c(0, 0)) +
  scale_y_discrete("Self behavior at time t", expand=c(0, 0)) +
  scale_fill_gradientn("Regression\ncoefficient", colors=c("red", "gray", "blue"),
                       limits=c(-13, 13),
                       na.value = "white") +
  ggtitle("C. Solitary") +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    panel.grid = element_blank(),
    
  )

g3

g4 <- ggplot(solitary_regression3_partner) +
  geom_tile(aes(beta, key, fill=est)) +
  scale_x_discrete("Partner behavior at time t-1", expand=c(0, 0)) +
  scale_y_discrete("Self behavior at time t", expand=c(0, 0)) +
  scale_fill_gradientn("Regression\ncoefficient", colors=c("red", "gray", "blue"),
                       limits=c(-13, 13),
                       na.value = "white") +
  ggtitle("D. Solitary") +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    panel.grid = element_blank(),
    
  )

gtot <- ggarrange(g1, g2, g3, g4, nrow=2)
gtot

ggsave("figure_regression.pdf", gtot, width=12, height=12)
