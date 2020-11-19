library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=12, base_family = "Times"))
library(egg)
source("../R/bee_to_ts.R")

datafun <- function(x) {
  lapply(split(x, x$video), function(x) {
    xx <- split(x, x$subject)
    
    x1 <- xx[[1]]
    x2 <- xx[[2]]
    
    y1 <- x1 %>%
      gather(key, value, -video, -time, -subject, -type)
    
    y2 <- x2 %>%
      gather(key, value, -video, -time, -subject, -type)
    
    mycov1 <- x1 %>%
      filter(
        time!=1800
      ) %>%
      mutate(
        time=time+1
      )                #my behavior against mine
    
    mycov2 <- x2 %>%
      filter(
        time!=1800
      ) %>%
      mutate(
        time=time+1
      )
    
    pcov1 <- y1 %>%
      mutate(key=paste0("partner", key)) %>%
      filter(
        time!=1800
      ) %>%
      mutate(
        time=time+1
      ) %>%
      spread(key, value) %>%
      ungroup %>%
      select(-subject)
    
    pcov2 <- y2 %>%
      mutate(key=paste0("partner", key)) %>%
      filter(
        time!=1800
      ) %>%
      mutate(
        time=time+1
      ) %>%
      spread(key, value) %>%
      ungroup %>%
      select(-subject)
    
    z1 <- y1 %>%
      merge(mycov1) %>%
      merge(pcov2)
    
    z2 <- y2 %>%
      merge(mycov2) %>%
      merge(pcov1)
    
    finaldata <- bind_rows(z1, z2)
    
    finaldata
  }) %>%
    bind_rows
}

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

social_raw <- read_excel_allsheets("all social bee pairs_events.xlsx")
solitary_raw <- read_excel_allsheets("all solitary bee pairs_events.xlsx") 

social <- social_raw %>%
  lapply(bee_to_ts) %>%
  bind_rows(.id="video") %>%
  mutate(
    type="social"
  )

solitary <- solitary_raw %>%
  lapply(bee_to_ts) %>%
  bind_rows(.id="video") %>%
  mutate(
    type="solitary"
  )

social_spread <- social %>%
  select(-category) %>%
  spread(behavior, status) %>%
  arrange(video, subject, time) %>%
  group_by(video, subject)

solitary_spread <- solitary %>%
  select(-category) %>%
  spread(behavior, status) %>%
  arrange(video, subject, time) %>%
  group_by(video, subject)

social_regression_data <- datafun(social_spread)
solitary_regression_data <- datafun(solitary_spread)

social_regression <- lapply(split(social_regression_data, social_regression_data$key), function(x) {
  print(x$key[1])
  x2 <- x
  colnames(x2) <- gsub(" ", "", colnames(x2))
  colnames(x2) <- gsub("-", "", colnames(x2))
  
  gfit <- glm(value~1+Antennation+Back+BacktoBack+
                CPosture+Charge+Chases+Follow+HdBk+
                HdHd+Lunges+MandibleLock+MHA+Passes+Push+Sidebyside+
                Uturn+Withdraw+partnerAntennation+partnerBack+partnerBacktoBack+
                partnerCPosture+partnerCharge+partnerChases+partnerFollow+
                partnerHdBk+partnerHdHd+partnerLunges+partnerMandibleLock+partnerMHA+
                partnerPasses+partnerPush+partnerSidebyside+partnerUturn+partnerWithdraw,
              data=x2, family=binomial,
              control = list(maxit = 50))
  
  ss <- summary(gfit)
  
  data.frame(
    key=x$key[1],
    beta=rownames(ss$coefficients),
    est=ss$coefficients[,1],
    p.value=ss$coefficients[,4]
  ) %>%
    bind_rows
  
})

solitary_regression <- lapply(split(solitary_regression_data, solitary_regression_data$key), function(x) {
  print(x$key[1])
  x2 <- x
  colnames(x2) <- gsub(" ", "", colnames(x2))
  colnames(x2) <- gsub("-", "", colnames(x2))
  
  gfit <- glm(value~1+Antennation+Back+BacktoBack+
                CPosture+Charge+Chases+Follow+HdBk+
                HdHd+Lunges+MandibleLock+MHA+Passes+Push+Sidebyside+
                Uturn+Withdraw+partnerAntennation+partnerBack+partnerBacktoBack+
                partnerCPosture+partnerCharge+partnerChases+partnerFollow+
                partnerHdBk+partnerHdHd+partnerLunges+partnerMandibleLock+partnerMHA+
                partnerPasses+partnerPush+partnerSidebyside+partnerUturn+partnerWithdraw,
              data=x2, family=binomial,
              control = list(maxit = 50))
  
  ss <- summary(gfit)
  
  data.frame(
    key=x$key[1],
    beta=rownames(ss$coefficients),
    est=ss$coefficients[,1],
    p.value=ss$coefficients[,4]
  ) %>%
    bind_rows
  
})

save("social_regression", "solitary_regression", file="regression.rda")
