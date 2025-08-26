# R Thornley
# 02/06/2025
# Demo data tidy in wide format for field checking

library(tidyverse)

# species: 
# 1) PV
# 2) HN
# 3) AE
# 4) TA
# 5) BS
# 6) BP
# 7) LC
# 8) OV

PV <- read_csv("data/demo_test/Wytham_PV_T1_T2.csv")
HN <- read_csv("data/demo_test/Wytham_HN_T1_T2.csv")
AE <- read_csv("data/demo_test/Wytham_AE_T1_T2.csv")
TA <- read_csv("data/demo_test/Wytham_TA_T1_T2.csv")
BS <- read_csv("data/demo_test/Wytham_BS_T1_T2.csv")
BP <- read_csv("data/demo_test/Wytham_BP_T1_T2.csv")
LC <- read_csv("data/demo_test/Wytham_LC_T1_T2.csv")
OV <- read_csv("data/demo_test/Wytham_OV_T1_T2.csv")

dat <- BP

make_ts_demo <- function(dat) {
  
  height <- dat %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Height, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Height) %>% 
  arrange(id) %>% rename(T1_height = T1, T2_height = T2) %>%
  add_column(T3_height = NA)
  
  width_1 <- 
  dat %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Width_1, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_1) %>% 
  arrange(id) %>% rename(T1_width_1 = T1, T2_width_1 = T2) %>%
  add_column(T3_width_1 = NA)
  
  width_2 <- 
  dat %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Width_2, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_2) %>% 
  arrange(id) %>% rename(T1_width_2 = T1, T2_width_2 = T2) %>%
  add_column(T3_width_2 = NA)
  
  leaves <- 
  dat %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Number_leaves, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Number_leaves) %>% 
  arrange(id) %>% rename(T1_leaves = T1, T2_leaves = T2) %>%
  add_column(T3_leaves = NA)
  
  flowers <- 
  dat %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Flower_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Flower_head) %>% 
  arrange(id) %>% rename(T1_flower = T1, T2_flower = T2) %>%
  add_column(T3_flower = NA)

all <-
  height %>% left_join(width_1) %>% 
  left_join(width_2) %>% 
  left_join(leaves) %>% 
  left_join(flowers)

}

# species: 
# 1) PV
# 2) HN
# 3) AE
# 4) TA
# 5) BS
# 6) BP
# 7) LC
# 8) OV

PV <- make_ts_demo(PV)
PV$taxa <- "PV"
PV <- PV %>% select(id, taxa, T1_height:T3_flower)
PV
write_csv(PV, "data/PV_demo_TS.csv")

HN <- make_ts_demo(HN)
HN$taxa <- "HN"
HN <- HN %>% select(id, taxa, T1_height:T3_flower)
HN
write_csv(HN, "data/HN_demo_TS.csv")

AE <- make_ts_demo(AE)
AE$taxa <- "AE"
AE <- AE %>% select(id, taxa, T1_height:T3_flower)
AE
write_csv(AE, "data/AE_demo_TS.csv")

TA <- make_ts_demo(TA)
TA$taxa <- "TA"
TA <- TA %>% select(id, taxa, T1_height:T3_flower)
TA
write_csv(TA, "data/TA_demo_TS.csv")

BS <- make_ts_demo(BS)
BS$taxa <- "BS"
BS <- BS %>% select(id, taxa, T1_height:T3_flower)
BS
write_csv(BS, "data/BS_demo_TS.csv")

OV <- make_ts_demo(OV)
OV$taxa <- "OV"
OV <- OV %>% select(id, taxa, T1_height:T3_flower)
OV
write_csv(OV, "data/OV_demo_TS.csv")

# this is also problematic with this code as BP is measured as the area of a quadrat
height <- BP %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Height, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Height) %>% 
  arrange(id) %>% rename(T1_height = T1, T2_height = T2) %>%
  add_column(T3_height = NA)
width_1 <- BP %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Width_1, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_1) %>% 
  arrange(id) %>% rename(T1_width_1 = T1, T2_width_1 = T2) %>%
  add_column(T3_width_1 = NA)
BP <- height %>% left_join(width_1)
BP$taxa <- "BP"
BP <- BP %>% select(id, taxa, T1_height:T3_width_1)
write_csv(BP, "data/BP_demo_TS.csv")

# this is problematic at the moment as I changed the size approximation approach 
# between the two time points
LC
height <- LC %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Height, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Height) %>% 
  arrange(id) %>% rename(T1_height = T1, T2_height = T2) %>%
  add_column(T3_height = NA)
width_1 <- LC %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Width_1, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_1) %>% 
  arrange(id) %>% rename(T1_width_1 = T1, T2_width_1 = T2) %>%
  add_column(T3_width_1 = NA)
flowers <- LC %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Flower_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Flower_head) %>% 
  arrange(id) %>% rename(T1_flower = T1, T2_flower = T2) %>%
  add_column(T3_flower = NA)


LC <- height %>% left_join(width_1)
LC$taxa <- "LC"
LC <- LC %>% select(id, taxa, T1_height:T3_width_1)
LC
write_csv(LC, "data/LC_demo_TS.csv")

################################################################################
# Hazelrigg
# RR
# RA
# CA
# AO

RR <- read_csv("data/demo_test/Hazelrigg_RR_T1.csv")
AO <- read_csv("data/demo_test/Hazelrigg_AO_T1.csv")
CA <- read_csv("data/demo_test/Hazelrigg_CA_T1.csv")
RA <- read_csv("data/demo_test/Hazelrigg_RA_T1.csv")

names(AO)
dat <- RR
dat <- AO
dat <- CA

make_ts_demo_Hazel <- function(dat) {
  
  height <- dat %>% 
    mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
    select(id, Height, Time_point) %>%
    drop_na()%>%
    pivot_wider(names_from = Time_point, values_from = Height) %>% 
    arrange(id) %>% rename(T1_height = T1) %>%
    add_column(T2_height = NA)
  
  width_1 <- 
    dat %>% 
    mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
    select(id, Width_1, Time_point) %>%
    drop_na()%>%
    pivot_wider(names_from = Time_point, values_from = Width_1) %>% 
    arrange(id) %>% rename(T1_width_1 = T1) %>%
    add_column(T2_width_1 = NA)
  
  width_2 <- 
    dat %>% 
    mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
    select(id, Width_2, Time_point) %>%
    drop_na()%>%
    pivot_wider(names_from = Time_point, values_from = Width_2) %>% 
    arrange(id) %>% rename(T1_width_2 = T1) %>%
    add_column(T2_width_2 = NA)
  
  leaves <- 
    dat %>% 
    mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
    select(id, Number_leaves, Time_point) %>%
    drop_na() %>%
    pivot_wider(names_from = Time_point, values_from = Number_leaves) %>% 
    arrange(id) %>% rename(T1_leaves = T1) %>%
    add_column(T2_leaves = NA)
  
  flowers <- 
    dat %>% 
    mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
    select(id, Flower_head, Time_point) %>%
    drop_na() %>%
    pivot_wider(names_from = Time_point, values_from = Flower_head) %>% 
    arrange(id) %>% rename(T1_flower = T1) %>%
    add_column(T2_flower = NA)
  
  seeds <- 
    dat %>% 
    mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
    select(id, Seed_head, Time_point) %>%
    drop_na() %>%
    pivot_wider(names_from = Time_point, values_from = Seed_head) %>% 
    arrange(id) %>% rename(T1_seed = T1) %>%
    add_column(T2_seed = NA) 
  
  all <-
    height %>% left_join(width_1) %>% 
    left_join(width_2) %>% 
    left_join(leaves) %>% 
    left_join(flowers) %>%
    left_join(seeds)
  
}



height <- AO %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Height, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Height) %>% 
  arrange(id) %>% rename(T1_height = T1) %>%
  add_column(T2_height = NA)

width_1 <- AO %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Width_1, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_1) %>% 
  arrange(id) %>% rename(T1_width_1 = T1) %>%
  add_column(T2_width_1 = NA)
flowers <- AO %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Flower_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Flower_head) %>% 
  arrange(id) %>% rename(T1_flower = T1) %>%
  add_column(T2_flower = NA)
seeds <- AO %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos)) %>%
  select(id, Seed_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Seed_head) %>% 
  arrange(id) %>% rename(T1_seed = T1) %>%
  add_column(T2_seed = NA) 
AO <-
  height %>% left_join(width_1) %>% 
  left_join(width_2) %>% 
  left_join(leaves) %>% 
  left_join(flowers) %>%
  left_join(seeds)

write_csv(AO, "data/demo_ts/AO_demo_TS.csv")
RA <- make_ts_demo_Hazel(RA)
write_csv(RA, "data/demo_ts/RA_demo_TS.csv")
RR <- make_ts_demo_Hazel(RR)
write_csv(RR, "data/demo_ts/RR_demo_TS.csv")
CA <- make_ts_demo_Hazel(CA)
write_csv(CA, "data/demo_ts/CA_demo_TS.csv")

##############################################################
# Ainsdale

# HV
# SR
# PD
# EP

EP <- read_csv("data/demo_test/Ainsdale_EP_T1.csv")
HV <- read_csv("data/demo_test/Ainsdale_HV_T1.csv")
MA <- read_csv("data/demo_test/Ainsdale_MA_T1.csv")
PD <- read_csv("data/demo_test/Ainsdale_PD_T1.csv")
SR <- read_csv("data/demo_test/Ainsdale_SR_T1.csv")

make_ts_demo_Ains <- function(dat) {
  
  height <- dat %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Height, Time_point) %>%
  drop_na()%>%
  arrange(id) %>% rename(T1_height = Height) %>%
  add_column(T2_height = NA) %>%
  select(id, Time_point, T1_height, T2_height)
  
  width_1 <- dat %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Width_1, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_1) %>% 
  arrange(id) %>% rename(T1_width_1 = T1) %>%
  add_column(T2_width_1 = NA)
  
  leaves <- dat %>% 
    mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
    select(id, Number_leaves, Time_point) %>%
    drop_na()%>%
    pivot_wider(names_from = Time_point, values_from = Number_leaves) %>% 
    arrange(id) %>% rename(T1_Number_leaves = T1) %>%
    add_column(T2_Number_leaves = NA)
  
  flowers <- dat %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Flower_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Flower_head) %>% 
  arrange(id) %>% rename(T1_flower = T1) %>%
  add_column(T2_flower = NA)
  
  seeds <- dat %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Seed_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Seed_head) %>% 
  arrange(id) %>% rename(T1_seed = T1) %>%
  add_column(T2_seed = NA) 
  
  all <- 
  height %>% 
  left_join(width_1) %>% 
  left_join(leaves) %>% 
  left_join(flowers) %>%
  left_join(seeds)

}

EP <- make_ts_demo_Ains(EP)
write_csv(EP, "data/demo_ts/EP_demo_TS.csv")
HV <- make_ts_demo_Ains(HV)
write_csv(HV, "data/demo_ts/HV_demo_TS.csv")
MA <- make_ts_demo_Ains(MA)
write_csv(MA, "data/demo_ts/MA_demo_TS.csv")

######################################################################

# PD - needs width 1 and 2 as a rosette
# PD <- make_ts_demo_Ains(PD)
height <- PD %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Height, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Height) %>% 
  arrange(id) %>% rename(T1_height = T1) %>%
  add_column(T2_height = NA)
width_1 <- PD %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Width_1, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_1) %>% 
  arrange(id) %>% rename(T1_width_1 = T1) %>%
  add_column(T2_width_1 = NA)
width_2 <- PD %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Width_2, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_2) %>% 
  arrange(id) %>% rename(T1_width_2 = T1) %>%
  add_column(T2_width_2 = NA)
leaves <- PD %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Number_leaves, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Number_leaves) %>% 
  arrange(id) %>% rename(T1_leaves = T1) %>%
  add_column(T2_leaves = NA) 
flowers <- PD %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Flower_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Flower_head) %>% 
  arrange(id) %>% rename(T1_flower = T1) %>%
  add_column(T2_flower = NA)
seeds <- PD %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Seed_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Seed_head) %>% 
  arrange(id) %>% rename(T1_seed = T1) %>%
  add_column(T2_seed = NA) 
PD <-
  height %>% left_join(width_1) %>% 
  left_join(width_2) %>% 
  left_join(leaves) %>% 
  left_join(flowers) %>%
  left_join(seeds)
write_csv(PD, "data/demo_ts/PD_demo_TS.csv")

#################################################################

# PD - needs width 1 and 2 as a rosette
# PD <- make_ts_demo_Ains(PD)
height <- MA %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Height, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Height) %>% 
  arrange(id) %>% rename(T1_height = T1) %>%
  add_column(T2_height = NA)
width_1 <- MA %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Width_1, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_1) %>% 
  arrange(id) %>% rename(T1_width_1 = T1) %>%
  add_column(T2_width_1 = NA)
width_2 <- MA %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Width_2, Time_point) %>%
  drop_na()%>%
  pivot_wider(names_from = Time_point, values_from = Width_2) %>% 
  arrange(id) %>% rename(T1_width_2 = T1) %>%
  add_column(T2_width_2 = NA)
leaves <- MA %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Number_leaves, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Number_leaves) %>% 
  arrange(id) %>% rename(T1_leaves = T1) %>%
  add_column(T2_leaves = NA) 
flowers <- MA %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Flower_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Flower_head) %>% 
  arrange(id) %>% rename(T1_flower = T1) %>%
  add_column(T2_flower = NA)
seeds <- MA %>% 
  mutate(id = paste0(Block, "_", Treatment, "_", Individual_nos, "_", Plot_number)) %>%
  select(id, Seed_head, Time_point) %>%
  drop_na() %>%
  pivot_wider(names_from = Time_point, values_from = Seed_head) %>% 
  arrange(id) %>% rename(T1_seed = T1) %>%
  add_column(T2_seed = NA) 
MA <-
  height %>% left_join(width_1) %>% 
  left_join(width_2) %>% 
  left_join(leaves) %>% 
  left_join(flowers) %>%
  left_join(seeds)
write_csv(MA, "data/demo_ts/MA_demo_TS.csv")

SR <- make_ts_demo_Ains(SR)
write_csv(SR, "data/demo_ts/SR_demo_TS.csv")