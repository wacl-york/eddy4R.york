library(dplyr)
library(purrr)
library(ggplot2)

source("acf_testing.R")
devtools::load_all()

resultsACFList = list()

idxs = 1:100

cli::cli_progress_bar(total = max(idxs))

for(i in idxs){

  cli::cli_progress_update()

  argsBase = acf_test_defaults(i) # Default - Lag Max == 2*freq
  argsBaseYork = argsBase
  argsBaseYork$lagMax = 40*5 # eddy4r.york default is 40 * freq

  argsBaseYorkNoHpf = argsBaseYork
  argsBaseYorkNoHpf$hpf = FALSE

  argsLongLagMax = argsBase
  argsLongLagMax$lagMax = 180*5

  argsLongLagMaxNoHpf = argsLongLagMax
  argsLongLagMaxNoHpf$hpf = FALSE

  argsHpfModified = argsBaseYork
  argsHpfModified$freqThsh = 1 / (2 * 10 / 5)

  # argsHpfModifiedLongLag = argsLongLagMax
  argsHpfModifiedLongLag = argsHpfModified
  argsHpfModifiedLongLag$lagMax = 180*5
  argsHpfModifiedLongLag$freqThsh = 1 / (2 * 10 / 5)

  temp = list(
    base = do.call(def.lag2, argsBase),
    baseYork = do.call(def.lag2, argsBaseYork),
    baseYorkNoHpf = do.call(def.lag2, argsBaseYorkNoHpf),
    longLagMax = do.call(def.lag2, argsLongLagMax),
    longLagMaxNoHpf = do.call(def.lag2, argsLongLagMaxNoHpf),
    hpfModified = do.call(def.lag2, argsHpfModified),
    hpfModifiedLongLag = do.call(def.lag2, argsHpfModifiedLongLag)

  ) |>
    map(extract_acf_df) |>
    list_rbind(names_to = "type") |>
    filter(lag <= 0)

  resultsACFList[[i]] = temp
}

names(resultsACFList) = idxs


resultsACF = list_rbind(resultsACFList, names_to = "file") |>
  group_by(file, type) |>
  mutate(
    acfNorm =  (acf - min(acf, na.rm = T))/(max(acf, na.rm = T)-min(acf, na.rm = T)),
    acfalign = acf-median(acf, na.rm = T)
  )

resultsACF |>
  filter(file == 1,
         type %in% c("baseYork", "longLagMax", "baseYorkNoHpf", "longLagMaxNoHpf")) |>
  ggplot()+
  geom_point(aes(lag/5, acfalign, colour = type))+
  theme_minimal()


resultsACF |>
  ggplot()+
  geom_line(
    aes(lag/5, acfalign, group = file)
  )+
  facet_wrap(~type, scales = "free")

resultsACF |>
  filter(type != "base") |>
  ggplot()+
  geom_line(
    aes(lag/5, acfalign, group = file)
  )+
  facet_wrap(~type, scales = "free")+
  theme_minimal()


resultsACF |>
  filter(type %in% c("baseYork", "longLagMax"),
         lag >= -200) |>
  ggplot()+
  geom_line(
    aes(lag/5, acfalign, group = file)
  )+
  facet_wrap(~type, scales = "free", ncol = 1)+
  theme_minimal()

resultsACF |>
  filter(type %in% c("baseYork", "baseYorkNoHpf"),
         lag >= -200) |>
  ggplot()+
  geom_line(
    aes(lag/5, acfalign, group = file)
  )+
  facet_wrap(~type, scales = "free", ncol = 1)+
  theme_minimal()

resultsACF |>
  filter(type %in% c("baseYork", "longLagMax")) |>
  ggplot()+
  geom_line(
    aes(lag/5, acfalign, group = file)
  )+
  facet_wrap(~type, ncol = 1)+
  theme_minimal()

resultsACF |>
  filter(type %in% c("hpfModified", "hpfModifiedLongLag"),
         lag >= -200) |>
  ggplot()+
  geom_line(
    aes(lag/5, acfalign, group = file)
  )+
  facet_wrap(~type, ncol = 1)+
  theme_minimal()

resultsACF |>
  filter(type %in% c("baseYorkNoHpf", "hpfModifiedLongLag"),
         lag >= -200) |>
  ggplot()+
  geom_line(
    aes(lag/5, acfalign, group = file)
  )+
  facet_wrap(~type, ncol = 1)+
  theme_minimal()

plotly::ggplotly()

resultsACF |>
  ggplot()+
  geom_line(
    aes(lag/5, acf, colour = type)
  )


plotly::ggplotly()




1 / (2 * 40 / 5)
1 / (2 * 180 / 5)



# Test HPF ----------------------------------------------------------------

argsBaseHpf = acf_test_defaults(93)
argsBaseHpf$lagMax = 40*5
argsBaseHpf$freqSplt = 5/2

argsHpfLongLagMax = argsBaseHpf
argsHpfLongLagMax$lagMax = 180*5

thrsh = 1 / (2 * c(seq(5,180,5)) / 5)
1/(2*(180/5))
hpfList = list()

for(j in 1:length(thrsh)){

  argsBaseHpf$freqThsh = thrsh[j]
  argsHpfLongLagMax$freqThsh = thrsh[j]

  temp2 = list(
    baseHpf = do.call(def.lag2, argsBaseHpf),
    hpfLongLagMax = do.call(def.lag2, argsHpfLongLagMax)
  ) |>
    map(extract_acf_df) |>
    list_rbind(names_to = "type") |>
    filter(lag <= 0)

  hpfList[[j]] = temp2

}

names(hpfList) = thrsh

resultsHpf = list_rbind(hpfList, names_to = "thrsh") |>
  group_by(thrsh, type) |>
  mutate(
    acfNorm =  (acf - min(acf, na.rm = T))/(max(acf, na.rm = T)-min(acf, na.rm = T)),
    acfalign = acf-median(acf, na.rm = T)
  )


resultsHpf |>
  ggplot()+
  geom_line(aes(lag, acfalign, colour = as.numeric(thrsh), group = thrsh))+
  scale_colour_viridis_c()+
  facet_wrap(~type, ncol = 1)+
  theme_minimal()

resultsHpf |>
  ggplot()+
  geom_line(aes(lag, acfalign, colour = as.numeric(thrsh), group = thrsh))+
  scale_colour_viridis_c()+
  scale_x_continuous(limits = c(-200,0))+
  facet_wrap(~type, ncol = 1)+
  theme_minimal()


plotly::ggplotly()



















