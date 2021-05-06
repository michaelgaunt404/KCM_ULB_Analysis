#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a scratch script
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: used for general eda or exploration
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#project file performs this task - section is not required

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#zz_localG performs this task - section is not required

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#zz_localG performs this task - section is not required


#Section: data load=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_files = getwd() %>%
  paste0(., "/data") %>%
  list.files() %>%
  .[str_detect(., ".csv")] #watch out, only grabbing csvs

data_list = read_csv_allFiles(all_files, "/data/")


#Section: cleaning UNIT_DEPT_COMP_MAIN=========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
o_wo = data_list$O_WO.csv %>%
  select(company, unit_id, fisc_pd, meter, ends_with(c("cost_do", "_dt"))) %>%
  arrange(unit_id, meter) %>%
  mutate(event_cost = act_comm_cost_do + act_fluid_cost_do + act_labor_cost_do + act_part_cost_do,
         across(ends_with("dt"), lubridate::ymd_hms ),
         fisc_pd_corr = ym(fisc_pd),
         meter_sans0 = ifelse(meter==0, 1, meter),
         cost_per_meter = event_cost/meter_sans0,
         meter_bin = floor_divide(meter_sans0, 1e3)
  ) %>%
  arrange(unit_id, fisc_pd_corr, meter_sans0) %>%
  group_by(unit_id) %>%
  mutate(event_cost_cumm = cumsum(event_cost)) %>%
  data.table()


#Section: exploring FISCAL_CAL data=============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_list$FISCAL_CAL.csv

identical(data_list$FISCAL_CAL.csv, data_list$FISCAL_CAL_all_records.csv)

#increase in ~300 records
skimr::skim(data_list$FISCAL_CAL.csv)
skimr::skim(data_list$FISCAL_CAL_all_records.csv)

#fiscal period suffix seems to indicate month
data_list$FISCAL_CAL_all_records.csv[, c("FISC_PD", "START_DATE")] %>%  unique()

data_list$FISCAL_CAL_all_records.csv %>%group_by(BILL_CLOSE_DT) %>% count()

data_list$FISCAL_CAL_all_records.csv %>%
  janitor::remove_constant()


#Section: exploring MCC=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#veh lookup table
mcc = data_list$MCC.csv %>%
  select(company, mcc, description, max_usage, change_dt)

#Section: exploring TECH_SPEC===================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#data for individual assets
tec_spec = data_list$TECH_SPEC.csv  %>%
  select(company, spec_no, make, manufacturer, model, year, change_dt)
remove_constant()

#Section: exploring UNIT_DEPT_COMP_MAIN=========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#data for individual assets
data_list$UNIT_DEPT_COMP_MAIN.csv %>%
  names() %>%
  sort()
remove_constant()

#col filter for relevance
udcm = data_list$UNIT_DEPT_COMP_MAIN.csv %>%
  select(company, mcc, unit_id, year, asset_no, make, manufacturer, model, year, contains(c("meter", "acquis")),
         -contains(c("var", "source", "req"))) %>%
  .[order(unit_id)] %>%
  mutate(across(contains(c('_date', '_dt')), lubridate::ymd_hms),
         nu_year = ymd(paste0(year, "-01-01")))

udcm_make = data_list$UNIT_DEPT_COMP_MAIN.csv %>%
  select(company, mcc, unit_id, asset_no, make, manufacturer, model, year, contains(c("meter", "acquis")),
         -contains(c("var", "source", "req")))

udcm %>%
  skimr::skim()

udcm[, .(year, acquis_dt)] %>%
  unique() %>%
  mutate(nu_year = year(acquis_dt),
         check = year==nu_year) %>%
  count(check)
data.frame()

nrow(udcm) == length(unique(udcm$unit_id))

#Section: exploring UNIT_DEPT_COMP_MAIN=========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_list$UNIT_HIST.csv %>%  #skimr::skim()
  select(company, unit_id, fisc_pd, labor_do, ends_with("dt"),  tb_updated_date) %>%
  arrange(unit_id)

#Section: exploring UNIT_DEPT_COMP_MAIN=========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
o_wo = data_list$O_WO.csv %>%
  # names() %>%  sort()C
  select(company, unit_id, fisc_pd, meter, ends_with(c("cost_do", "_dt"))) %>%
  arrange(unit_id, meter) %>%
  mutate(event_cost = act_comm_cost_do + act_fluid_cost_do + act_labor_cost_do + act_part_cost_do,
         across(ends_with("dt"), lubridate::ymd_hms ),
         fisc_pd_corr = ym(fisc_pd),
         meter_sans0 = ifelse(meter==0, 1, meter),
         cost_per_meter = event_cost/meter_sans0,
         meter_bin = floor_divide(meter_sans0, 1e3)) %>%
  arrange(unit_id, fisc_pd_corr, meter_sans0) %>%
  group_by(unit_id) %>%
  mutate(event_cost_cumm = cumsum(event_cost)) %>%
  data.table()

joined = merge.data.table(
  o_wo,
  udcm,
  by = c("company", "unit_id")
) %>%
  filter(cost != 0) %>%
  filter(!is.na(nu_year)) %>%
  arrange(unit_id,  meter_sans0) %>%
  select(company, unit_id, cost, close_dt, fisc_pd_corr:mcc, nu_year) %>%
  mutate(age = fisc_pd_corr-nu_year) %>%
  data.table() %>%
  .[,`:=`(cumm_cost = cumsum(cost),
          meter_diff = meter_sans0-lag(meter_sans0)), by = .(unit_id)] %>%
  .[,`:=`(cumm_cost_per_meter = cumm_cost/meter_sans0)]

list = joined[,unit_id] %>%
  unique() %>%
  sample(100)

#density of duration between maintenance events
joined[unit_id %in% list, ] %>%
  # count(unit_id, meter_bin) %>%
  ggplot() +
  geom_histogram(aes(meter_diff, group = unit_id)) +
  # geom_line(aes(meter_bin, n, group = unit_id)) +
  scale_x_log10()

#density of duration between maintenance events
joined[unit_id %in% list, ] %>%
  ggplot() +
  geom_point(aes(meter_diff, cost), alpha = .2) +
  scale_x_log10() #+ scale_y_log10()

joined[unit_id %in% list, ] %>%
  ggplot() +
  geom_line(aes(meter_sans0, cumm_cost, group = unit_id), alpha = .2) +
  scale_x_log10() #+ scale_y_log10()

#doesn't look like mainteance event costs increase with mileage
joined[unit_id %in% list, ] %>%
  ggplot() +
  geom_line(aes(meter_sans0, cost, group = unit_id), alpha = .2) +
  scale_x_log10()

joined %>%
  # .[unit_id %in% list, ] %>%
  # count(unit_id, meter_bin) %>%
  ggplot() +
  geom_point(aes(meter_sans0, cumm_cost_per_meter), alpha = .2) +
  ylim(NA, .7)

joined %>%
  filter(unit_id == 577) %>%
  .[unit_id %in% list, ] %>%
  pivot_longer(cols = c(cost, cumm_cost, cumm_cost_per_meter)) %>%
  ggplot(aes(meter_sans0, value)) +
  geom_line(aes(group = name), alpha = .1) +
  stat_summary(fun = "mean", geom = "point") +
  facet_grid(rows = vars(name),
             scales = "free")

#
tmp = joined %>%
  filter(unit_id == 577
         | unit_id == 126
         | unit_id == 400) %>%
  arrange(meter_bin, -meter_sans0, -cumm_cost) %>%
  data.table()

#checking age vs event cost
#not very much of a relationship here
ggplot(tmp) +
  geom_point(aes(age, cost))

#checking meter_diff vs event cost
#seems no relationship either
ggplot(tmp) +
  geom_point(aes(meter_diff, cost, color = as.factor(unit_id)))

#investigating selection methods
#tec1
tmp %>%
  .[,.(sum_cost = sum(cost),
       max_meter = max(meter_sans0)), by = .(meter_bin, unit_id)] %>%
  .[,`:=`(cum_sum = cumsum(sum_cost),
          meter_reset = crrct0(max_meter)), by = .(unit_id)] %>%
  .[order(unit_id, meter_bin)] %>%
  print()
group_by(meter_bin, unit_id) %>%
  summarise(
    sum_cost = sum(cost),
    max_meter = max(meter_sans0)
  ) %>%
  mutate(cumm = cumsum(sum_cost))




#Vehical costs ===============================================================

veh_costs = read_excel(
  here("data/FA033_20210426.xlsx"),
  sheet = "FA_RPRT_033",
  skip = 14) %>%
  clean_names() %>%
  data.table()

veh_costs %>%
  .[asset_category == "TRANSIT EQ.VANPOOL VAN",] %>%
  .[,`:=`(desc_cleaned = str_remove_all(asset_description, "[[:digit:]]") %>%
            str_to_lower())] %>%
  # filter(desc_cleaned == "DODGE GRAND CARAVAN HOV")
# filter(str_detect(desc_cleaned, "ada"))
#   .[desc_cleaned == "DODGE GRAND CARAVAN HOV"] %>%
#   print()
  .[,.(ave_cost = max(cost, na.rm = T)), by = desc_cleaned] %>%


  King_County_Metro.Asset_Inventory.2020

veh_costs = read_excel(
  here("data/King_County_Metro.Asset_Inventory.2020.xlsx"),
  sheet = "Rolling stock form_mg",
  skip = 10) %>%
  clean_names() %>%
  data.table() %>%
  .[,`:=`(make_model = make_model %>%  str_to_lower())]

veh_costs_agg = veh_costs %>%
  .[,.(ave_cost = mean(replacement_cost, na.rm = T),
       med_cost = median(replacement_cost, na.rm = T)), by = make_model] %>%
  separate(col = "make_model", into = c("make", "model"), sep = "/") %>%
  filter(!is.na(model))

working_list = joined %>%
  .[, .(manufacturer, make, model)] %>%
  .[order(manufacturer, model)] %>%
  unique() %>%
  mutate(across(c('manufacturer', 'make', 'model'), str_to_lower))

working_list %>%
  .[,lapply(.SD, str_to_lower), .SDcols = c('manufacturer', 'make', 'model')]

working_list %>%
  merge.data.table(veh_costs_agg, by = "model")


joined %>%
  .[model == "SENATOR II", `:=`(cost = 82081.75)] %>%
  .[,`:=`(rep_cost = cost/meter_max_mnth)] %>%
  .[,`:=`(cosy_cost = rep_cost+event_cost_per_meter_per_mnth)] %>%
  .[model == "SENATOR II",] %>%
  .[,.(yolo = mean(rep_cost)), by = .(type = as.factor(floor_divide(meter_max_mnth, 5000)))] %>%
  ggplot() +
  # geom_line(aes(meter_max_mnth, rep_cost)) +
  geom_point(aes(guy, yolo)) +
  scale_y_log10()

joined %>%
  mutate(across(c('manufacturer', 'make', 'model'), str_to_lower)) %>%
  merge.data.table(veh_costs_agg, by = "model") %>%
  .[,`:=`(rep_cost = ave_cost/meter_max_mnth)] %>%
  .[,`:=`(cosy_cost = rep_cost+event_cost_per_meter_per_mnth)] %>%
  # .[model == "senator ii",] %>%
  .[,.(yolo = mean(rep_cost),
       bolo = mean(event_cost_per_meter_per_mnth),
       molo = mean(cosy_cost)), by = .(type = as.factor(floor_divide(meter_max_mnth, 5000)),
                                      model)] %>%
  ggplot() +
  # geom_line(aes(meter_max_mnth, rep_cost)) +
  geom_line(aes(type, molo, group = model)) +
  # geom_line(aes(type, bolo, group = model)) +
  geom_hline(yintercept = .5) +
  scale_y_log10()


joined %>%
  mutate(across(c('manufacturer', 'make', 'model'), str_to_lower)) %>%
  merge.data.table(veh_costs_agg, by = "model") %>%
  .[,`:=`(rep_cost = ave_cost/meter_max_mnth)] %>%
  .[,`:=`(comb_cost = rep_cost + event_cost_per_meter_per_mnth)] %>%
  pivot_longer(cols = c(rep_cost, event_cost_per_meter_per_mnth)) %>%
  ggplot() +
  geom_boxplot(aes(as.factor(floor_divide(meter_max_mnth, 20000)), value, color = name)) +
  scale_y_log10() +
  ylim(0,2) +
  facet_wrap(vars(model), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5)) +
  labs(y = "Combined Cost (replacement + maintenance)", x = "Veh. Meter Bins")


  joined %>%
    mutate(across(c('manufacturer', 'make', 'model'), str_to_lower)) %>%
    merge.data.table(veh_costs_agg, by = "model") %>%
    .[,`:=`(rep_cost = ave_cost/meter_max_mnth)] %>%
    .[,`:=`(comb_cost = rep_cost + event_cost_per_meter_per_mnth)] %>%
  # pivot_longer(cols = c(rep_cost, event_cost_per_meter_per_mnth)) %>%
    ggplot() +
    # geom_boxplot(aes(meter_bin, event_cost_per_meter_per_mnth)) +
    # geom_boxplot(aes(meter_bin, rep_cost)) +
    geom_boxplot(aes(meter_bin, comb_cost)) +
    facet_grid(rows = vars(model)) +
    scale_y_log10()
    ylim(0,1)

    install.packages("gghighlight")
    library(gghighlight)

    joined %>%
      mutate(across(c('manufacturer', 'make', 'model'), str_to_lower)) %>%
      merge.data.table(veh_costs_agg, by = "model") %>%
      .[,`:=`(rep_cost = ave_cost/meter_max_mnth)] %>%
      .[,`:=`(comb_cost = rep_cost + event_cost_per_meter_per_mnth)] %>%
      .[,.(rep_cost_mean = mean(rep_cost),
           MMMR_mean = median(event_cost_per_meter_per_mnth)), by = .(meter_bin, model)] %>%
      .[,`:=`(both = rep_cost_mean+MMMR_mean)] %>%
      melt.data.table(measure.vars = c('rep_cost_mean', "MMMR_mean", "both")) %>%
      ggplot() +
      # geom_boxplot(aes(meter_bin, event_cost_per_meter_per_mnth)) +
      # geom_boxplot(aes(meter_bin, rep_cost)) +
      geom_line(aes(as.numeric(meter_bin), value, color = variable)) +
      facet_grid(rows = vars(model)) +
      scale_y_log10()
