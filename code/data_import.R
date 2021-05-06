#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is loads the data and provides EDA
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert breif readme here]]
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


#Script Start===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Section: data load=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_files = here() %>%
  paste0(., "/data") %>%
  list.files() %>%
  .[str_detect(., ".csv")] #watch out, only grabbing csvs

data_list = read_csv_allFiles(all_files, "/data/")


#Section: cleaning UNIT_DEPT_COMP_MAIN=========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
o_wo = data_list$O_WO.csv %>%
  select(company, unit_id, fisc_pd, meter, ends_with(c("cost_do", "_dt"))) %>%
  mutate(event_cost = act_comm_cost_do + act_fluid_cost_do + act_labor_cost_do + act_part_cost_do,
         across(ends_with("dt"), lubridate::ymd_hms ),
         fisc_pd_corr = ym(fisc_pd),
         meter_sans0 = ifelse(meter==0, 1, meter)) %>%
  arrange(unit_id, fisc_pd_corr, meter_sans0) %>%
  group_by(unit_id) %>%
  mutate(meter_diff_mnth = (meter_sans0-lag(meter_sans0)) %>%
           replace_na(0),
         meter_corr = crrct0(meter_sans0),
         fisc_pd_corr2 = crrct0(fisc_pd_corr) %>%  as.numeric(),
         event_cost_cumm = cumsum(event_cost) #this is not applicable until after
         ) %>%
  data.table()

#Section: exploring UNIT_DEPT_COMP_MAIN=========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
udcm = data_list[["UNIT_DEPT_COMP_MAIN_edits_20210503.csv"]] %>%
  select(company, mcc, unit_id, year, asset_no, make, manufacturer, model, year, contains(c("meter", "acquis")),
         -contains(c("var", "source", "req"))) %>%
  .[order(unit_id)] %>%
  mutate(across(contains(c('_date', '_dt')), lubridate::ymd_hms),
         nu_year = ymd(paste0(year, "-01-01")))


#Section: writing out RDS objects===============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
saveRDS(o_wo, file = here(paste("data/o_wo_", Sys.Date(), ".rds")))
saveRDS(udcm, file = here(paste("data/udcm_", Sys.Date(), ".rds")))



#
#
# library(validate)
# install.packages("validate")
# names(o_wo)
# rules = validator(negative_meter_diff_check = meter_diff_mnth>0
#                   )
#
# yolo = confront(o_wo, rules)
#
# summary(yolo)
#
# o_wo %>%
#   filter(meter_diff_mnth>0 &
#            abs(meter_diff_mnth)<15000) %>%
#   # filter(unit_id %in% unit_list) %>%
#   # filter(unit_id == 577) %>%
#   pivot_longer(cols = c(meter_corr, meter_diff_mnth)) %>%
#   ggplot(aes(fisc_pd_corr2, value)) +
#   geom_line(aes(group = unit_id), alpha = .2) +
#   facet_grid(rows = vars(name), scales = 'free_y',
#              labeller = as_labeller(
#                c(
#                  `meter_diff_mnth` = "Monthly Meter Diff",
#                  `meter_sans0` = "Vehicle Meter"
#                ))) +
#   # scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
#   labs(y = "Value", x = "Year", title = "Example of Poor Meter Data Quality")
#
#
# unit_list = sample(unique(o_wo$unit_id), 20)
#
# #remove very large positive meter differences
# #-----negative stay becasue they are a result of the increase and could not need to be removed
# #remake meter_diff_mnth
# #remove negative now - these should be true negatives
# #-----still an incorrect input but difference was too small to filter inw with first filer
# o_wo_agg = o_wo %>%
#   # filter(meter_diff_mnth<(mean(meter_diff_mnth)+sd(meter_diff_mnth))) %>%
#   # .[,`:=`(meter_diff_mnth = (meter_sans0-lag(meter_sans0)) %>%
#   #           replace_na(0)), by = .(unit_id)] %>%
#   # filter(meter_diff_mnth>=0) %>%
#   # filter(meter_diff_mnth>0 &
#   #          abs(meter_diff_mnth)<15000) %>%
#   # filter(unit_id == 576 |
#   #          unit_id == 577) %>%
#   #4L perfroms month aggregations
#   .[,.(event_cost_mnth_ttl = sum(event_cost),
#        event_cost_max = max(event_cost_cumm),
#        meter_max_mnth = max(meter_sans0),
#        wo_count = .N),
#     by = .(fisc_pd_corr, unit_id)] %>%
#   #3L makes cummlatives
#   .[,`:=`(event_cost_mnth_ttl_cumm = cumsum(event_cost_mnth_ttl),
#           event_cost_check = round(event_cost_max - cumsum(event_cost_mnth_ttl),2)),
#     by = .(unit_id)] %>%
#   #3L remove first group given greater than lead vioulation
#   group_by(unit_id) %>%
#   filter(!(meter_max_mnth > lead(meter_max_mnth) & row_number() == 1)) %>%
#   data.table() %>%
#   #3L recreates
#   .[,`:=`(meter_0_corr = crrct0(meter_max_mnth)), by = .(unit_id)] %>%
#   .[,`:=`(meter_diff_mnth = meter_0_corr-lag(meter_0_corr, default =  0)),
#     by = .(unit_id)] %>%
#   #1L removes rows where violation occurs per unit group
#   .[, .SD[abs(meter_diff_mnth)<10*median(meter_diff_mnth)], by = .(unit_id)] %>%
#   .[,`:=`(meter_bin = as.factor(floor_divide(meter_max_mnth, 25000)),
#           meter_bin_o_corr = as.factor(floor_divide(meter_0_corr, 25000)),
#           event_cost_per_meter_per_mnth = event_cost_mnth_ttl/meter_diff_mnth)] %>%
#   .[,
#     .SD[between(event_cost_per_meter_per_mnth,
#             0,
#             quantile(event_cost_per_meter_per_mnth, probs = .75, na.rm = T) + 1.5*IQR(event_cost_per_meter_per_mnth, na.rm = T)
#             )], by = .(meter_bin)] %>%
#   .[event_cost_per_meter_per_mnth<4,]
#
#
# joined = merge.data.table(
#   o_wo_agg,
#   udcm,
#   by = c("unit_id")
# ) %>%
#   mutate(age = as.numeric(fisc_pd_corr-nu_year),
#          age_bin = as.factor(age%/%365))
#
#
#
# joined %>%
#   ggplot(aes(age_bin, event_cost_per_meter_per_mnth)) +
#   geom_boxplot() +
#   stat_summary(fun = mean, geom = "point", shape = 20, size = 10, color = "red") +
#   # facet_grid(cols = vars(mcc), scales = "free") +
#   ylim(0, 1)
#
# joined %>%
#   ggplot(aes(meter_bin, event_cost_per_meter_per_mnth)) +
#   geom_boxplot() +
#   stat_summary(fun = mean, geom = "point", shape = 20, size = 10, color = "red") +
#   facet_grid(cols = vars(mcc), scales = "free") +
#   ylim(0, 1)
#
# joined %>%
#   ggplot(aes(meter_bin, event_cost_per_meter_per_mnth)) +
#   geom_boxplot() +
#   stat_summary(fun = mean, geom = "point", shape = 20, size = 10, color = "red") +
#   facet_grid(cols = vars(model), scales = "free") +
#   ylim(0, 1)
#
# o_wo_agg %>%
#   ggplot() +
#   geom_boxplot(aes(meter_bin, meter_diff_mnth)) +
#   ylim(0, NA)
#
# #
# o_wo_agg %>%
#   ggplot() +
#   geom_boxplot(aes(meter_bin, wo_count)) +
#   ylim(0, NA)
#
# o_wo_agg %>%
#   # filter(meter_diff_mnth > 500 ) %>%
#   # filter(event_cost_per_meter_per_mnth < 5 )  %>%
#   ggplot(aes(meter_bin, event_cost_per_meter_per_mnth)) +
#   geom_boxplot() +
#   stat_summary(fun.y = mean, geom = "point", shape = 20, size = 14, color = "red") +
#   ylim(0, 1)
#
# o_wo_agg %>%
#   filter(meter_diff_mnth > 500 ) %>%
#   filter(event_cost_per_meter_per_mnth < 5 )  %>%
#   ggplot() +
#   geom_line(aes(meter_max_mnth, event_cost_max, group = unit_id)) +
#   ylim(0, NA)
#
# o_wo_agg %>%
#   filter(meter_diff_mnth > 500 ) %>%
#   filter(event_cost_per_meter_per_mnth < 5 )  %>%
#   ggplot() +
#   geom_boxplot(aes(meter_bin_o_corr, event_cost_max)) +
#   ylim(0, NA)
# IQR(o_wo_agg$event_cost_per_meter_per_mnth, na.rm = T)
# fivenum(o_wo_agg$event_cost_per_meter_per_mnth)
#
#
#
# o_wo_agg %>%
#   filter(meter_diff_mnth > 500 ) %>%
#   filter(event_cost_per_meter_per_mnth < 5 ) %>%
#   ggplot() +
#   geom_density(aes(event_cost_per_meter_per_mnth)) +
#   scale_y_log10()
#
#
#
#
#
#
#
#   .[,`:=`(meter_0_corr = crrct0(meter_max_mnth)), by = .(unit_id)] %>%
#   .[,`:=`(meter_diff_mnth = meter_0_corr-lag(meter_0_corr, default =  0)),
#     by = .(unit_id)] %>%
#   .[,`:=`(event_cost_per_meter_per_mnth = event_cost_mnth_ttl_cumm/meter_diff_mnth),
#     by = .(unit_id)] %>%
#   select(fisc_pd_corr, unit_id, starts_with("meter"), starts_with("event"))
#
#   o_wo_agg %>%
#     ggplot(aes(fisc_pd_corr2, value)) +
#     geom_line(aes(group = unit_id), alpha = .2)
#
#   o_wo_agg %>%
#     ggplot(aes(fisc_pd_corr2, value)) +
#     geom_line(aes(group = unit_id), alpha = .2)
#
#   o_wo_agg %>%
#     filter(unit_id == 117)  %>%
#     ggplot(aes(meter_max_mnth, event_cost_per_meter_per_mnth)) +
#     geom_point(aes(group = unit_id), alpha =1) +
#     ylim(NA, 5)
#     pivot_longer(cols = !c(fisc_pd_corr, unit_id, meter_bin)) %>%
#     ggplot(aes(fisc_pd_corr, value)) +
#     geom_point(aes(group = unit_id), alpha =1) +
#     facet_grid(rows = vars(name), scales = 'free_y')
#                # ,
#                # labeller = as_labeller(
#                #   c(
#                #     `meter_diff_mnth` = "Monthly Meter Diff",
#                #     `meter_sans0` = "Vehicle Meter"
#                #   )
#                  # )
#   ) +
#     # scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
#     labs(y = "Value", x = "Year", title = "Example of Poor Meter Data Quality" )
#

