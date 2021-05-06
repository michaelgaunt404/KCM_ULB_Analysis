#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is applies additional data aggregation and filters
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: data_import imports data and perfroms basic munging
#-------- the data resulting form the file is not sufficient for analysis
#-------- this file performs additional data filtering and munding to make the data usable
#-------- also perfroms plotting operations
#-------- should be used in conjunction to markdown
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


# unit_list = sample(unique(o_wo$unit_id), 20)

#remove very large positive meter differences
#-----negative stay becasue they are a result of the increase and could not need to be removed
#remake meter_diff_mnth
#remove negative now - these should be true negatives
#-----still an incorrect input but difference was too small to filter inw with first filer
o_wo_agg = o_wo %>%
  # filter(meter_diff_mnth<(mean(meter_diff_mnth)+sd(meter_diff_mnth))) %>%
  # .[,`:=`(meter_diff_mnth = (meter_sans0-lag(meter_sans0)) %>%
  #           replace_na(0)), by = .(unit_id)] %>%
  # filter(meter_diff_mnth>=0) %>%
  # filter(meter_diff_mnth>0 &
  #          abs(meter_diff_mnth)<15000) %>%
  # filter(unit_id == 576 |
  #          unit_id == 577) %>%
  #4L perfroms month aggregations
  .[,.(event_cost_mnth_ttl = sum(event_cost),
       event_cost_max = max(event_cost_cumm),
       meter_max_mnth = max(meter_sans0),
       wo_count = .N),
    by = .(fisc_pd_corr, unit_id)] %>%
  #3L makes cummlatives
  .[,`:=`(event_cost_mnth_ttl_cumm = cumsum(event_cost_mnth_ttl),
          event_cost_check = round(event_cost_max - cumsum(event_cost_mnth_ttl),2)),
    by = .(unit_id)] %>%
  #3L remove first group given greater than lead vioulation
  group_by(unit_id) %>%
  filter(!(meter_max_mnth > lead(meter_max_mnth) & row_number() == 1)) %>%
  data.table() %>%
  #3L recreates
  .[,`:=`(meter_0_corr = crrct0(meter_max_mnth)), by = .(unit_id)] %>%
  .[,`:=`(meter_diff_mnth = meter_0_corr-lag(meter_0_corr, default =  0)),
    by = .(unit_id)] %>%
  #1L removes rows where violation occurs per unit group
  .[, .SD[abs(meter_diff_mnth)<10*median(meter_diff_mnth)], by = .(unit_id)] %>%
  .[,`:=`(meter_bin = as.factor(floor_divide(meter_max_mnth, 25000)),
          meter_bin_o_corr = as.factor(floor_divide(meter_0_corr, 25000)),
          cost_bin = floor_divide(event_cost_mnth_ttl, 1000),
          event_cost_per_meter_per_mnth = event_cost_mnth_ttl/meter_diff_mnth)]

#this is potenttially not needed
#it does not impact the values that are seen in the boxplots
#   .[,
#     .SD[between(event_cost_per_meter_per_mnth,
#             0,
#             quantile(event_cost_per_meter_per_mnth, probs = .75, na.rm = T) + 1.5*IQR(event_cost_per_meter_per_mnth, na.rm = T)
#             )], by = .(meter_bin)] %>%
#   .[event_cost_per_meter_per_mnth<4,]


joined = merge.data.table(
  o_wo_agg,
  udcm,
  by = c("unit_id")
) %>%
  mutate(age = as.numeric(fisc_pd_corr-nu_year),
         age_bin = as.factor(age%/%365))

#Section: writing out RDS objects===============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
saveRDS(o_wo_agg, file = here(paste0("data/o_wo_agg_", Sys.Date(), ".rds")))
saveRDS(joined, file = here(paste0("data/joined_", Sys.Date(), ".rds")))



#
# joined_mdl_data = joined %>%
#   select(event_cost_per_meter_per_mnth, mcc, year, make:model, meter_bin)
#
# mod_lm = lm(event_cost_per_meter_per_mnth~., joined_mdl_data)
#
# mod_lm$fitted.values %>%
#   summary()
#
# joined_mdl_data$pred = mod_lm$fitted.values
#
# ggplot(joined_mdl_data) +
#   geom_point(aes(event_cost_per_meter_per_mnth, pred))
#
joined %>%
  ggplot(aes(age_bin, event_cost_per_meter_per_mnth)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 10, color = "red") +
  # facet_grid(cols = vars(mcc), scales = "free") +
  ylim(0, 1)
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
#
