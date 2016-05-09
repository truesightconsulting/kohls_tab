# # halo effect of goal seek
# source(paste(main.path,"opt_modelinput_halo.r",sep=""),local = T)

# calculate final result
curve$value_decomp=calc_decomp(curve$sp_current)
curve$value_npv=curve$value_decomp*curve$clv
if (ex.setup$optimization_type %in% c(3,5,9)) {
  curve$spend_start=curve$sp_plan
  curve$value_decomp_start=calc_decomp(curve$sp_plan)
  curve$value_npv_start=curve$value_decomp_start*curve$clv
  curve$support_start=1000*curve$sp_plan/curve$cps
}else{
  curve$spend_start=curve$sp_min
  curve$value_decomp_start=calc_decomp(curve$sp_min)
  curve$value_npv_start=curve$value_decomp_start*curve$clv
  curve$support_start=1000*curve$sp_min/curve$cps
}
curve$support=1000*curve$sp_current/curve$cps

# summarize result
input_sp=c("sp_current","spend_start","support","support_start")
output_sp=c("spend","spend_start","support","support_start")
input_decomp=c("value_decomp","value_npv","value_decomp_start","value_npv_start")
output_decomp=c("decomp","value","decomp_start","value_start")

source(paste(main.path,"opt_modelinput_post_calc_agg.r",sep=""),local = T)

# format output
# new var 
metric_eff=c("eff1","eff1_start")
name_eff=c("ROI","Planned ROI")
f_eff=c("value/spend","value_start/spend_start")

# existing var
metric_reg=c("spend","decomp","value","spend_start","decomp_start","value_start","support","support_start")
name_reg=c("Spend","Revenue","Profit","Planned Spend","Planned Revenue","Planned Profit","Impressions","Planned Impressions")

# table column order 
order_all=c("spend","support","decomp","value","eff1","spend_start","support_start","decomp_start","value_start","eff1_start")
order_other=c("spend","spend_start","support","support_start","decomp","decomp_start","value","value_start","eff1","eff1_start")

# any columns to be dropped for all, excel and other tables; NAME HERE IS AFTER RENAMED!!!!!!!!!!!!!!!!!!!!!!!!!!
drop_all=c("all_name")
drop_excel=c()
drop_other=c()

source(paste(main.path,"opt_modelinput_post_calc_format.r",sep=""),local = T)