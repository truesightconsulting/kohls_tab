# prepare curve parameters for optm
print("Note: Calculating Curve Parameters")

####################################################################################
# Customized part:
####################################################################################
# merge clv table
curve=merge(curve,ex.clv[,c("sales1_id","clv"),with=F],by=c("sales1_id"),all.x=T)

# tweak curve based on time window
start.optm=as.Date(ex.setup$date_start)
end.optm=as.Date(ex.setup$date_end)
date.temp=optm.date(ex.setup$date_start,ex.setup$date_end)
range.wk.optm=date.temp$range.wk
out.wk.optm=date.temp$out.wk
n=length(out.wk.optm)
ex.setup$input_increment=n*ex.setup$input_increment
curve$a=n*curve$a
curve$b=curve$b/n

# # tweak a based on approval
# curve$a=curve$a*curve$approval

# set a based on clv value
curve$a.decomp=curve$a
if(ex.setup$optimization_target==2){
  curve=curve[,a:=a*clv]
}

# backup curve table
curve.org=curve

# response curve function
calc_decomp=function(x){
  # x is spend; output is sales
  curve$a.decomp*(1-exp(-curve$b*x*1000/curve$cps))
}

calc_npv=function(x){
  # x is spend; output is sales
  curve$a*(1-exp(-curve$b*x*1000/curve$cps))
}

# # decomp function
# calc_decomp=function(x){
#   # x is spend; output is sales
#   ad=function(x1,x2){(1 - ((1 - x1 * exp(1) ^ (log(0.5) / curve$hl)) / (exp(1) ^ ((x2/curve$cps) / 
#                                                                                     (curve$wks * curve$max) * (-log(1 - curve$hrf)) * 10))))}
#   curve$beta2*ad(x1=ad(x1=rep(0,length(curve$wks)),x2=x),x2=x)
# }
# 
# # npv function
# calc_npv=function(x){
#   # x is spend; output is sales
#   ad=function(x1,x2){(1 - ((1 - x1 * exp(1) ^ (log(0.5) / curve$hl)) / (exp(1) ^ ((x2/curve$cps) / 
#                                                                                     (curve$wks * curve$max) * (-log(1 - curve$hrf)) * 10))))}
#   curve$beta1*ad(x1=ad(x1=rep(0,length(curve$wks)),x2=x),x2=x)
# }