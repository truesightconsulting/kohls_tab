# set beta based on clv value
if (curve_f=="ninah") {
  curve$beta.decomp=curve$beta
  if(ex.setup$optimization_target==2){
    curve=curve[,beta:=beta*clv]
  }else if (ex.setup$optimization_target==3){
    curve=curve[,beta:=beta/factor_1]
  }
}else if (curve_f %in% c("exp","exp000","exp_cpm")){
  curve$a.decomp=curve$a
  if(ex.setup$optimization_target==2){
    curve=curve[,a:=a*clv]
  }
}
