#######################################################################################
# Customized part:
#######################################################################################
print("Note: Dimensions Seletion for Certain Type of Optimization")
temp.flag=ex.cstr
if (ex.setup$optimization_type %in% c(3,4)){
  ex.plan.input=temp.flag[!is.na(sp_plan)]
  index=paste(ex.media$chan1_id,sep="+") %in% 
    unique(paste(ex.plan.input$chan1_id,sep="+")) 
  ex.media$flag_chan[index]=1
  ex.media$flag_chan[!index]=0
  index=paste(ex.dma$dma1_id,ex.dma$dma2_id,sep="+") %in% unique(paste(ex.plan.input$dma1_id,ex.plan.input$dma2_id,sep="+")) 
  ex.dma$flag_dma[index]=1
  ex.dma$flag_dma[!index]=0
}else if (ex.setup$optimization_type==10){
  #dim.multi=as.vector(as.matrix(ex.multigoal[iter]))
  dim.multi=ex.multigoal[iter]
  index1=ex.sales$sales1_id %in% as.numeric(strsplit(dim.multi[["sales1_id"]],",")[[1]]) 
  index=index1
  ex.sales$flag_sales[index]=1
  ex.sales$flag_sales[!index]=0
}