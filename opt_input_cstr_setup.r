# optm based on constraint setup
print("Note: Generating OPTM Setup based on Constraint")
temp=temp.cstr.input[iter]
# setup time window
if (ex.setup$optimization_time==1){
  ex.setup$date_start=temp$date_start
  ex.setup$date_end=temp$date_end
}

# setup type and type value
ex.setup$optimization_type=1
ex.setup$optimization_type_value=temp[[names(result)[loop.cstr]]]

####################################################################################
# Customized part:
####################################################################################
#setup dim
index1=ex.media$chan1_id %in% as.numeric(strsplit(temp[["chan1_id"]],",")[[1]]) 
#index2=ex.media$chan2_id %in% as.numeric(strsplit(temp[["chan2_id"]],",")[[1]]) 
index=index1
ex.media$flag_chan[index]=1
ex.media$flag_chan[!index]=0

index1=ex.dma$dma1_id %in% as.numeric(strsplit(temp[["dma1_id"]],",")[[1]]) 
index2=ex.dma$dma2_id %in% as.numeric(strsplit(temp[["dma2_id"]],",")[[1]]) 
index=index1&index2
ex.dma$flag_dma[index]=1
ex.dma$flag_dma[!index]=0
####################################################################################