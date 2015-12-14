####################################################################################
# Customized part:
####################################################################################
# Post-optm Calc
print("Note: Post-optmization Calc.")
if (check.error==0){
  # halo effect of goal seek
  source(paste(path,"opt_modelinput_halo.r",sep=""),local = T)
  
  # put the start spend in for multi goal seek
  if (ex.setup$optimization_type==10) curve=merge(curve[,!"sp_min",with=F],start.sp,by="bdgt_id")

  # calculate final result
  curve$value_decomp=calc_decomp(curve$sp_current)
  curve$value_npv=curve$value_decomp*curve$clv
  if (ex.setup$optimization_type %in% c(3,5,9)) {
    curve$value_plan_decomp=calc_decomp(curve$sp_plan)
    curve$value_plan_npv=curve$value_plan_decomp*curve$clv
    curve$support_plan=1000*curve$sp_plan/curve$cps
  }else{
    curve$value_decomp_start=calc_decomp(curve$sp_min)
    curve$value_npv_start=curve$value_decomp_start*curve$clv
    curve$support_start=1000*curve$sp_min/curve$cps
  }
  curve$support=1000*curve$sp_current/curve$cps
  
  # summarize result
  print("Note: Summarizing output.")
  if (ex.setup$optimization_time==1){
    month=strftime(curve$week_name,"%m/%Y")
    curve$month_id=as.Date(paste("01/",month,sep=""),format="%d/%m/%Y")
    curve$month_name=curve$month_id
  }
  summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
  summary=vector("list",nrow(ex.output))
  bdgt_dim=str_split(ex.bdgt$bdgt_dim,",")[[1]]

  for (i in 1:nrow(ex.output)){
    names(summary)[i]=ex.output$label[i]
    dim=str_split(ex.output$dim[i],",")[[1]]
    dim1=c(dim,paste(as.vector(do.call(cbind,strsplit(dim,"_id"))),"_name",sep=""))
    if (ex.setup$optimization_type %in% c(3,5,9)) {
      summary.sp1=summary.sp[,list(spend=sum(sp_current),spend_start=sum(sp_plan),
                                   support=sum(support),support_start=sum(support_plan)),by=c(bdgt_dim[bdgt_dim %in% dim])]
      summary.npv=curve[,list(decomp=sum(value_decomp),value=sum(value_npv),
                              decomp_start=sum(value_plan_decomp),value_start=sum(value_plan_npv)),by=c(dim1)]
      if(sum(bdgt_dim %in% dim)==0){
        summary[[i]]=data.table(summary.npv,summary.sp1)
      }else{
        summary[[i]]=merge(summary.npv,summary.sp1,by=c(bdgt_dim[bdgt_dim %in% dim]),all.x=T)
      }
    }else{
      summary.sp1=summary.sp[,list(spend=sum(sp_current),spend_start=sum(sp_min),
                                   support=sum(support),support_start=sum(support_start)),by=c(bdgt_dim[bdgt_dim %in% dim])]
      summary.npv=curve[,list(decomp=sum(value_decomp),value=sum(value_npv),
                              decomp_start=sum(value_decomp_start),value_start=sum(value_npv_start)),by=c(dim1)]
      if(sum(bdgt_dim %in% dim)==0){
        summary[[i]]=data.table(summary.npv,summary.sp1)
      }else{
        summary[[i]]=merge(summary.npv,summary.sp1,by=c(bdgt_dim[bdgt_dim %in% dim]),all.x=T)
      }
    }
    if(!is.na(ex.output$filter[i])){
      index=grep(ex.output$filter[i],dim)
      dim=dim[-index]
      index=grep(ex.output$filter[i],dim1)
      dim1=dim1[-index]
      if (ex.setup$optimization_type %in% c(3,5,9)) {
        summary.sp1=summary.sp[,list(spend=sum(sp_current),spend_start=sum(sp_plan),
                                     support=sum(support),support_start=sum(support_plan)),by=c(bdgt_dim[bdgt_dim %in% dim])]
        summary.npv=curve[,list(decomp=sum(value_decomp),value=sum(value_npv),
                                decomp_start=sum(value_plan_decomp),value_start=sum(value_plan_npv)),by=c(dim1)]
        if(sum(bdgt_dim %in% dim)==0){
          temp=data.table(summary.npv,summary.sp1)
        }else{
          temp=merge(summary.npv,summary.sp1,by=c(bdgt_dim[bdgt_dim %in% dim]),all.x=T)
        }
      }else{
        summary.sp1=summary.sp[,list(spend=sum(sp_current),spend_start=sum(sp_min),
                                     support=sum(support),support_start=sum(support_start)),by=c(bdgt_dim[bdgt_dim %in% dim])]
        summary.npv=curve[,list(decomp=sum(value_decomp),value=sum(value_npv),
                                decomp_start=sum(value_decomp_start),value_start=sum(value_npv_start)),by=c(dim1)]
        if(sum(bdgt_dim %in% dim)==0){
          temp=data.table(summary.npv,summary.sp1)
        }else{
          temp=merge(summary.npv,summary.sp1,by=c(bdgt_dim[bdgt_dim %in% dim]),all.x=T)
        }
      }
      temp=rbindlist(list(summary[[i]],temp),fill=T,use.names = T)
      temp[is.na(temp)]="All"
      summary[[i]]=temp
    }
  }
  ####################################################################################
  # Customized part:
  ####################################################################################
  # format output
  summary_output=vector("list",nrow(ex.output))
  print("Note: Outputing result.")
  for (i in 1:nrow(ex.output)){
    # i=7
    dim=str_split(ex.output$dim[i],",")[[1]]
    if ("week_id" %in% dim==F){
      temp=summary[[ex.output$label[i]]][(spend!=0|spend_start!=0)&(decomp!=0|decomp_start!=0),!dim,with=F]
    }else temp=summary[[ex.output$label[i]]]
    
    # rename, delete col's, calc efficiency, reorder col's...
    if (ex.setup$optimization_type %in% c(3,5,9)){
      temp=temp[,':='(eff1=spend/decomp,eff1_start=spend_start/decomp_start,
                      eff2=value/spend,eff2_start=value_start/spend_start)]
      temp$eff1[temp$eff1==Inf]=0
      temp$eff1_start[temp$eff1_start==Inf]=0
      temp$eff2[temp$eff2==Inf]=0
      temp$eff2_start[temp$eff2_start==Inf]=0
      temp[is.na(temp)]=0
      temp.dim=names(temp)[grep("_name",names(temp))]
      temp[,c("spend","decomp","value","spend_start","decomp_start","value_start","support","support_start")]=
        round(temp[,c("spend","decomp","value","spend_start","decomp_start","value_start","support","support_start"),with=F],digits = 0)
      temp[,c("eff1","eff1_start","eff2","eff2_start")]=
        round(temp[,c("eff1","eff1_start","eff2","eff2_start"),with=F],digits = 1)
      if (dim[1]=="all_id") {
        temp=data.table(temp[,temp.dim,with=F],temp[,!temp.dim,with=F]
                        [,c("spend","support","decomp","value","eff1","eff2","spend_start","support_start","decomp_start","value_start","eff1_start","eff2_start"),with=F])
      } else{
        temp=data.table(temp[,temp.dim,with=F],temp[,!temp.dim,with=F]
                        [,c("spend","spend_start","support","support_start","decomp","decomp_start","value","value_start","eff1","eff1_start","eff2","eff2_start"),with=F])
      }
      temp=temp[order(-spend)]
      setnames(temp,c("spend","spend_start","support","support_start","decomp","decomp_start","value","value_start","eff1","eff1_start","eff2","eff2_start"),
               c("Spend","Planned Spend","Circ","Planned Circ","Revenue","Planned Revenue","Profit","Planned Profit","CPA","Planned CPA","ROI","Planned ROI"))
      temp=temp[,!c("CPA","Planned CPA"),with=F]
    }else{
      temp=temp[,!c("decomp_start","value_start","spend_start","support_start"),with=F]
      temp=temp[,':='(eff1=spend/decomp,eff2=value/spend)]
      temp$eff1[temp$eff1==Inf]=0
      temp$eff2[temp$eff2==Inf]=0
      temp[is.na(temp)]=0
      temp.dim=names(temp)[grep("_name",names(temp))]
      temp[,c("spend","decomp","value","support")]=
        round(temp[,c("spend","decomp","value","support"),with=F],digits = 0)
      temp[,c("eff1","eff2")]=
        round(temp[,c("eff1","eff2"),with=F],digits = 1)
      temp=data.table(temp[,temp.dim,with=F],temp[,!temp.dim,with=F]
                      [,c("spend","support","decomp","value","eff1","eff2"),with=F])
      temp=temp[order(-spend)]
      setnames(temp,c("spend","support","decomp","value","eff1","eff2"),
               c("Spend","Circ","Revenue","Profit","CPA","ROI"))
      temp=temp[,!c("CPA"),with=F]
    }
    # delete dimension columns for overall output table
    if (dim[1]=="all_id"){
      temp=temp[,!"all_name",with=F]
    }else if (ex.output$type[i]=="excel"){
      dim.id=data.table(dbGetQuery(conn,paste("select * from opt_modules_dim a inner join opt_label_modules_dim b on a.opt_label_modules_dim_id =b.id where client_id=",client_id,sep="")))
      dim.id$dim=paste(dim.id$dim,"_name",sep="")
      index=grepl("_name",names(temp))
      dim.name=merge(data.table(dim=names(temp)[index]),dim.id[,c("dim","label"),with=F],by="dim",all.x=T)
      setnames(temp,dim.name$dim,dim.name$label)
    }
    summary_output[[i]]=temp
    names(summary_output)[i]=ex.output$label[i]
    # export
    if (db.usage){
      index=ex.output$label==ex.output$label[i]
      ex.output$json[index]=toJSON(temp)
    }else write.csv(temp,paste("opt_output_",ex.output$label[i],".csv",sep=""),row.names = F)
  }
  ####################################################################################
  
  # convert to json and upload to DB
  source(paste(path,"opt_modelinput_tojson.r",sep=""),local = T)
} 