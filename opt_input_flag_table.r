# flag all the selected curves
####################################################################################
# Customized part:
####################################################################################
print("Note: Filtering Curves")
# merge all the info with curve
curve=merge(curve,ex.media[,c("chan1_id","flag_chan"),with=F],
            by=c("chan1_id"),all.y=T)
curve=merge(curve,ex.sales[,c("sales1_id","flag_sales"),with=F],
            by=c("sales1_id"),all.y=T)
curve=merge(curve,ex.dma[,c("dma2_id","dma1_id","flag_dma"),with=F],
            by=c("dma2_id","dma1_id"),all.y=T)
curve=merge(curve,ex.curvegroup[,c("curvegroup_id","flag_curvegroup"),with=F],
            by=c("curvegroup_id"),all.y=T)

# filter out non-selected curves 
flag=curve$flag_chan+curve$flag_sales+curve$flag_dma+curve$flag_curvegroup
curve=curve[flag==4,]

# merge shell with min and max
ex.cstr=merge(ex.cstr,ex.media[,c("chan1_id","flag_chan"),with=F],
              by=c("chan1_id"),all.y=T)
ex.cstr=merge(ex.cstr,ex.dma[,c("dma2_id","dma1_id","flag_dma"),with=F],
              by=c("dma2_id","dma1_id"),all.y=T)

ex.cstr.hidden=merge(ex.cstr.hidden,ex.media[,c("chan1_id","flag_chan"),with=F],
                     by=c("chan1_id"),all.y=T)
ex.cstr.hidden=merge(ex.cstr.hidden,ex.dma[,c("dma2_id","dma1_id","flag_dma"),with=F],
                     by=c("dma2_id","dma1_id"),all.y=T)


flag=ex.cstr$flag_chan+ex.cstr$flag_dma
ex.cstr=ex.cstr[flag==2,]
flag=ex.cstr.hidden$flag_chan+ex.cstr.hidden$flag_dma
ex.cstr.hidden=ex.cstr.hidden[flag==2,]
####################################################################################
if (iter==1) start.sp=ex.cstr[,c("bdgt_id","sp_min"),with=F]
