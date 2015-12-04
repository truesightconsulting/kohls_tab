adm_curve
TRIM all _name column first!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

adm_setup
update: y means update, n means create a new client
calendar: 1 means yes, 2 means no.
bdgt_dim: if calendar is 1, then month_id/week_id is forced to be included
dim_...: all dimension selections, no other attribute with prefix dim_
general_message: NULL if nothing



adm_output
group: tab name on the output page
label: unique value required; for drill down table and chart use this column, those rows the format should be ..._number, such as market_1, market_2; the number refers to dma1, dma2, ....
type: tell Jay the field name of the output table
dim: fields to create output
chart: if table, then table; if for chart, then validate highchart type name; if for export, then excel
filter: only for tables need a filter; if so, put the dim name such as chan, dma; if not, put NULL