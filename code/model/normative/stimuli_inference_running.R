#+ load stimuli -------------------
#' # load stimuli

load(file = "nAgB.Rda")
sqc_raw=pAnB

sqc_raw_sub_a=subset(sqc_raw,obj=="A")
sqc_raw_sub_a$obj_idx=seq(1:nrow(sqc_raw_sub_a))
sqc_raw_sub_b=subset(sqc_raw,obj=="B")
sqc_raw_sub_b$obj_idx=seq(1:nrow(sqc_raw_sub_b))

sqc_raw_sub_ab=rbind(sqc_raw_sub_a,sqc_raw_sub_b)
sqc_raw_sub_ab=sqc_raw_sub_ab[order(sqc_raw_sub_ab$time),]
sqc_raw_sub_ab$ab_idx=seq(1:nrow(sqc_raw_sub_ab))

sqc_raw_sub_e=subset(sqc_raw,obj=="E")
sqc_raw_sub_e$obj_idx=seq(1:nrow(sqc_raw_sub_e))
sqc_raw_sub_e$ab_idx=rep(0,nrow(sqc_raw_sub_e))

sqc_raw=rbind(sqc_raw_sub_ab,sqc_raw_sub_e)
sqc_raw=sqc_raw[order(sqc_raw$time),]
sqc_raw$total_idx=c(1:nrow(sqc_raw))