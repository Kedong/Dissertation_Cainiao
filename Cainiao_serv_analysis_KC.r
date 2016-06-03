# rm(list=ls());

library(RODPS)
library(Hmisc)
library(plyr)
rodps.list.tables()
# rodps.drop.table("view_wh_inv_di")

# s_branch_table = rodps.load.table('cndata.s_branch_wh_replenish_plan')
# fdf_www_branch_table = rodps.load.table('cndata.cnods_fdf_www_branch_wh_replenish_plan')

s_branch_table = rodps.query('select * from cndata.s_branch_wh_replenish_plan where pt=20160601000000')
# s_branch_0531 = rodps.query('select * from cndata.cnods_fdf_www_branch_wh_replenish_plan where ds=20160531')

# item_cat = rodps.query('select spu_id, product_id, category_id, title from tbods.s_tmall_product where ds=20160601 limit 10000')
# a temporarily useless table here #

ls(s_branch_table)
# ls(s_branch_0531)

s_branch_table$plan_replenish_num = as.numeric(s_branch_table$plan_replenish_num)
s_branch_table$actual_replenish_num = as.numeric(s_branch_table$actual_replenish_num)
s_branch_table$diff = s_branch_table$plan_replenish_num - s_branch_table$actual_replenish_num

# test accounts
# "c测试账号133","供销测试帐号03品牌商","商家测试帐号029","商家测试帐号19","商家测试帐号26","商家测试帐号28",
# "商家测试帐号6","商家测试帐号7","商家测试帐号80","商家测试帐号89","菜鸟测试帐号1001","商家测试帐号42"
# sort(unique(s_branch_table$creator_name))
test_id = which((s_branch_table$creator_name %in% c("c测试账号133","供销测试帐号03品牌商","商家测试帐号029",
                                                    "商家测试帐号19","商家测试帐号26","商家测试帐号28",
                                                    "商家测试帐号6","商家测试帐号7","test","商家测试帐号80",
                                                    "商家测试帐号89","菜鸟测试帐号1001","商家测试帐号42")) | 
                  (s_branch_table$supplier_name %in% c("c测试账号133","供销测试帐号03品牌商","商家测试帐号029",
                                                       "商家测试帐号19","商家测试帐号26","商家测试帐号28",
                                                       "商家测试帐号6","商家测试帐号7","test","商家测试帐号80",
                                                       "商家测试帐号89","菜鸟测试帐号1001","商家测试帐号42")))
s_branch_notest = s_branch_table[-test_id,]

# some stats #
nrow(s_branch_notest)
sum(s_branch_notest$plan_replenish_num==0 & s_branch_notest$actual_replenish_num==0)   # num of both 0
sum((s_branch_notest$plan_replenish_num!=0 | s_branch_notest$actual_replenish_num!=0) & (s_branch_notest$plan_replenish_num-s_branch_notest$actual_replenish_num==0))
length(unique(s_branch_notest$supplier_name))  # notice that id & name does not match exactly, USE ID!
length(unique(s_branch_notest$supplier_id))
temp_1 = unique(cbind(s_branch_notest$supplier_id,s_branch_notest$supplier_name))
temp_1[duplicated(temp_1[,1]),]
#[1,] "1770526498" "容声官方旗舰店"
#[2,] "112083600"  "世纪百诚官方旗舰店"
#[3,] "1854775143" "JBL品牌商"
#[4,] "1646373572" "NEW7分销平台"
#[5,] "684765000"  "JBL官方旗舰店"
#[6,] "2042423866" "AO史密斯品牌商"

# diff needs scaling #
s_branch_notest_aug = ddply(s_branch_notest, "scitem_id", transform, diff.group.mean=mean(diff), diff.group.sd=sd(diff))
describe(s_branch_notest_aug[is.na(s_branch_notest_aug$diff.group.sd),]$diff)
describe(s_branch_notest_aug$diff)

# some descripition of difference b/w actual and planned when neither is 0
s_branch_nonzero = s_branch_notest_aug[-which(s_branch_notest$plan_replenish_num==0 & s_branch_notest$actual_replenish_num==0),]
Diff_0 = s_branch_nonzero
Diff_1 = s_branch_nonzero[which(s_branch_nonzero$diff>=-1 & s_branch_nonzero$diff<=1000),]
Diff_2 = s_branch_nonzero[which(s_branch_nonzero$diff>=-1 & s_branch_nonzero$diff<=100),]
Diff_3 = s_branch_nonzero[which(s_branch_nonzero$diff>=7 & s_branch_nonzero$diff<=100),]
# draw histogram #
ggplot(Diff_0, aes(Diff_0$diff)) + geom_histogram()
ggplot(Diff_1, aes(Diff_1$diff)) + geom_histogram() # binwidth = 5
ggplot(Diff_2, aes(Diff_2$diff)) + geom_histogram(binwidth = 1)
ggplot(Diff_3, aes(Diff_3$diff)) + geom_histogram(binwidth = 1)

### warehouse info ###
# think about CDC and DC #
s_branch_notest[which(s_branch_notest$warehouse_code=="not_any_warehouse"),]
s_warehouse = rodps.query('select * from cndata.s_wh_ext where ds=20160601')
# Justify warehouse type (CDC, DC, RDC) through TABLE s_wh_ext

### seller info ###
s_seller = rodps.query('select * from tbcdm.dim_tm_seller_brand_grant where ds=20160601')
# 旗舰店-1  专卖店-2  专营店-3  商家账号法务-NA

s_seller_small = s_seller[which(s_seller$shop_name %in% c(unique(s_branch_notest$supplier_name))),]
unique(s_seller[which(is.na(s_seller$b2c_type)),]$user_nick)
美菱品牌商




######################################################
######################################################

which(sale_diff!=0)
sale_diff[which(sale_diff!=0),]

x$sys_advise_sale_ratio[which(is.na(x$sys_advise_sale_ratio)==0)]

# Sample, #obs is 0.1*total #
rodps.sample.srs('cndata.s_branch_wh_replenish_plan','small_s_branch',0.001)
# rodps.sample.strat - stratified sampling based on prob or lines #

unique(s_branch_0531$gmt_sell_begin)
unique(s_branch_table$cycle_id)[order(as.vector(unique(s_branch_table$cycle_id)))]
s_branch_notest = s_branch_table[-which(s_branch_table$gmt_sell_end=="1970-01-01 08:00:00"),]
nrow(s_branch_notest)