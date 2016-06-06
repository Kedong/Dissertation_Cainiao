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
s_seller = rodps.query('select user_id,user_nick,brand_id,brand_name,status,shop_name,b2c_type,seller_status,bus_cat_id,bus_cat_name from tbcdm.dim_tm_seller_brand_grant where ds=20160601')
# 旗舰店-1  专卖店-2  专营店-3  商家账号法务-NA

## Total number ##
length(unique(s_seller$user_id))
## Total digital number ##
length(unique(s_seller[which(s_seller$bus_cat_id %in% c(474,7,263)),1]))+5  # 5=(1-服饰?(3个),2-母婴1个,378-家具家纺1个)

# s_seller_small = s_seller[which(s_seller$shop_name %in% c(unique(s_branch_notest$supplier_name))),]  # nrow=4135
s_seller_small = s_seller[which(s_seller$user_id %in% c(as.numeric(unique(s_branch_notest$supplier_id)))),c(1:2,7:10)]  # nrow=4614
# unique(s_seller[which(is.na(s_seller$b2c_type)),]$user_nick)  #b2c_type = NA is "商家帐号法务"
# s_seller_small is used for merging into s_branch_notest
s_seller_small = unique(s_seller_small)   # has one duplicate: 931421195 名龙堂官方旗舰店(b2c=1), 931421195 名龙堂数码专营店(b2c=1)
s_seller_small_nodup = s_seller_small[-which(s_seller_small$user_nick=="名龙堂数码专营店"),]   # nrow=4613
unique(s_seller_small_nodup[,5:6])  #474-数码卖场,7-数码3C,263-家用电器,1-服饰?(3个),2-母婴1个,378-家具家纺1个
# unique(s_seller_small_nodup[which(s_seller_small_nodup$bus_cat_id=="2"),])
nrow(s_seller_small_nodup)

## Users not in the TMall merchant list ##
length(unique(s_branch_notest[which(s_branch_notest$supplier_id %in% unique(s_seller[which(s_seller$bus_cat_id %in% c(474,7,263)),1])),8]))+5
unique(s_branch_notest[-which(s_branch_notest$supplier_id %in% unique(s_seller[which(s_seller$bus_cat_id %in% c(474,7,263)),1])),9])
## Turned out those are 品牌商 供应商 with different supplier_id

## Some stats of merchants ##
table(s_seller_small_nodup[which(s_seller_small_nodup$bus_cat_id=="474"),]$b2c_type)
temp_2 = unique(s_seller[which(s_seller$bus_cat_id %in% c("474","263","7")),1:2])
write.csv(temp_2,"sellerinfo.csv")
head(unique(s_seller[which(s_seller$bus_cat_id %in% c("7")),c(1:2,7:8)]))

### The list of users that also sell on TMall.com ###
# s_seller_small_nodup
### Merge s_seller_small_nodup into s_branch_notest
replenishment_seller_merge = merge(x=s_branch_notest, y=s_seller_small_nodup, by.x=c("supplier_id"),by.y=c("user_id"), all.y=T,sort=F)
# BELOW is for verification. Verified #
# nrow(replenishment_seller_merge)
# nrow(s_branch_notest)
# sum(s_branch_notest$supplier_id %in% s_seller_small_nodup$user_id)
# length(unique(replenishment_seller_merge$supplier_id))





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