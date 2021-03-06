# rm(list=ls());

library(RODPS)
library(Hmisc)
library(plyr)
library(MASS)
library(ggplot2)
rodps.list.tables()
# rodps.drop.table("view_branch_wh_replenish_plan")

s_branch_table = rodps.load.table('s_branch_wh_replenish_plan_KC')
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
                                                       "商家测试帐号89","菜鸟测试帐号1001","商家测试帐号42")) |
                  (s_branch_table$memo %in% "虚拟订单"))
s_branch_notest = s_branch_table[-test_id,]

# some stats #
cat("######### Some Statistics for Replenishment #########")
cat("######### Total Num Valid Observations #########")
nrow(s_branch_notest)
cat("######### Num Both Planned & Actual Replenishment = 0 #########")
sum(s_branch_notest$plan_replenish_num==0 & s_branch_notest$actual_replenish_num==0)   # num of both 0
cat("######### Num Difference b/w Planned & Actual = 0 (excluded) #########")
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
ggplot(Diff_1, aes(Diff_1$diff)) + geom_histogram(binwidth = 3) # binwidth = 5
ggplot(Diff_2, aes(Diff_2$diff)) + geom_histogram(binwidth = 1)
ggplot(Diff_3, aes(Diff_3$diff)) + geom_histogram(binwidth = 1)


### seller info ###
s_seller = rodps.load.table('s_tm_seller_KC')
# 旗舰店-1  专卖店-2  专营店-3  商家账号法务-NA
# delete test accounts
test_id_2 = which((s_seller$user_nick %in% c("商家测试帐号61","商家测试帐号53","商家测试帐号57","商家测试帐号43",
                                             "商家测试帐号10","商家测试帐号110","商家测试帐号029","商家测试帐号18",
                                             "商家测试帐号55","商家测试帐号50","商家测试帐号3","商家测试帐号11",
                                             "商家测试帐号104","商家测试帐号54","商家测试帐号35","商家测试帐号109",
                                             "天猫国际测试账号1","b2ctest04测试","b2ctest10测试","商家测试帐号51",
                                             "商家测试帐号83","商家测试帐号14","测试旗舰店","测试手机专营店")) | 
                    (s_seller$shop_name %in% c("商家测试帐号61","商家测试帐号53","商家测试帐号57","商家测试帐号43",
                                               "商家测试帐号10","商家测试帐号110","商家测试帐号029","商家测试帐号18",
                                               "商家测试帐号55","商家测试帐号50","商家测试帐号3","商家测试帐号11",
                                               "商家测试帐号104","商家测试帐号54","商家测试帐号35","商家测试帐号109",
                                               "天猫国际测试账号1","b2ctest04测试","b2ctest10测试","商家测试帐号51",
                                               "商家测试帐号83","商家测试帐号14","测试旗舰店","测试手机专营店")))
s_seller_notest = s_seller[-test_id_2,]


## Total number ##
cat("######### Some Statistics for TMall sellers #########")
cat("######### Num of Total Sellers #########")
length(unique(s_seller_notest$user_id))
## Total digital number ##
cat("######### Num of Digital Sellers #########")
length(unique(s_seller_notest[which(s_seller_notest$bus_cat_id %in% c(474,7,263)),1]))+5  # 5=(1-服饰?(3个),2-母婴1个,378-家具家纺1个)

# s_seller_small = s_seller[which(s_seller$shop_name %in% c(unique(s_branch_notest$supplier_name))),]  # nrow=4135
cat("######### TMall Sellers who Receive Cainiao Replenishment Service #########")
s_seller_small = s_seller_notest[which(s_seller_notest$user_id %in% c(as.numeric(unique(s_branch_notest$supplier_id)))),c(1:2,7:10)]  # nrow=4614
# unique(s_seller[which(is.na(s_seller$b2c_type)),]$user_nick)  #b2c_type = NA is "商家帐号法务"
# s_seller_small is used for merging into s_branch_notest
s_seller_small = unique(s_seller_small)   # has one duplicate: 931421195 名龙堂官方旗舰店(b2c=1), 931421195 名龙堂数码专营店(b2c=1)
s_seller_small_nodup = s_seller_small[-which(s_seller_small$user_nick=="名龙堂数码专营店"),]   # nrow=4613
unique(s_seller_small_nodup[,5:6])  #474-数码卖场,7-数码3C,263-家用电器,1-服饰?(3个),2-母婴1个,378-家具家纺1个
# unique(s_seller_small_nodup[which(s_seller_small_nodup$bus_cat_id=="2"),])
cat("######### Num TMall Digital Sellers who Receive Cainiao Service #########")
nrow(s_seller_small_nodup)

## Users not in the TMall merchant list ##
length(unique(s_branch_notest[which(s_branch_notest$supplier_id %in% unique(s_seller_notest[which(s_seller_notest$bus_cat_id %in% c(474,7,263)),1])),]$supplier_id))+5
unique(s_branch_notest[-which(s_branch_notest$supplier_id %in% unique(s_seller_notest[which(s_seller_notest$bus_cat_id %in% c(474,7,263)),1])),]$supplier_id)
## Turned out those are 品牌商 供应商 with different supplier_id

## Some stats of merchants ##
cat_num=263
user_dist_1 = table(s_seller_small_nodup[which(s_seller_small_nodup$bus_cat_id==cat_num),]$b2c_type)
# we focus on user_id, otherwise things become messy // don't consider user_nick
temp_2 = unique(s_seller_notest[which(s_seller_notest$bus_cat_id==cat_num),c(1,7,9)])
# same user_nick would have different user_ids
# for example
# 2440758352 暴风魔镜旗舰店
# 2468310833 暴风魔镜旗舰店
tmall_cat_dist = table(temp_2$b2c_type)
chisq.test(cbind(user_dist_1,tmall_cat_dist))   #p-value < 2.2e-16


### The list of users that also sell on TMall.com ###
# s_seller_small_nodup
### Merge s_seller_small_nodup into s_branch_notest
replenishment_seller_merge = merge(x=s_branch_notest, y=s_seller_small_nodup, by.x=c("supplier_id"),by.y=c("user_id"), all.y=T,sort=F)
# BELOW is for verification. Verified #
# nrow(replenishment_seller_merge)
# nrow(s_branch_notest)
# sum(s_branch_notest$supplier_id %in% s_seller_small_nodup$user_id)
# length(unique(replenishment_seller_merge$supplier_id))

replenishment_seller_merge$b2c_type = as.factor(replenishment_seller_merge$b2c_type)
rep_b2c_m1 = lm(diff~b2c_type, replenishment_seller_merge)
summary(rep_b2c_m1)
# 未经warehouse调整过的，未经product调整过的，b2c type对diff不具有显著性
# 以下考虑warehouse 和 product 调整


### warehouse info ###
# think about CDC and DC #
s_branch_notest[which(s_branch_notest$warehouse_code=="not_any_warehouse"),]
s_branch_table = rodps.load.table('s_warehouse_KC')
# Justify warehouse type (JHC, CDC, DC, RDC) through TABLE s_wh_ext
s_warehouse[which(s_warehouse$store_code=="LFA201"),2]="DC"
s_warehouse = unique(s_warehouse[,c(1,3)])
s_warehouse = rbind(s_warehouse,c("STORE_11326863","no_records"))
s_warehouse = rbind(s_warehouse,c("not_any_warehouse","no_records"))
# write.csv(s_warehouse,"a.csv")


# Merge warehouse and s_branch
reple_wh_merge = merge(x=replenishment_seller_merge, y=s_warehouse, by.x=c("warehouse_code"),by.y=c("store_code"), all=F,sort=F)
# 考虑经过warehouse调整的replenish
reple_wh_merge$b2c_type = as.factor(reple_wh_merge$b2c_type)
## 把所有三四集仓code成DC ##
reple_wh_merge[which(reple_wh_merge$store_role=="SANJC"),]$store_role="DC"
reple_wh_merge[which(reple_wh_merge$store_role=="JHC"),]$store_role="CDC"
# Analysis
rep_b2c_m2 = lm(diff~b2c_type*store_role, data=reple_wh_merge)
summary(rep_b2c_m2)
## RESULT: store_roleDC ** ##

xtabs(diff~b2c_type+store_role, data=reple_wh_merge)
ddply(reple_wh_merge, .(b2c_type,store_role), summarise, mean(diff))

## 考察 why CDC has such large diff ##
## 从product price角度考虑? ##
describe(reple_wh_merge$diff)


### product price ###
# DONT RUN #
#s_tm_product = rodps.query('select product_id,spu_id,category_id,price,sales_promotion from tbods.s_tmall_product where ds=20160605')
#s_tm_product = rodps.query('select * from product_info_KC_1')
## NOT suitable dataset





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