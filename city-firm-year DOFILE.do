

use "merge0331.dta" 


destring 当年独立申请的发明数量 当年独立申请的实用新型数量 当年独立申请的外观设计数量 当年联合申请的发明数量 当年联合申请的实用新型数量 当年联合申请的外观设计数量, replace


gen logfmpat_dep_app= log(当年独立申请的发明数量+1)
gen logfmpat_indep_app= log(当年联合申请的发明数量+1)

*生成区域变量
gen region=1 if 所属地域 =="东部"
replace region=2 if 所属地域 =="西部"
replace region=3 if 所属地域 =="中部"

global control2 " size roa tl tobin logprofit SA_index lnage  " 

* 生成连续DID变量
gen post2018= year>=2018

egen openintense2018pre_mean = mean(openintense / (2013<=year<=2017)), by(citycode) //计算每个城市2005年至2017年openintense变量平均值并存储为新变量openintense2018pre_mean
	
gen GDID= openintense2018pre_mean*post2018  //生成强度DID变量	



* 生成相对时间变量
gen rel_year = year - 2018

keep if inrange(rel_year, -4, 3)  // 保留政策前5年至后4年

* 生成相对时间交互项（兼容Stata语法）
forvalues t = -4(1)3 {
    if `t' != -1 {
        local t_clean = subinstr("`t'", "-", "n", 1)  // 将负号替换为n
        gen D_rel`t_clean' = (rel_year == `t') * openintense2018pre_mean
    }
}

* 运行事件研究回归----------------联合发明专利申请


* 绘制优化后的系数图-fig5b


ppmlhdfe 当年联合申请的发明数量 D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda)

  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))


* 绘制优化后的系数图-fig5a

ppmlhdfe 当年独立申请的发明数 D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda)

  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))
	
	
* 绘制优化后的系数图-fig5c

ppmlhdfe 产学研合作专利数 D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda)

  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))
	
 

gen logrdperson=log( RDPerson +1) /* 研发劳动投入*/
 
* 绘制优化后的系数图-fig5d 

reghdfe logrdperson D_rel* $control2  , absorb(stkcd Sicda#year) cluster(Sicda)

coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))
 

 * 绘制优化后的系数图-fig5e 
 
reghdfe RDPersonRatio D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda) /*研发劳动结构*/

  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom) row(1)) 
 
 
 * 绘制优化后的系数图-fig5f
 
gen logrdcpa= log(RDSpendSum)
 
reghdfe logrdcpa  D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda) /*研发资金*/

coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom) row(1)) 
 
 
* 绘制优化后的系数图-fig5g 

reghdfe  RDSpendSumRatio  D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda) /*研发资金占营收比*/
  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom) row(1)) 
	
	
* 绘制优化后的系数图-figA2-a (without control2) 


ppmlhdfe 核心专利申请总数 D_rel* , absorb(stkcd Sicda#year) cluster(Sicda)

  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))
	
		
* 绘制优化后的系数图-figA2-b(with control2) 


ppmlhdfe 核心专利申请总数 D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda)

  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))
		
	