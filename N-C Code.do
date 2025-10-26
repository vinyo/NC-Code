


use "合并20250321.dta" 

global contrlol "secindus_ratio thirdindus_ratio logindsfirm loggdppp logpop_hj logsalary_av logforfirm"

* 变量处理

gen logfmpat_shouquan= log(专利授权_发明专利+1)
gen logsypat_shouquan= log(专利授权_实用新型+1)
gen logwgpat_shouquan= log(专利授权_外观设计+1)
gen logfmpat_shenqing= log(专利申请_发明专利+1)
gen logsypat_shenqing= log(专利申请_实用新型+1)
gen logwgpat_shenqing= log(专利申请_外观设计+1)


* 生成连续DID变量
gen post2018= year>=2018

egen openintense2018pre_mean = mean(openintense / (2013<=year<=2017)), by(citycode) 
	
gen GDID= openintense2018pre_mean*post2018  //生成强度DID变量	



* Table 1 Baseline regression result

reghdfe logfmpat_shenqing GDID $contrlol , absorb(year citycode) cluster(citycode)
outreg2 using baseline.doc, replace keep(GDID)  tstat bdec(3) tdec(2) ctitle(IP)  addtext( Control, Yes, City FE, Yes,Year FE, Yes)

reghdfe logsypat_shenqing GDID $contrlol , absorb(year citycode) cluster(citycode)
outreg2 using baseline.doc, append keep(GDID) tstat bdec(3) tdec(2) ctitle(UMP)  addtext( Control, Yes, City FE, Yes,Year FE, Yes)

reghdfe logwgpat_shenqing GDID $contrlol , absorb(year citycode) cluster(citycode)
outreg2 using baseline.doc, append keep(GDID) tstat bdec(3) tdec(2) ctitle(DP)  addtext( Control, Yes, City FE, Yes,Year FE, Yes)



* fig.2 -event study

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

* 运行事件研究回归-----发明专利申请
reghdfe logfmpat_shenqing D_rel* $contrlol  , absorb(citycode year) cluster(citycode)

* fig2a
  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ))


 reghdfe 发明申请专利质量_均值 D_rel* $contrlol  , absorb(citycode year) cluster(citycode)
 
 * fig2b
  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ))


 reghdfe 实用新型申请专利质量_均值 D_rel* $contrlol  , absorb(citycode year) cluster(citycode)
 
 * fig2c
  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ))




* table2 
 
*生成区域变量
gen region=1 if 所属地域 =="东部"
replace region=2 if 所属地域 =="西部"
replace region=3 if 所属地域 =="中部"

 * 生成城市2014年发明专利申请数变量：
bysort citycode: egen 专利申请总量2014 = mean(cond(year==2014, 专利申请总量 , .))

gen log专利申请总量2014=log(专利申请总量+1)

* 生成交互变量，将基准的DID变量与2014年专利数相乘
gen interact_DD_sumpat2014 = GDID * log专利申请总量2014

reghdfe logfmpat_shenqing GDID interact_DD_sumpat2014 $contrlol , absorb(year citycode) cluster(citycode) // col,1

reghdfe logfmpat_shenqing GDID interact_DD_sumpat2014 $contrlol if region==1 , absorb(year citycode) cluster(citycode) //col,2

reghdfe logfmpat_shenqing GDID interact_DD_sumpat2014 $contrlol if region!=1 , absorb(year citycode) cluster(citycode) //col,3

 
 
 
 

use "merge0411.dta"  // city-firm level panel data


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

* 生成相对时间交互项
forvalues t = -4(1)3 {
    if `t' != -1 {
        local t_clean = subinstr("`t'", "-", "n", 1)  // 将负号替换为n
        gen D_rel`t_clean' = (rel_year == `t') * openintense2018pre_mean
    }
}



* 运行事件研究回归-----企业创新行为


* fig3a

ppmlhdfe 当年独立申请的发明数 D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda)

coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))
	

* fig3b


ppmlhdfe 当年联合申请的发明数量 D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda)

coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))

	
	
* fig3c

ppmlhdfe 产学研合作专利数 D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda)

  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))
	


	
* fig3d

ppmlhdfe 核心专利申请总数 D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda)

coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))
	

* fig3e

reghdfe RDPersonRatio D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda) /*研发劳动结构*/

coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom) row(1)) 
	
	
*fig3f


reghdfe  RDSpendSumRatio  D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda) /*研发资金占营收比*/
  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom) row(1)) 
	
	

 
 
******* supplementary information tables&figs. *******
 
 
* 生成工具变量：到港口距离 × 时间趋势
gen logmindistance= log(distance_to_port)
gen iv_port = logmindistance * post_trend
 
* supplementary-table2
ivreghdfe logfmpat_shenqing (GDID=iv_port) $contrlol, absorb(year citycode) cluster(citycode) first

ivreghdfe logfmpat_shenqing (GDID=iv_port) $contrlol if region==1 , absorb(year citycode) cluster(citycode) first

ivreghdfe logfmpat_shenqing (GDID=iv_port) $contrlol if region!=1 , absorb(year citycode) cluster(citycode) first 
 
 


* supplementary-fig 2
twoway (kdensity 发明申请专利质量_均值 if year == 2014, lcolor(red)) ///
       (kdensity 发明申请专利质量_均值 if year == 2017, lcolor(gold)) ///
       (kdensity 发明申请专利质量_均值 if year == 2021, lcolor(blue)), ///
       legend(order(1 "2014" 2 "2017" 3 "2021") cols(3) )




 reghdfe 地区生产总值增长率 D_rel* $contrlol  , absorb(citycode year) cluster(citycode)
 
 * supplementary-fig3
  
coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom) rows(1) )


	   
	
 * supplementary-fig4a
 

gen logrdperson=log( RDPerson +1) /* 研发劳动投入*/
 

reghdfe logrdperson D_rel* $control2  , absorb(stkcd Sicda#year) cluster(Sicda)

coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom))

 * supplementary-fig4b
 
 
gen logrdcpa= log(RDSpendSum)
 
reghdfe logrdcpa  D_rel* $control2 , absorb(stkcd Sicda#year) cluster(Sicda) /*研发资金*/

coefplot, keep(D_rel*)  vertical title("") yline(0, lcolor(red)) xlabel(, labsize(small) ) ///
  order( D_rel_4 D_rel_3 D_rel_2 D_rel_0 D_rel_1 D_rel_2 D_rel_3) ///
  coeflabels( D_reln4 = "-4"  D_reln3 = "-3" D_reln2 = "-2" D_rel0 = "0" ///
    D_rel1 = "1"  D_rel2 = "2"  D_rel3 = "3" ) ///
	 xsize(5) ysize(4) ///
	legend(order(1 "95% Confidence Interval" 2 "Point Estimate" ) position(bottom) row(1)) 
 
 
 


