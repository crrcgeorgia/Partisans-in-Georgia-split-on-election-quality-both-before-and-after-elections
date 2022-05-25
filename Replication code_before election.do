set more off
use "ISFED_2021_07.09.2021.dta"
      
recode q11 (1=1 "Georgian Dream") (2/25=2 "Opposition") (-5 -1 -2=3 "No party/DK/RA"), gen(q11r)                                 //q11r Party Support

recode q3 (5 6 = 1 "Working") (1 2 3 4 7 8=2 "Not working"), gen(q3r)
recode q4 (1=1 "Public") (0 =2 "Not public") (-7=3 "Other"), gen(q4r)
recode q13 (4 3=1 "Agree") (1 2 -1 -2 =0 "Disagree, DK, RA"), gen(q13r)             
recode q13 (4 3=1 "Agree") (1 2 =0 "Disagree") (-1 -2 =.), gen(q13rr)                                                           //q13rr CEC Counting
recode q14 (1=1 "With major violations") (2=2 "With minor violations") (3=3 "Without violations") (-1 -2=.), gen(q14r)          //q14r 2021 Local Elect        

recode q40 (1=1) (-2 -1 0=0), gen(q40r)                              
recode q40 (1=1 "Possible") (0=0 "Impossible") (-1 -2 -9=.), gen(q40rr)                                                       //q40rr     Voting Privacy                      



gen emp=0
replace emp=3 if q3==6
replace emp=4 if (q3>-3 & q3<5) |(q3>6 & q3<9)
replace emp=1 if q4==1
replace emp=2 if q4==0 | q4==-1
recode emp (0=.)


gen WorkPlace=0
replace WorkPlace=1 if q4==1                                //1)Working in the public sector
replace WorkPlace=2 if q4==0 | q3==6                        //2)Working in non public sector
replace WorkPlace=3 if (q3>-3 & q3<5) | (q3>6 & q3<9)       //3)Not working            
recode WorkPlace (0=.)


foreach var of varlist q43_1-q43_10 {
recode `var' (1=1) (-1 -2 0=0), gen(`var'r)
}
recode q43_1r q43_2r q43_3r q43_4r q43_5r q43_6r q43_7r q43_8r q43_9r q43_10r (-9/-3=.)
gen own=q43_1r + q43_2r + q43_3r + q43_4r + q43_5r + q43_6r + q43_7r + q43_8r + q43_9r + q43_10r


recode q28_4r q28_5r q28_3r q41r q40r q38 q42r q31r q32r exp q11r edu emp q1r q4r q15_6r q13r q14r q20r q19_1r q19_2r q19_5r q21_3r q22r q22 prgm lead own q28_1r q28_2r q39r q24r q25r q26r q27r q29 q30 q29r q30r(-9/-1=.)





///////////////////////////////Logistic Regression/////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////



svyset id[pweight=weight]
svy: logit q13rr i.sex i.agegroup i.stratum i.q1r i.edu i.WorkPlace i.q11r own

margins, at(q1r=(1 2))

margins, at(sex=(1 2))

margins, at(agegroup=(1 2 3))

margins, at(stratum=(1 2 3))

margins, at(edu=(1 2 3))

margins, at(WorkPlace=(1 2 3 ))

margins, at(q11r=(1 2 3))

margins, at(own=(0 1 2 3 4 5 6 7 8 9 10))


svyset id[pweight=weight]
svy: logit q40rr i.sex i.agegroup i.stratum i.q1r i.edu i.WorkPlace i.q11r own

margins, at(q1r=(1 2))

margins, at(sex=(1 2))

margins, at(agegroup=(1 2 3))

margins, at(stratum=(1 2 3))

margins, at(edu=(1 2 3))

margins, at(WorkPlace=(1 2 3 ))

margins, at(q11r=(1 2 3))

margins, at(own=(0 1 2 3 4 5 6 7 8 9 10))



svyset id[pweight=weight]
svy: ologit q14r i.sex i.agegroup i.stratum i.q1r i.edu i.WorkPlace i.q11r own

margins, at(q1r=(1 2))

margins, at(sex=(1 2))

margins, at(agegroup=(1 2 3))

margins, at(stratum=(1 2 3))

margins, at(edu=(1 2 3))

margins, at(WorkPlace=(1 2 3 ))

margins, at(q11r=(1 2 3))

margins, at(own=(0 1 2 3 4 5 6 7 8 9 10))



stop

//regression

/*
svy: logit q15_6r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q13r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: mlogit q14r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q14r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q19_1r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q19_2r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q19_5r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q21_3r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: mlogit q22r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own, base(0)

svy: mlogit q22 i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own, base(3)

svy: logit prgm i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit lead i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q39r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q28_1r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q28_2r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q24r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q25r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q26r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q27r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q31r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q32r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q42r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: mlogit q38 i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own, base(3)

svy: logit q40r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q41r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

svy: logit q28_3r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own

//experiment
svy: logit q29r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own i.exp
svy: logit q30r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own i.exp

//margins, at(q11r=(1 2 3))
//marginsplot

//margins, at(emp=(1 2 3 4))
//marginsplot
*/
svy: mlogit q22r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own, base(0)
svy: logit q28_4r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own
svy: logit q28_5r i.sex i.agegroup i.stratum i.region_gr q1r i.edu i.emp i.q11r own


stop






