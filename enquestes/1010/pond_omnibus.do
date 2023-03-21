import spss using "Microdades anonimitzades REO_1011.sav", clear

recode LLOC_NAIX 3=4, gen(NAIX_Q)
egen quotes=group(SEXE EDAT_GR NAIX_Q)

gen pond=1			
replace pond=	1.09375	if quotes==	1
replace pond=	0.57000	if quotes==	2
replace pond=	0.37400	if quotes==	3
replace pond=	1.09685	if quotes==	4
replace pond=	0.86333	if quotes==	5
replace pond=	0.33250	if quotes==	6
replace pond=	1.18687	if quotes==	7
replace pond=	1.60000	if quotes==	8
replace pond=	0.33340	if quotes==	9
replace pond=	1.17067	if quotes==	10
replace pond=	1.14344	if quotes==	11
replace pond=	0.51435	if quotes==	12
replace pond=	1.13610	if quotes==	13
replace pond=	1.12085	if quotes==	14
replace pond=	0.89250	if quotes==	15
replace pond=	1.14278	if quotes==	16
replace pond=	0.59000	if quotes==	17
replace pond=	0.52700	if quotes==	18
replace pond=	1.24067	if quotes==	19
replace pond=	1.42250	if quotes==	20
replace pond=	0.26036	if quotes==	21
replace pond=	1.12179	if quotes==	22
replace pond=	1.32818	if quotes==	23
replace pond=	0.33649	if quotes==	24
replace pond=	1.13183	if quotes==	25
replace pond=	1.06211	if quotes==	26
replace pond=	0.51962	if quotes==	27
replace pond=	1.20625	if quotes==	28
replace pond=	1.15038	if quotes==	29
replace pond=	0.60111	if quotes==	30
