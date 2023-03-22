//this is new stimuli for exp4

//order GG (EXP 2: SEED 2 ), GN (EXP 2: SEED 5 ), GP (EXP 3: SEED 2), NN (EXP 2: SEED 8), NP (EXP 3: SEED 3), PP (EXP 3: SEED 5)
//

var sti01 ={"obj":["E","A","B","E","E","E","A","E","E","B","E","A","E","E","B","E","E"],"time":["0","1.58485964871943","2.4797170702368","3.03150911811845","3.77028153345976","4.7460472010312","8.36137813981622","9.34254837845968","9.8379767664385","10.3602175787091","11.412569113678","12.1156002627686","13.1707280277734","13.9712906164898","16.3525959476829","17.3931826183405","18.6768956078456"],"org":["E","N","N","B","A","E","N","E","A","N","B","N","A","E","N","B","E"]}
var sti02 ={"obj":["E","A","E","E","A","E","E","B","A","E","E","B","B","E"],"time":["0","1.85531384241767","3.38031810472768","5.05719193741092","8.46969427075237","9.60306658188784","10.2735106396436","11.0211168162059","12.383211968001","13.8712708070099","14.5266081894163","15.0174697604962","18.0762388627045","19.3849314776192"],"org":["E","N","A","E","N","A","E","N","N","A","E","N","N","E"]}
var sti03 ={"obj":["E","B","B","E","A","E","A","E","E","B","A"],"time":["0","1.71347031719051","4.84782082261518","10.14070570971","11.2145265128929","12.1297609864146","13.2696178520564","13.9777452046155","14.923827551123","16.4120167701039","18.8056231893133"],"org":["E","N","N","E","N","A","N","A","E","N","N"]}
var sti04 ={"obj":["E","A","E","B","A","E","A","E","B","B","E"],"time":["0","0.558100597001612","4.77097709317978","5.26934814965352","6.39399715303443","9.21624923920214","11.0730049461126","14.3628266046504","16.6094351036008","18.5614492113236","19.3854554797156"],"org":["E","N","E","N","N","E","N","E","N","N","E"]}
var sti05 ={"obj":["E","B","B","B","A","E","A","E","A"],"time":["0","1.26507181324996","1.75285460893065","4.94807482720353","9.61576969968155","11.3486202274387","13.4728346138727","16.2710953822922","16.893383984454"],"org":["E","N","N","N","N","E","N","E","N"]}
var sti06 ={"obj":["E","B","E","A","B","A","E","B","A"],"time":["0","0.595286454772577","4.94191186643306","6.19729813979939","8.9699557996355","10.4848577813245","14.702037045343","16.1465184055269","18.8068767653313"],"org":["E","N","E","N","N","N","E","N","N"]}


var sti1 ={"obj":["E","A","E","B","E","A","B","E","B","A"],"time":["0","2","5","7","8.5","9.8","10.5","14.5","16.5","17.5"]}
var sti2 ={"obj":["E","B","A","E","A","B","E","A","B","E"],"time":["0","0.8","2","5.2","9.5","11","12.5","14","15","19.5"]}
var sti3 ={"obj":["E","B","E","A","B","E","A","B","A","E"],"time":["0","2.5","4","5","6.2","10.2","11","12","15","19.6"]}
var sti4 ={"obj":["E","B","A","A","B","E","B","A","E"],"time":["0","2.5","4","9.5","11","12.5","14","15","19.5"]}


var sti5 ={"obj":["E","B","E","A","E","A","B","E","E","B","A","E","E"],"time":["0","3","5","5.5","7","7.5","9","9.5","10.5","12","13","14.5","15.8"]}
var sti6 ={"obj":["E","A","E","B","E","A","B","E","E","A","B","E","E"],"time":["0","2.5","4","4.5","5.5","8","9","10","11.3","12","13","13.5","16"]}
var sti7 ={"obj":["E","A","B","E","E","A","B","E","E","B","E","A","E"],"time":["0","4","5","5.5","6","7","8.5","9","11.5","13.8","15.9","17.8","19.3"]}
var sti8 ={"obj":["E","A","B","E","E","B","E","A","B","E","E","A","E"],"time":["0","3","3.5","4.5","5.8","9","9.8","11.5","12.8","13.5","15.7","17","19"]}

var sti_list= [sti01,sti02,sti03,
			   sti04,sti05,sti06,
			   sti1,sti2,sti3,sti4,
			   sti5,sti6,sti7,sti8];

var sti_list_num=1
var con_list_num="regular"


var type_list=["G","N","P"];

sti_list[0].a="G"
sti_list[1].a="G"
sti_list[2].a="G"
sti_list[3].a="N"
sti_list[4].a="N"
sti_list[5].a="P"

sti_list[0].b="G"
sti_list[1].b="N"
sti_list[2].b="P"
sti_list[3].b="N"
sti_list[4].b="P"
sti_list[5].b="P"


for (var p=6;p<10;p++){
	sti_list[p].a="P"
	sti_list[p].b="G"
}

for (var p=10;p<14;p++){
	sti_list[p].a="G"
	sti_list[p].b="N"
}


for (var i=0;i<6;i++){
	sti_list[i].id=i+1
	sti_list[i].type="practice"
}

for (var i=6;i<14;i++){
	sti_list[i].id=i+1
	sti_list[i].type="formal"
}