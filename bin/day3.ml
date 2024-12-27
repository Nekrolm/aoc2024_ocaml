open Printf

let pat = Str.regexp {|mul([0-9]+,[0-9]+)|}
let () = print_string "hello"

let input =
  {input|
;({where()+'what()mul(445,324)#what()select()(+mul(430,603)why()@^&why()<mul(991,973)what()*~where()mul(320,361)from()how()mul(39,679)+what(856,339)what()why()*{+mul(73,323)from() [select()-,what()!(mul(133,891)?,mul(996,200)who()when()mul(668,190)mul(537,572)who()who(){why()mul(455,630)!$!from()do()@%-!when()who()-:mul(77,345)@(/when()^~mul(759,479)})++(+</mul(112,700) why()>'$>{!*]mul(11,880)#?$##:@mul(653,180)mul(343,172)^*({]!&!@~mul(121,364)>(~ mul(905,512)!''&;{%?why()mul(253,82)%&mul(173,725)''[mul(948,412)])how()select()) don't()where()mul(688,386)mul(798,885)+:&:)mul(139,466)from();what()'*( +,mul(337,913)!?,why(315,219)why()]@'when()mul(299,848)?-mul(404,810)don't()who()[$@$)mul(562,587)/%%mul(930,204)?$?why()mul(671,431)]from()#+(who()who();mul(467,198)select()what(748,331)from()[when()%@:do();mul(648,804))~where()who()mul(433,801)':how();@!'^{mul(294,95)&?mul(394,740)((}:select())mul(627,328)?how()who()>+where()select()!mul(726,536)^what()select(603,656)#@<!)why()@mul(665,531)<#&( where()@why()when()/mul(651,419)when()who()@mul(251,901)mul(917,673)mul(139,942)<(-]don't()>];~~mul(100,802)where()what()mul(675,411)$((+how();/%mul(858,719)from()-do()what()*]~;mul(873,971)//%mul(78,89)~'from()&where()@from()mul(308,52)mul(959,448)why()#%select()#(mul(379,76)}how(678,794)mul(575,884)how())*do()mul(829,831)why()how(999,884)who()where()^(@mul(686,468)}from() how()&<&mul(886,603)</?,mul(171,272)how()-%where()mul(766,411)#$^&,why()'),,mul(763,590)from()who()> <when()#mul(486,962){[+how()&:+mul(572,266)}@!~who())%select()mul(15,796) 'how()!$select()mul(147,232)(@; ;mul(267,231)do()who()/mul(859,800)how()& select(396,707)-mul(291,826)&what()mul(512,787)how()?{%'#<?don't() *@!mul(767,135)+}~,how(){,)mul(727,699)-/}[-mul(566,179)%@$- !^mul(193,546)&}from(),mul(705,766):>+;]?from()why()*do()&~!;mul(247,653)-mul(828,136)when()select()from()mul(258,842)what()?<)#];mul(517,650)!%+'~-mul(215,28)*-^*mul(810,676)select()%who():where(){>who()when()mul(632,298)how()'how()from()mul(823,219)what()'@^who():when()mul(400,833)from()$%'/,[(]mul(216,848),#!/#:mul(725,486) }~~<(mul(564,772)where())mul(819,487)]:)-&/}mul(71-(mul(732,511)~ (where()<?^&mul(650,977);?{what() ]where()mul(81,106),$%mul(909,962)@,how()@mul(452,162)@mul(468,658)select()~?('~**mulwhen()]:,<mul(8,295)^from()[when()$~]mul(697,476)+why()<how()*who()~from()}mul(755,649)&^!}select()mul(298,890)why()<'&mul(912,689^what()+do()*when()mul(577*/what()-<#how();!^mul(131,588)how(461,8)from()]!mul(331,224)%<$<^:]~{mul(918,263)select()<~;mul(374,998)^mul(182,130)how(61,655)<%who()who()&,what(743,368)why()mul(623,620)'?why()mul(783,314#select()-when()]mul(153,641)~)</^*where()mul(148,613how(),]%/*<why())from()mul(663,15)where()(:*mul(90,830)where())>:- mul(773,414)what()how(581,907)[how(218,409)?//mul(94,697))<what()!- <]!mul(530#when()~)who(),)select()@/mul(373,912)mul(684,83)>)?<-#>mul(89,657)mul(385,928)%what()why(467,355)&->mul(757,734) ]mul(517,244)]select()when(253,938)^>/'&(mul(232,10)mul(665,886&mul(880,285)
what()#where()&$mul(310,297)%>why()^<mul(768,599where()mul(999,433)^who()select()(?>what()]mul(593,941^where(),-select()mul(937,169),+!why()>when()([>+do()%]select()why()mul(104,652)[*;^who()-}#(mul(963,537)mul(579,212)select()--{from(644,349)+:select(857,111)mul(531,515)(&,%how()}$:mul(989,356){;{,))}'mul(142,281)&@~!]mul(289,916)%+when()from()/$:mul(551,55)from()from()$how();;why()}mul(738,532)< (~mul(547,99))how()>mul(159,62)(^*who()where()@@who()*^do()^'*&what()mul(34,665)-do()%@how(732,538)mul(459,928)what()*mul(407,795)+^+@{[:[mul(695,539)mul(359,852)mul(132,930)mul(729,877)/^(^mul(727,452)(<why() {(<?mul(390,950);when())what(398,903))when()^'()mul(998,792)why()when()(! $mul(583,453)>mul(782,665)when()>*mul/^mul(688,251),(mul(117,838)mul(313,301)??from():mul(918,351)>mul(513,285](what()&why()~what()who()mul(493,941)(%who():mul(298,591)]:select()&/mul(91,464)}(%*,{-mul(316,801)/$+'~select()[!do()?>%,mul(253,555)*@when():why()>what()mul(511,598)/where() /<from()&-^mul(947,949)$/mul(20,922)@>#'?mul(193,147)mul(693,462)>}~}mul(942,134)%mul(545,298)select(883,847)'&;what()why()])]select()mul(932,859),;*who()mul[what()where()/&%mul(479,587)%?{<-^]select()#where()mul(992,583)who()[&[&[mul(714,106){<mul(505,757)%(][mul(579,833)>% where())]~'mul(158,893)/select(){when()~select()*:from()[don't()@select()>~^$how()%}mul(262,794)who()/how()mul(856,695)}(;who()+from()what()mul(203,385)select()@<^;?>-';mul(409,500);why()+<who())> mul(721-{where();{mul(419,384)when()from()where()+~%select()mul(23,108)/~}select(932,457)/[from()mul(381,237)(mul(249,569)mul(332,31)from()select()$mul(760,792){'why()%< @mul(378,678)-/mul(146,764)+:[why()mul(262,148)*<mul(121,174):why()&how(307,405)select()#^^when()mul(392,80)$,:]!>?,who()%mul(209,306)$]*@from()>(;how(),mul(992,237)from()};+{<mul(670,900)#^{$mul(661)how()*do()~)'who()%mul(151,599)what() [where()%#mul(673^(who()do()mul(544,71)${?:}}&mul(662,996)  /}+do()-when()}[!{mul(609,652)?where() why()do()&what()select()@mul(335,388)$[mul(213,850)how()^mul(495,85)-+mul(391,227)why()![^/'how()mul(248,277)mulwhen(){who()] mul(488,770)from()^'*from()[where()mul(700,675)mul(97,173)mul(284,269)!why(205,144)]-[[<mul(490,718)<$?~+mul(931,41)who()#(*what()}[mul(148,888){( >who()&when()!mul(941,631))~what()$what()]mul(528,570)what()select()when()<#}what()don't()>*#-mul(379,986)!()]select(9,153)mul(942,395)><#?what(313,440)where()@how()from(){mul(663,775)*^what()[mul(584,62)when();$#mul(270? ])>&select(){ ^how()mul(837,91)what()where(){%^&!!mul#>^~<'#]-%mul(160,825)?*&%)({!'$mul(488,459)mul(466,879)#,!%+ ](+%mul(371,785)-(,mul(884,509)<,select();mul(423,954))what()[what()what()~mul(548,650)']don't()where()<?-:%>[]'mul(892,161)who();)]?mul(54,246)!-mul(140,679)who()@)@mul(193,36)mul(64,972)&;+)@mul(152,414)where()when(748,355)mul(778,929)*<[&*$mul(549,697),mul(183,897)when()/':&%:&#mul(428,989)mul(220,961)}do()what()$what(536,484)*[]:$select()}mul(943,870)'mul(658,52)->(^!;select()^)$mul(474,140)where()mul(700,771)]^%>]^'mul(933,512)
:]$^do()select()^~]who()& ^}mul(142,82)(?{mul(324,299) ^&@mul(426,954)*why()mul(535,943)'^)*<]mul(540,959)~select(),how(770,994)select()mul(623,558)<why()why()}^]do(),!mul(953,55),#{mul(233,892)!}(mul(80select()where()'what()how(835,158)mul(802,151)+from()(from(128,497)what()*([mul(29,512)mul(92,69)%)?mul(702select(342,843)-(~>#mul(778,72)+-*where(631,115)how()mul(777,784):+mul(834,445)*-$do()mul(504,419)mul(760,788)%~why()-mul(861,519)},how()~*#mul(654,701)/;<how()< ?(mul(395,393)mul(229,309!&when()select()-mul(22,564)why()select()+#-who()}%do()when()~,mul(899,367)<,<){ mul(881,948)from(710,608)from();who()/mul(672,218)!$where()why():mul(156,654)how()why()*where()&when()#<#<mul(631,751):$-^:/mul(519,756)}select()<mul(994,484)#<#why()/mul(208,160)/~%}mul(982,443)don't()~mul(235,731)~what()}<$^{(mul(816,163)?who()who()select(),/&mul(200,800)'/?from(),mul(17,848)where())[,~!^]{mul(837,649)('# }](^mul(710,747),mul(43,339)):+[%what()/)mul(250,801)mul(177,503)~>/mul(765,616)}- ]why()!#mul(695,46)~!/,#~&mul(867,931)mul(69,230)<!-*who()+mul(953,132)]{from():* -:mul(981,205)how()from()how()where()+[mul(741,911)[{why()%?who(176,667)what()!mul(194,445)mul(970,761)select()mul(722,592)~*?what()[}*}mul(807,681)<--how()mul#~{,/'}mul(118,771)mul(217from()/;; where()mul(21,190)what(),when()#~[}mul(490,199)*mul(29,437)'&where();[when()where()&mul(437,931)?*/~what()where()@!where()where()mul(454,998)mul!when()what(283,438)from()mul(752,709)/:<*/do()from()what();<}<mul(20,583),$),mul(770,783)(!%-*mul(508,561)>/[?mul(937,711)+#;don't()-when()mul(287,875)who()>*^who()how()how(59,735)when()what()mul(642,627)&:from()how(){:mul(273,186)mul(513,893)<+when()'select()(mul(250,147)mul(944,800)&}/:}%don't()/when(763,491))&what()~mul(369,406)when()) [$<'mul(17,672)<mul(656,755)+{/])select()&mul(109,548)where()) mul(210when(573,420):why()%,$~mul(262,927)?(:{<%who()!^mul(92,518){mul(261,612)]-(mul(724,456)mul(887,115))/why()',?mul(732,96)-[ mul; mul(925,310)when()-:-what(),!*do()-}who()who()#how()>mul(882,623who()[)'[why())#'?mul(81,716)+ mul(447,640)mul(653,686)when()-!)*>']@mul(261where()(($(who()why()^~mul(513,546):?@',who()when()??[mul(133,831)what(642,114)- from()$mul(688,974)&mul(130,103)!+what(){&%)mul(208,734)@do()<(</-[how(),mul(600,588)$} where()@!+$mul(215,125) *,don't()</':select()select()&,select()^mul(335,926)mul(526,944)@+[,!where()(mul(357,677)[from()mul(29,462)what()@what()when()[mul(990,235))>$<>)%,-mul(184,818)mul(890,576)how()mul(837,954)'select()([;*)>,;mul(238,270)^^what()](/mul(850,817)^what()+'why()<](;mul(399,231)+how()when()-mul'when()*<+('->what():mul(57,595)+where()mul(61>{from()who())when()mul(20,581)^'?<+#mul(506,640)select()'[)&{^from()$mul(238,389)select()when(41,502)@what()[-where(),select()who()mul(330,955)~^/when(134,118)&<@%!mul(766,471)mul(292$select()what()^)}{mul(294,301)select()+mul(4,456);]what(270,427)/what(638,143)#mul(567,277)+~what()when()mul(291,92)&what()mul(883,529);}'+><from()@@mul(153,229)from(),,}::)$<when(903,784)do()[(^#mul(810,887)mul(127? ,+ !do()from()#what();select();'mul(573,461[[where()what();'#!/usr/bin/perl}!why()@^+%/where()-mul(151,714)#'{;'><mul(757,774)
don't():>>/;%^)mul(837,12)why()#[>@mul(180,108)<)^)-select()mul(790,366) mul(477,626)who();don't()what():>})>select()#,select()mul(970,250)+!why() (&+mul(702,494))-^when()>mul(365,357)who(823,464)>@from()when()~#mul(193,867[)$mul(59,73)what()/mul(150,669)$}who()why()why()?>+,mul(503,887)~!&&(:#&,}mul(770,232))mul(608,780)}what()>@where()mul(814,784)how();<from()%/where())mul(352,786)$mul(207,994)?&$%%&+from()-mul(109,408))what()~mul(814,457)select()<[mul(19,549)~)mul(917,930)[{^mul-mul(8,721);'+<[-do()from()mul(509,815)]&*who(487,114)?#who()(mul(821,706)/$why()from()'#;^mul(952,474)$mul(595 ($#how():who()$who()mul(191,64)when():why()mul(945,156)+mul(67,396)when()}mul(676,86)mul(983,709)@mul(302,19)*$select()&who()from()!??mul(109,820):select(229,939)@^!when()mul(775,689)mul(123,536)<{where():(:mul(744,643/^~'#where()<where()~mul(877,757)select()select()where()%$mul(964from()$why(){how(888,807)/{;mul(20,417)&from(931,370)mul(916,436)>mul(195,454)/]why()how()who()mul(119,155),^mul(840,203)'+]<};mul(307,495)#(who()what()$select()when()$;+mul(26,644)how()+;select(255,590)mul(531from()select()'mul(714,614),mul(325,872)<<[mul#}!:mul(394,222)@%what(588,571)-,:<&mul(400,422)-]do()what()how()#{&why()%when(466,904)~mul(851,835)~how()where()&*why(499,551)*mul(910,493),!what()>where(303,78),'select()%mul(625,527)mul(479,758)mul(327,98)mul(554,259)select()}mulwhy() !;]@'/mul(113,41)where()$how()*select()do()%'^]@mul(285,496)&select()do()[/!?/[mul(27,435),{;;from()?/{}^mul(918,36)!{? [![:mul(490,857)where()((,mul(428,611)-$,/><#(who()mul(456,409)when(369,358) :~+where()-(what()@mul(733,862)'@$(mul(131,879)from()~-$select()mul(734,484)from()what()what()mul(507,287)where(),why(284,579)>>!]from()who()mul(295,272)-mul(882,901)/]<-$^+#*mul(745]:when():<?from(317,30)how()%mul(789,365)]!]who()mul(893,528)!mul(53,733),'mul(608,702):?{select()don't(),>:$-where()how()<mul(43,408)mul(527,351)&/how()why()(:mul(902,966)$do()]?*~/how()mul(60,36)}+~@where()^mul(95,237)what()select()'who()what()-where()))}mul(954,748)$#from()from(),(*>/mul(898,313)do()<$why()!!</mul(821,96)-why()mul(200,701)/mul(421,170)[when())from()what()^:select(263,923)mul(718,787)$,^ > why(839,58)$,mul(788,713)from()+[what()}~#what()+do()[ $*where()~@!@+mul(603,956)when()mul(601,972)what(56,144)do():why(){mul(103,342))$>who()'mul(60,904){why()/)who()$mul(761,131)!&->when()mul(560,725)from()mul(818,250when())(<(#/-mul(387,817){ ;!-^mul(702,837)';don't() $}]@mul(461,199)$'where()~:from()from(171,996) }don't()/-??$}mul(994,543):#!why(),'>mul(442,181) ;!@%mul(601,317)+?-+$mul(909,275)when(296,190)when()select():->when(453,892)'mul(551,960)mul(507,289)what()when()+mul(671,135)select()+~from()}when(),why()/mul(121,912)from()#who()mul(799,631)mul(784,458),'do()::~+}how())mul(454,304)who()%what()select()what() what()~mul(94,667)mul(360,282)mul(528,823) *;;when()?~)mul(848,397)}[/^({/%mul(261,776)-from(),mul(726,676)$what()+)mul(641,845:+*#mul(729,810)!how())<mul(13,187)how()%#who())what()mul(587,161)where()?where()~when()'mul(280,145)~:!@)&why()mul(436,283)who()]mul(752,757):!>$/mul(352,322)how()from(156,362)(}>:$,])mul(784,187)
mul(989,116)what()? mul(240what()^&;;mul(154,827)^<don't();how()@#from()/?@,mul(445,633)from()how()+mul(546,175)why(),what()?[)what())!~mul(522,402):what()-mul(825,669)/~from())[&(from()mul(157,376)<*#from():,(%mul(957,617))why()&how()where(),,from(),mul(668,157)(},from()((:@'~mul(63,982)+;:<[ do();what()>@what()mul(254,522)select()how()'why()(?do()@}#where()mul(193,567)/mul(775,751)([]!what()>%(mul(788,585)&/,mul(475,307){when(140,109)-why()/mul(349,674)how()from()<$%when():where()#mul(227,383)when()%#-mul(711,505)what()why()#^<why()mul(43,231)/&:why()+/-/how()^don't()why()<>'}}why(){what()mul(717,303)how()>mul(760,51)%&-~)>!mul(674,136)mul(140,636),)~&-mul(908,30)mul(154,688):from()-when()<^#!?mul(417,70)%<from()?(>>do()~^--#'<@>>mul(354,115)~when()'what():from(354,427)>mul(279,257)mul(292,504):[{[who()('what()^mul(893,699)^+&)[//mul(753,807)><+])}~mul(957,644)$]]+how()~$mul(53,811)mul(447,226)select()mul(774,984)why()why()'![don't()+$where()>$&from(16,716)}?mul(252,848)]#&from(){mul(895,641)///when()#)mul(482,275)how()) )select()from(){mul(645,131)*));*+mul(266,281)%[mul(446,962):)< who();/mul(876,107)%*>mul(187,697)-how();select()$mul(962,372)mul(276,649)}what()!#select(),from()? who()mul(540,977)why()#}</&-[-from()mul(753,655)what(589,427)<{where()what()#mul(623,760){select() where()who()?$mul(26,86)(+[$,select()%<mul(877,970),do()&^<^from()#>why()-mul(527,726)!/select()%?mul(602,536)/?what()why() select()%/mul(926,882)why()who()-}*/mul(960,515) >~!?^!how()select()mul(597,249)what()/<@$~$what(708,877)/mul(871,408)mul(178,932)/why():why(541,591):why():$+%mul(107,703)[@*from();who()don't()? [>*mul(345,156)< ?];^)>from()select(549,167)mul(764,609)&where(32,545)#mul(35,321)<&when(241,647)/mul(414,62)[![how(243,208)-)mul(399,237)#'#+when(820,119)where()($mul(418,23)what()mul(618,231)$(mul(864,185)'#!^mul(730,572)#];what()$>};why(866,942)mul(196,426)($@where()where(),mul(51,66)from()[)#<;where()<:mul(504,489),@*when()why()mul(979,151),]@*^(^where(48,22)why()mul(910,862)mul(58,405)#>' ~from()when()mul(817,943)*(who()[<how()*what(251,66)mul(277,652))mul(669,16)%:from()why():[/[{*mul(727,589)-#:!mul(34,541)+mul(906,174)*+^[mul(112,617) what()+'how()do()#~+[what()when()^~/mul(203,659)when()[}select()'#mul(869,605)where()->[*%;+who():mul(140,620)~[mul(93,354),do()how()who(){how()'$-what()why()mul(542,872)<;mul(490,224)+*):}when() ]mul(840*]what()}('&mul(563,138)when()mul(298,803)+!'&#+~+mul(914,40)~<select(347,181)#mul&?~<mul(848,266)/~'#!?mul(436,708)>+how()mul(766,174){/#what()who()when()~)%don't()&where(){mul(587,419)'!don't()&where()'/+mul(629,54)$!!where()/what()*~mul(523,43)?where(686,184)#>%how()-; +mul(165<who()<where()[from()when()#select():from()mul(297,190)[&^-~/how()mul(441,676)what()mul(47,942)^+/$who()<]mul(4,166)/mul(257,565)&~$how()^#%&mul(446,402)where();:mul(900,590)-)>from()mul(137,486)(;}when()!?from()where()where()^mul(694,53)what()mul(631,877)(how()?/'@)-from()mul(711,927)+what()when()&how()mul(66,129)from())],}/$what()#mul(347>mul(691,91)mul(791,897)when()~ }-)mul(325,178)mul(105,565)^<*mul(193);;]how()mul(355,707)#{#%mul(27,653)
%how()how():?:mul(766,746)*@mul(364,566)-< who()(*':mul(999,344)*/select()--mul(672,593){how())<mul(73#)()@+mul(83,507)  mul(373,176)who()^'('mul(584,620)what()#//!do()mul(103,225) ~;;'why()*~mul(187,119)+/]mul+(select()%mul(874,888)}when():how()mul(583,992)^~[[what()don't()() {from() ]]((mul(68,200)^?what()who()*[mul(932,283)['$mul(189,932)< ,mul(652,125))$how()where()how()^how()#]mul(501,335)!when():+%])!<mul(551,924)+,#why()) $mul(118,951)@])/'who()mul(858,212) who(){-how()!don't())mul(746,402)/%}where()mul(629,312)];*~#]mul(680,3)what()how()what()'}?'@where();mul(263,427)#$$from()-what()mul(698,847)#(;$!$<+mul;why())what())$select()mul(482,169)-where()mul(546,79)mul(796,632)how()select()when()&$/*mul(749,226)-%what()>(who()'/<when()mul(932,346)?where(),^^>mul(722,627)>-?mul(231,501)~}#!mul(694,751)when()how()- where(202,572)select() }*^mul(17,75):+'what(),&mul(413,505)mul(113,65)[-+{,[mul(83,722)((mul(475,980)mul(588,832):/;)what()/+mul(103,764)?{$:?{{+:select()mul(583,487)mul(757,133)why()??mul(47,54)<]>select()>^?$mul(201,196)$from()]^~#where()mul(494,817)]?//-#select()%+mul(444,319)%?from()mul(316,303)}-~'<<-when()when()mul(350,810)mul(557,674)~##(select()$mul(97,781)who()(>>' >!),mul(473,488)who(290,952)mul(33,630)why()>do():)<select()~ mul(571,144){mul(931,78)mul(200,845)how()#select(403,528)mul(741,613)mul(54,465)@;(<[>mul(267,367)+/who())^select()^from()$!mul(409,900)*what()[)]who())[+where()mul(309,751)~don't()!mul(165,206)mul(113,418)]from(),'&do()select()/*:)]!mul(272,138) mul(211,851)]/$mul(916,846)mul(203,199)mul(40,428){&*from()%mul(305,353)? >}where()<what()(mul(904,794)+$from()-,/{mul(712,685)@ what(628,776)why(){;:;-mul(909,11){<,,mul(287,272),?),>%mul(397,337)]!mul(352,23)@don't()where()^{from()mul(804,392)${<}!mul(392,298),>>mul(572,89)+why()$*;when())#where()$mul(458,495);mul(375,386)~from()mul(429,704),{*%select()$who()]mul(442,21)#why()@?!mul(659,81)when()<($%^&&don't()!mul(934,729)/<[:how(288,214)'mul(971,226);+!%!mul(465,736)/]&%&^what(),+mul(613,544)-/from()what() },<-!mul(906,152)[who()&when()select()mul(612,56)~&<')/!mul(247,423)from()[{&who()mul(979,442)[mul(319,494)~%/+mul(781,251);<>)who()%from()[from()mul(27,381)}+)what()%/select(),,mul(324,64)mul(938,422)how():@>}:%'/&mul(388,707)]@mul(98,712)~who()$%@?(what()from()who()mul(161,906)~where():#mul(198,30)why() ~!>how()['-who()mul(5,68)what()<%%{mul(829,126):,mul(509,883)mul(142,939)do()#>mul(53,112)!(what()/?do()(,how()%mul(523,469) who(){what()'/mul(356,713)~@;!~ ->mul(309,932)where()mul(93,190)where()select()){how()}why()mul(202,888))!,{{:what(),~mul(591,813)select()<&{[&mul(652,199)
|input}

(* let () = print_string input *)

let found =
  Str.full_split pat input
  |> List.filter_map (fun x -> match x with Str.Delim x -> Some x | _ -> None)

let parse_op op =
  let n = String.length op in
  let args = String.sub op 4 (n - 4 - 1) in
  args |> String.split_on_char ',' |> List.map int_of_string
  |> List.fold_left ( * ) 1

let answer = found |> List.map parse_op |> List.fold_left ( + ) 0
let () = printf "Part1 %d\n" answer

let pat = Str.regexp {|\(mul([0-9]+,[0-9]+)\)\|\(do()\)\|\(don't()\)|}

type op = Do | Dont | Op of int
let map_op s =
  match s with "do()" -> Do | "don\'t()" -> Dont | x -> Op (parse_op x)

let found =
  Str.full_split pat input
  |> List.filter_map (fun x ->
         match x with Str.Delim x -> Some (map_op x) | _ -> None)

(* let () = List.iter print_string found *)

let update (ans, ignore) o =
  match o with
  | Do -> (ans, false)
  | Dont -> (ans, true)
  | Op x -> if ignore then (ans, ignore) else (ans + x, ignore)

let answer, _ = found |> List.fold_left update (0, false)
let () = printf "Part2: %d\n" answer
