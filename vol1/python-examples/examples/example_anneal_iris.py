#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 1: Fundamental Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2013 by Jeff Heaton

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

    For more information on Heaton Research copyrights, licenses
    and trademarks visit:
    http://www.heatonresearch.com/copyright
    ============================================================================================================
    This example takes awhile to execute.  It uses simulated annealing to fit an RBF network to the iris data set.
    You can see the output from the example here.  As you can see, it took xxx iterations to train to xxxx.
    You can see that it is able to classify many of the iris species correctly, but not all.

    This example uses one-of-n encoding for the iris species.  Equilateral could have also been used.

    This example makes use of a Python implemented RBF network, as well as a Python implemented Simulated Annealing
    algorithm.  Better performance could be had by implementing the Simulated Annealing portion in a more
    performant language than Python.  I also tried using the Scipy minimize function's implementation of Simulated
    Annealing, but failed to get good results.  If anyone can make this example work with Scipy annealing, please
    submit the example.

/Users/jheaton/anaconda/bin/python /Users/jheaton/projects/aifh/vol1/python-examples/examples/example_anneal_iris.py
Reading CSV file: /Users/jheaton/projects/aifh/vol1/python-examples/datasets/iris.csv
Iteration #1, Score: 0.275434707866,k=1,kMax=100,t=343.589110735,prob=0.999969370348,1.49938091471
Iteration #2, Score: 0.275434707866,k=2,kMax=100,t=295.133692539,prob=0.999225533927,2.28898330261
Iteration #3, Score: 0.275434707866,k=3,kMax=100,t=253.511807419,prob=0.998776541801,5.07604237841
Iteration #4, Score: 0.275434707866,k=4,kMax=100,t=217.75974118,prob=0.999439369618,6.97984955373
Iteration #5, Score: 0.275434707866,k=5,kMax=100,t=187.049689565,prob=0.998316554711,5.84700678088
Iteration #6, Score: 0.275434707866,k=6,kMax=100,t=160.670591252,prob=0.999568795309,2.57925280513
Iteration #7, Score: 0.275434707866,k=7,kMax=100,t=138.011663924,prob=0.9993971523,0.958742859778
Iteration #8, Score: 0.275434707866,k=8,kMax=100,t=118.548262197,prob=0.989666038515,11.8581247444
Iteration #9, Score: 0.275434707866,k=9,kMax=100,t=101.829729968,prob=0.991216161799,21.4597831304
Iteration #10, Score: 0.275434707866,k=10,kMax=100,t=87.4689659155,prob=0.98251251483,41.4708913991
Iteration #11, Score: 0.275434707866,k=11,kMax=100,t=75.1334605395,prob=0.983522657028,47.4170621726
Iteration #12, Score: 0.275434707866,k=12,kMax=100,t=64.5375972331,prob=0.98954014744,32.2700185673
Iteration #13, Score: 0.275434707866,k=13,kMax=100,t=55.4360391057,prob=0.986526207833,22.5931062562
Iteration #14, Score: 0.275434707866,k=14,kMax=100,t=47.6180484475,prob=0.977227708105,7.79006665813
Iteration #15, Score: 0.275434707866,k=15,kMax=100,t=40.9026073025,prob=0.999148159047,9.30974481467
Iteration #16, Score: 0.275434707866,k=16,kMax=100,t=35.1342261746,prob=0.964445349792,12.6799608595
Iteration #17, Score: 0.275434707866,k=17,kMax=100,t=30.1793438192,prob=0.997262042643,14.7648991703
Iteration #18, Score: 0.275434707866,k=18,kMax=100,t=25.9232347635,prob=0.968045023396,16.4149476608
Iteration #19, Score: 0.275434707866,k=19,kMax=100,t=22.2673529494,prob=0.982796488136,27.814728339
Iteration #20, Score: 0.275434707866,k=20,kMax=100,t=19.1270499958,prob=0.998350077135,27.004903493
Iteration #21, Score: 0.275434707866,k=21,kMax=100,t=16.4296152476,prob=0.993827633087,24.6395000753
Iteration #22, Score: 0.275434707866,k=22,kMax=100,t=14.1125922316,prob=0.99976752677,19.7060876522
Iteration #23, Score: 0.275434707866,k=23,kMax=100,t=12.1223325376,prob=0.969481516348,14.5363290526
Iteration #24, Score: 0.275434707866,k=24,kMax=100,t=10.4127536415,prob=0.971521322786,17.2285322341
Iteration #25, Score: 0.275434707866,k=25,kMax=100,t=8.94427191,prob=0.97766700048,6.88777586533
Iteration #26, Score: 0.275434707866,k=26,kMax=100,t=7.68288607932,prob=0.988392458058,5.38833603003
Iteration #27, Score: 0.275434707866,k=27,kMax=100,t=6.59938998968,prob=0.969121012418,0.899226232135
Iteration #28, Score: 0.275434707866,k=28,kMax=100,t=5.66869634487,prob=0.970329927659,1.38368778928
Iteration #29, Score: 0.275434707866,k=29,kMax=100,t=4.8692558404,prob=0.91845341413,5.92795195253
Iteration #30, Score: 0.275434707866,k=30,kMax=100,t=4.18255821037,prob=0.93686789023,3.05329028156
Iteration #31, Score: 0.275434707866,k=31,kMax=100,t=3.59270364024,prob=0.983820635511,1.49950609527
Iteration #32, Score: 0.275434707866,k=32,kMax=100,t=3.08603462221,prob=0.931288486515,6.21016338343
Iteration #33, Score: 0.275434707866,k=33,kMax=100,t=2.65081972886,prob=0.969422167751,5.63253987889
Iteration #34, Score: 0.275434707866,k=34,kMax=100,t=2.27698198339,prob=0.995444046827,7.6778095813
Iteration #35, Score: 0.275434707866,k=35,kMax=100,t=1.95586553709,prob=0.986367910198,6.97133569061
Iteration #36, Score: 0.275434707866,k=36,kMax=100,t=1.68003525151,prob=0.935022224301,4.63230219827
Iteration #37, Score: 0.275434707866,k=37,kMax=100,t=1.44310454518,prob=0.843862670076,2.99059242089
Iteration #38, Score: 0.275434707866,k=38,kMax=100,t=1.23958751844,prob=0.802584066244,4.35181252947
Iteration #39, Score: 0.275434707866,k=39,kMax=100,t=1.06477193285,prob=0.843352773805,1.66429328862
Iteration #40, Score: 0.275434707866,k=40,kMax=100,t=0.914610103855,prob=0.823458255576,1.64750338978
Iteration #41, Score: 0.275434707866,k=41,kMax=100,t=0.785625180632,prob=0.895692374188,1.58068933566
Iteration #42, Score: 0.275434707866,k=42,kMax=100,t=0.674830642961,prob=0.971647899151,0.642834707146
Iteration #43, Score: 0.275434707866,k=43,kMax=100,t=0.579661151279,prob=0.777487018523,0.800316895725
Iteration #44, Score: 0.275434707866,k=44,kMax=100,t=0.497913148739,prob=0.91237739137,0.973380142253
Iteration #45, Score: 0.275434707866,k=45,kMax=100,t=0.427693839996,prob=0.973141950717,0.456992857666
Iteration #46, Score: 0.275434707866,k=46,kMax=100,t=0.367377365378,prob=0.888096259679,0.478876689468
Iteration #47, Score: 0.275434707866,k=47,kMax=100,t=0.315567155686,prob=0.908384497796,0.375955186955
Iteration #48, Score: 0.248618454091,k=48,kMax=100,t=0.271063595998,prob=0.940493733533,0.639017480658
Iteration #49, Score: 0.248618454091,k=49,kMax=100,t=0.232836249754,prob=0.669495235194,0.458556266324
Iteration #50, Score: 0.248618454091,k=50,kMax=100,t=0.2,prob=0.892578713301,0.448694582363
Iteration #51, Score: 0.248618454091,k=51,kMax=100,t=0.171794555368,prob=0.797460946208,0.284176180674
Iteration #52, Score: 0.248618454091,k=52,kMax=100,t=0.14756684627,prob=0.63887700983,0.512837377492
Iteration #53, Score: 0.24009316994,k=53,kMax=100,t=0.126755903709,prob=0.426608565831,0.428915723561
Iteration #54, Score: 0.237081330794,k=54,kMax=100,t=0.10887987059,prob=0.8653981204,0.391056698598
Iteration #55, Score: 0.237081330794,k=55,kMax=100,t=0.0935248447823,prob=0.510947084303,0.447601985324
Iteration #56, Score: 0.237081330794,k=56,kMax=100,t=0.0803352956259,prob=0.87821295539,0.440110980031
Iteration #57, Score: 0.237081330794,k=57,kMax=100,t=0.0690058319619,prob=0.371905017644,0.330333041503
Iteration #58, Score: 0.223581317017,k=58,kMax=100,t=0.0592741310983,prob=0.943668038739,0.233304438757
Iteration #59, Score: 0.204600028571,k=59,kMax=100,t=0.0509148649841,prob=0.433579019829,0.323226059904
Iteration #60, Score: 0.204600028571,k=60,kMax=100,t=0.0437344829577,prob=0.720573384342,0.237421946906
Iteration #61, Score: 0.188145794332,k=61,kMax=100,t=0.0375667302698,prob=0.550890655017,0.253897716129
Iteration #62, Score: 0.187116376219,k=62,kMax=100,t=0.0322687986165,prob=0.178706791284,0.266613991728
Iteration #63, Score: 0.187116376219,k=63,kMax=100,t=0.0277180195529,prob=0.269001664582,0.300995081578
Iteration #64, Score: 0.187116376219,k=64,kMax=100,t=0.0238090242238,prob=0.55676073331,0.255857979641
Iteration #65, Score: 0.187116376219,k=65,kMax=100,t=0.0204513036513,prob=0.888020467396,0.25975178764
Iteration #66, Score: 0.187116376219,k=66,kMax=100,t=0.0175671130873,prob=0.162159049931,0.27651156123
Iteration #67, Score: 0.187116376219,k=67,kMax=100,t=0.0150896719096,prob=0.823379973251,0.226485092547
Iteration #68, Score: 0.187116376219,k=68,kMax=100,t=0.0129616173818,prob=0.0440327490353,0.253419683861
Iteration #69, Score: 0.187116376219,k=69,kMax=100,t=0.0111336764747,prob=0.52708157365,0.240499164304
Iteration #70, Score: 0.187116376219,k=70,kMax=100,t=0.0095635249979,prob=0.00241270923867,0.274582466329
Iteration #71, Score: 0.187116376219,k=71,kMax=100,t=0.0082148076238,prob=0.0234753538196,0.224499683402
Iteration #72, Score: 0.187116376219,k=72,kMax=100,t=0.0070562961158,prob=0.00166295733497,0.23229615334
Iteration #73, Score: 0.187116376219,k=73,kMax=100,t=0.00606116626878,prob=0.773654839553,0.208162306009
Iteration #74, Score: 0.187116376219,k=74,kMax=100,t=0.00520637682077,prob=0.726337827674,0.193661501156
Iteration #75, Score: 0.180837653817,k=75,kMax=100,t=0.004472135955,prob=0.0947425254154,0.208131027358
Iteration #76, Score: 0.177660646297,k=76,kMax=100,t=0.00384144303966,prob=2.39132671211e-05,0.180025819377
Iteration #77, Score: 0.157953915414,k=77,kMax=100,t=0.00329969499484,prob=4.60898936259e-08,0.167191344253
Iteration #78, Score: 0.153870675689,k=78,kMax=100,t=0.00283434817244,prob=0.000641455570637,0.15481764639
Iteration #79, Score: 0.136643644488,k=79,kMax=100,t=0.0024346279202,prob=4.62761270422e-05,0.136643644488
Iteration #80, Score: 0.132747534414,k=80,kMax=100,t=0.00209127910518,prob=0.0408450830552,0.142163154935
Iteration #81, Score: 0.126375927741,k=81,kMax=100,t=0.00179635182012,prob=0.07802751569,0.130296437395
Iteration #82, Score: 0.126375927741,k=82,kMax=100,t=0.00154301731111,prob=2.82948335295e-05,0.130492019045
Iteration #83, Score: 0.118388319521,k=83,kMax=100,t=0.00132540986443,prob=0.244595298048,0.118388319521
Iteration #84, Score: 0.113563827319,k=84,kMax=100,t=0.0011384909917,prob=1.36233381001e-30,0.117014129238
Iteration #85, Score: 0.102608826408,k=85,kMax=100,t=0.000977932768543,prob=3.29935386792e-07,0.103100004982
Iteration #86, Score: 0.102608826408,k=86,kMax=100,t=0.000840017625756,prob=2.60155460124e-12,0.104476721308
Iteration #87, Score: 0.100510215554,k=87,kMax=100,t=0.000721552272588,prob=2.69161643763e-08,0.100510215554
Iteration #88, Score: 0.100441369562,k=88,kMax=100,t=0.000619793759218,prob=1.39310806539e-08,0.100441369562
Iteration #89, Score: 0.096373706486,k=89,kMax=100,t=0.000532385966423,prob=0.359844394465,0.0970970214431
Iteration #90, Score: 0.0954430308546,k=90,kMax=100,t=0.000457305051927,prob=3.48869577978e-18,0.0954430308546
Iteration #91, Score: 0.0916450283032,k=91,kMax=100,t=0.000392812590316,prob=3.30957939345e-26,0.0916450283032
Iteration #92, Score: 0.0903930432397,k=92,kMax=100,t=0.00033741532148,prob=4.36359615481e-36,0.0903930432397
Iteration #93, Score: 0.0894323328306,k=93,kMax=100,t=0.00028983057564,prob=0.000707357509252,0.0894323328306
Iteration #94, Score: 0.0888391174467,k=94,kMax=100,t=0.00024895657437,prob=1.94884472412e-25,0.0888391174467
Iteration #95, Score: 0.0868532435744,k=95,kMax=100,t=0.000213846919998,prob=9.48326997154e-30,0.0868532435744
Iteration #96, Score: 0.0868532435744,k=96,kMax=100,t=0.000183688682689,prob=1.08639570753e-82,0.0868532435744
Iteration #97, Score: 0.0867827593661,k=97,kMax=100,t=0.000157783577843,prob=7.84481444736e-36,0.0867827593661
Iteration #98, Score: 0.0854931642171,k=98,kMax=100,t=0.000135531797999,prob=1.88407333451e-69,0.0854931642171
Iteration #99, Score: 0.0838511142207,k=99,kMax=100,t=0.000116418124877,prob=1.51942092592e-49,0.0838511142207
Iteration #100, Score: 0.0819990931608,k=100,kMax=100,t=0.0001,prob=6.60285189977e-65,0.0819990931608
Finished after 101 iterations, final score is 0.0819990931608
[ 0.22222222  0.625       0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.16666667  0.41666667  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.11111111  0.5         0.05084746  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.08333333  0.45833333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.66666667  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.79166667  0.11864407  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.08333333  0.58333333  0.06779661  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.58333333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.02777778  0.375       0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.16666667  0.45833333  0.08474576  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.70833333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.58333333  0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.41666667  0.06779661  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.          0.41666667  0.01694915  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.41666667  0.83333333  0.03389831  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.38888889  1.          0.08474576  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.79166667  0.05084746  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.625       0.06779661  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.38888889  0.75        0.11864407  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.75        0.08474576  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.58333333  0.11864407  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.70833333  0.08474576  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.08333333  0.66666667  0.          0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.54166667  0.11864407  0.16666667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.58333333  0.15254237  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.41666667  0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.58333333  0.10169492  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.25        0.625       0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.25        0.58333333  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.11111111  0.5         0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.45833333  0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.58333333  0.08474576  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.25        0.875       0.08474576  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.33333333  0.91666667  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.16666667  0.45833333  0.08474576  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.5         0.03389831  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.33333333  0.625       0.05084746  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.16666667  0.45833333  0.08474576  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.02777778  0.41666667  0.05084746  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.58333333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.625       0.05084746  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.05555556  0.125       0.05084746  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.02777778  0.5         0.05084746  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.625       0.10169492  0.20833333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.75        0.15254237  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.41666667  0.06779661  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.75        0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.08333333  0.5         0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.27777778  0.70833333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.54166667  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.75        0.5         0.62711864  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.58333333  0.5         0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.72222222  0.45833333  0.66101695  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.125       0.50847458  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.61111111  0.33333333  0.61016949  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.33333333  0.59322034  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.55555556  0.54166667  0.62711864  0.625     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.16666667  0.16666667  0.38983051  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.63888889  0.375       0.61016949  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.25        0.29166667  0.49152542  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.19444444  0.          0.42372881  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.44444444  0.41666667  0.54237288  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.47222222  0.08333333  0.50847458  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.5         0.375       0.62711864  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.375       0.44067797  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.66666667  0.45833333  0.57627119  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.41666667  0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.41666667  0.29166667  0.52542373  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.52777778  0.08333333  0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.20833333  0.49152542  0.41666667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.44444444  0.5         0.6440678   0.70833333] -> Iris-virginica, Ideal: Iris-versicolor
[ 0.5         0.33333333  0.50847458  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.55555556  0.20833333  0.66101695  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.5         0.33333333  0.62711864  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.58333333  0.375       0.55932203  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.63888889  0.41666667  0.57627119  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.69444444  0.33333333  0.6440678   0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.66666667  0.41666667  0.6779661   0.66666667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.47222222  0.375       0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.25        0.42372881  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.16666667  0.47457627  0.41666667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.16666667  0.45762712  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.41666667  0.29166667  0.49152542  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.47222222  0.29166667  0.69491525  0.625     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.30555556  0.41666667  0.59322034  0.58333333] -> Iris-virginica, Ideal: Iris-versicolor
[ 0.47222222  0.58333333  0.59322034  0.625     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.66666667  0.45833333  0.62711864  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.55555556  0.125       0.57627119  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.41666667  0.52542373  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.20833333  0.50847458  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.25        0.57627119  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.5         0.41666667  0.61016949  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.41666667  0.25        0.50847458  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.19444444  0.125       0.38983051  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.29166667  0.54237288  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.41666667  0.54237288  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.375       0.54237288  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.52777778  0.375       0.55932203  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.22222222  0.20833333  0.33898305  0.41666667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.33333333  0.52542373  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.55555556  0.54166667  0.84745763  1.        ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.41666667  0.29166667  0.69491525  0.75      ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.77777778  0.41666667  0.83050847  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.375       0.77966102  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.61111111  0.41666667  0.81355932  0.875     ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.91666667  0.41666667  0.94915254  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.16666667  0.20833333  0.59322034  0.66666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.83333333  0.375       0.89830508  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.66666667  0.20833333  0.81355932  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.80555556  0.66666667  0.86440678  1.        ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.61111111  0.5         0.69491525  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.58333333  0.29166667  0.72881356  0.75      ] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.69444444  0.41666667  0.76271186  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.38888889  0.20833333  0.6779661   0.79166667] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.41666667  0.33333333  0.69491525  0.95833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.58333333  0.5         0.72881356  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.61111111  0.41666667  0.76271186  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.94444444  0.75        0.96610169  0.875     ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.94444444  0.25        1.          0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.47222222  0.08333333  0.6779661   0.58333333] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.72222222  0.5         0.79661017  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.36111111  0.33333333  0.66101695  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.94444444  0.33333333  0.96610169  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.29166667  0.66101695  0.70833333] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.66666667  0.54166667  0.79661017  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.80555556  0.5         0.84745763  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.52777778  0.33333333  0.6440678   0.70833333] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.5         0.41666667  0.66101695  0.70833333] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.58333333  0.33333333  0.77966102  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.80555556  0.41666667  0.81355932  0.625     ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.86111111  0.33333333  0.86440678  0.75      ] -> Iris-virginica, Ideal: Iris-virginica
[ 1.          0.75        0.91525424  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.58333333  0.33333333  0.77966102  0.875     ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.33333333  0.69491525  0.58333333] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.5         0.25        0.77966102  0.54166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.94444444  0.41666667  0.86440678  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.58333333  0.77966102  0.95833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.58333333  0.45833333  0.76271186  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.47222222  0.41666667  0.6440678   0.70833333] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.72222222  0.45833333  0.74576271  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.66666667  0.45833333  0.77966102  0.95833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.72222222  0.45833333  0.69491525  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.41666667  0.29166667  0.69491525  0.75      ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.69444444  0.5         0.83050847  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.66666667  0.54166667  0.79661017  1.        ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.66666667  0.41666667  0.71186441  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.20833333  0.6779661   0.75      ] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.61111111  0.41666667  0.71186441  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.52777778  0.58333333  0.74576271  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.44444444  0.41666667  0.69491525  0.70833333] -> Iris-virginica, Ideal: Iris-virginica

Process finished with exit code 0

"""
__author__ = 'jheaton'

import os
import sys
import numpy as np

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from normalize import Normalize
from rbf_network import RbfNetwork
from error import ErrorCalculation
from train import TrainAnneal

# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")

# Read the Iris data set.
print('Reading CSV file: ' + irisFile)
norm = Normalize()
iris_work = norm.load_csv(irisFile)

# Extract the original iris species so we can display during the final validation.
ideal_species = [row[4] for row in iris_work]

# Setup the first four fields to "range normalize" between -1 and 1.
for i in range(0, 4):
    norm.make_col_numeric(iris_work, i)
    norm.norm_col_range(iris_work, i, 0, 1)

# Discover all of the classes for column #4, the iris species.
classes = norm.build_class_map(iris_work, 4)
inv_classes = {v: k for k, v in classes.items()}

# Normalize iris species using one-of-n.
# We could have used equilateral as well.  For an example of equilateral, see the example_nm_iris example.
norm.norm_col_one_of_n(iris_work, 4, classes, 0, 1)


# Prepare training data.  Separate into input and ideal.
training = np.array(iris_work)
training_input = training[:, 0:4]
training_ideal = training[:, 4:7]

# Create an RBF network.  There are four inputs and two outputs.
# There are also five RBF functions used internally.
# You can experiment with different numbers of internal RBF functions.
# However, the input and output must match the data set.
network = RbfNetwork(4, 4, 3)
network.reset()

def score_funct(x):
    """
    The score function for Iris anneal.
    @param x:
    @return:
    """
    global best_score
    global input_data
    global output_data
    # Update the network's long term memory to the vector we need to score.
    network.copy_memory(x)
    # Loop over the training set and calculate the output for each.
    actual_output = []
    for input_data in training_input:
        output_data = network.compute_regression(input_data)
        actual_output.append(output_data)
    # Calculate the error with MSE.
    result = ErrorCalculation.mse(np.array(actual_output), training_ideal)
    return result

# Create a copy of the long-term memory.  This becomes the initial state.
x0 = list(network.long_term_memory)

# Perform the annealing
train = TrainAnneal()
train.display_iteration = True
train.train(x0, score_funct)

# Display the final validation.  We show all of the iris data as well as the predicted species.
for i in range(0, len(training_input)):
    input_data = training_input[i]
    # Compute the output from the RBF network
    output_data = network.compute_regression(input_data)
    ideal_data = training_ideal[i]
    # Decode the three output neurons into a class number.
    class_id = norm.denorm_one_of_n(output_data)
    print(str(input_data) + " -> " + inv_classes[class_id] + ", Ideal: " + ideal_species[i])