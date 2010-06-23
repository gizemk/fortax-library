! .f90 FORTAX system; generated using sys_saveF90
 
call sys_init(sys) !deallocates arrays and sets values to default
 
!inctax
sys%inctax%numbands=3
sys%inctax%pa=88.7500000000000_dp
sys%inctax%mma=0.000000000000000E+000_dp
sys%inctax%ctc=0.000000000000000E+000_dp
sys%inctax%ctcyng=0.000000000000000E+000_dp
sys%inctax%mmarate=0.100000000000000_dp
sys%inctax%ctctaper=0.000000000000000E+000_dp
sys%inctax%c4rebate=0.000000000000000E+000_dp
allocate(sys%inctax%bands(3))
sys%inctax%bands(1)=37.6923076923077_dp
sys%inctax%bands(2)=586.538461538462_dp
sys%inctax%bands(3)=19230.7692307692_dp
allocate(sys%inctax%rates(3))
sys%inctax%rates(1)=0.100000000000000_dp
sys%inctax%rates(2)=0.220000000000000_dp
sys%inctax%rates(3)=0.400000000000000_dp
 
!natins
sys%natins%numrates=3
sys%natins%c4nrates=3
sys%natins%c2floor=78.7500000000000_dp
sys%natins%c2rate=2.00000000000000_dp
sys%natins%ceiling=9999999.00000000_dp
allocate(sys%natins%rates(3))
sys%natins%rates(1)=0.000000000000000E+000_dp
sys%natins%rates(2)=0.110000000000000_dp
sys%natins%rates(3)=1.000000000000000E-002_dp
allocate(sys%natins%bands(3))
sys%natins%bands(1)=89.0000000000000_dp
sys%natins%bands(2)=595.000000000000_dp
sys%natins%bands(3)=9999999.00000000_dp
allocate(sys%natins%c4rates(3))
sys%natins%c4rates(1)=0.000000000000000E+000_dp
sys%natins%c4rates(2)=8.000000000000000E-002_dp
sys%natins%c4rates(3)=1.000000000000000E-002_dp
allocate(sys%natins%c4bands(3))
sys%natins%c4bands(1)=88.7500000000000_dp
sys%natins%c4bands(2)=595.000000000000_dp
sys%natins%c4bands(3)=1000000.00000000_dp
 
!chben
sys%chben%basic=10.7500000000000_dp
sys%chben%kid1xtr=5.30000000000000_dp
sys%chben%opf=0.000000000000000E+000_dp
sys%chben%MatGrantVal=500.000000000000_dp
 
!fc
sys%fc%dofamcred=.false.
sys%fc%NumAgeRng=4
sys%fc%MaxAgeCC=0
sys%fc%WFTCMaxAgeCC=0
sys%fc%adult=0.000000000000000E+000_dp
sys%fc%ftprem=0.000000000000000E+000_dp
sys%fc%hours1=16.0000000000000_dp
sys%fc%hours2=30.0000000000000_dp
sys%fc%thres=0.000000000000000E+000_dp
sys%fc%taper=0.000000000000000E+000_dp
sys%fc%MaintDisreg=99999.0000000000_dp
sys%fc%MaxCC1=0.000000000000000E+000_dp
sys%fc%MaxCC2=0.000000000000000E+000_dp
sys%fc%WFTCMaxCC1=0.000000000000000E+000_dp
sys%fc%WFTCMaxCC2=0.000000000000000E+000_dp
sys%fc%WFTCPropCC=0.000000000000000E+000_dp
sys%fc%MinAmt=0.000000000000000E+000_dp
allocate(sys%fc%kidagel(4))
sys%fc%kidagel(1)=0
sys%fc%kidagel(2)=11
sys%fc%kidagel(3)=16
sys%fc%kidagel(4)=18
allocate(sys%fc%kidageu(4))
sys%fc%kidageu(1)=10
sys%fc%kidageu(2)=15
sys%fc%kidageu(3)=17
sys%fc%kidageu(4)=18
allocate(sys%fc%kidcred(4))
sys%fc%kidcred(1)=0.000000000000000E+000_dp
sys%fc%kidcred(2)=0.000000000000000E+000_dp
sys%fc%kidcred(3)=0.000000000000000E+000_dp
sys%fc%kidcred(4)=0.000000000000000E+000_dp
 
!ctc
sys%ctc%fam=10.4807692307692_dp
sys%ctc%baby=10.4807692307692_dp
sys%ctc%kid=27.7884615384615_dp
 
!wtc
sys%wtc%Basic=29.3269230769231_dp
sys%wtc%CouLP=58.1730769230769_dp
sys%wtc%FT=11.9230769230769_dp
sys%wtc%MinHrsKids=16.0000000000000_dp
sys%wtc%MinHrsNoKids=30.0000000000000_dp
sys%wtc%FTHrs=30.0000000000000_dp
sys%wtc%MinAgeKids=16
sys%wtc%MinAgeNoKids=25
sys%wtc%MaxCC1=135.000000000000_dp
sys%wtc%MaxCC2=200.000000000000_dp
sys%wtc%PropCC=0.700000000000000_dp
sys%wtc%MaxAgeCC=15
sys%wtc%NewDisreg=0.000000000000000E+000_dp
sys%wtc%NewDisregCon=.false.
 
!ntc
sys%ntc%donewtaxcred=.true. 
sys%ntc%thr1lo=97.3076923076923_dp
sys%ntc%thr1hi=254.423076923077_dp
sys%ntc%thr2=961.538461538462_dp
sys%ntc%taper1=0.370000000000000_dp
sys%ntc%taper2=6.666660000000001E-002_dp
sys%ntc%MinAmt=0.500000000000000_dp
 
!incsup
sys%incsup%IncChben=.false.
sys%incsup%NumAgeRng=1
sys%incsup%MainCou=85.7500000000000_dp
sys%incsup%YngCou=65.3000000000000_dp
sys%incsup%MainLP=54.6500000000000_dp
sys%incsup%YngLP=32.9000000000000_dp
sys%incsup%MainSin=54.6500000000000_dp
sys%incsup%YngSin=43.2500000000000_dp
sys%incsup%ValFSM=4.28000000000000_dp
sys%incsup%DisregLP=20.0000000000000_dp
sys%incsup%DisregSin=5.00000000000000_dp
sys%incsup%DisregCou=10.0000000000000_dp
sys%incsup%DisregShared=.true. 
sys%incsup%PremFam=0.000000000000000E+000_dp
sys%incsup%PremLP=0.000000000000000E+000_dp
sys%incsup%hours=16.0000000000000_dp
sys%incsup%MaintDisreg=10.0000000000000_dp
allocate(sys%incsup%AgeRngl(1))
sys%incsup%AgeRngl(1)=0
allocate(sys%incsup%AgeRngu(1))
sys%incsup%AgeRngu(1)=18
allocate(sys%incsup%AddKid(1))
sys%incsup%AddKid(1)=0.000000000000000E+000_dp
 
!ctax
sys%ctax%docounciltax=.true. 
sys%ctax%bandD=20.9001923076923_dp
sys%ctax%SinDis=0.250000000000000_dp
sys%ctax%RatioA=0.670000000000000_dp
sys%ctax%RatioB=0.770000000000000_dp
sys%ctax%RatioC=0.880000000000000_dp
sys%ctax%RatioE=1.22000000000000_dp
sys%ctax%RatioF=1.44000000000000_dp
sys%ctax%RatioG=1.67000000000000_dp
sys%ctax%RatioH=2.00000000000000_dp
 
!rebatesys
sys%rebatesys%RulesUnderFC=.false.
sys%rebatesys%RulesUnderNTC=.true. 
sys%rebatesys%NumAgeRng=2
sys%rebatesys%Restrict=.true. 
sys%rebatesys%docap=.false.
sys%rebatesys%MainCou=85.7500000000000_dp
sys%rebatesys%YngCou=65.3000000000000_dp
sys%rebatesys%MainLP=54.6500000000000_dp
sys%rebatesys%YngLP=43.2500000000000_dp
sys%rebatesys%MainSin=54.6500000000000_dp
sys%rebatesys%YngSin=43.2500000000000_dp
sys%rebatesys%DisregSin=5.00000000000000_dp
sys%rebatesys%DisregLP=25.0000000000000_dp
sys%rebatesys%DisregCou=10.0000000000000_dp
sys%rebatesys%CredInDisregCC=.true. 
sys%rebatesys%PremFam=15.7500000000000_dp
sys%rebatesys%PremLP=0.000000000000000E+000_dp
sys%rebatesys%MaintDisreg=15.0000000000000_dp
sys%rebatesys%taper=0.650000000000000_dp
sys%rebatesys%MinAmt=0.500000000000000_dp
sys%rebatesys%MaxCC1=94.5000000000000_dp
sys%rebatesys%MaxCC2=140.000000000000_dp
sys%rebatesys%MaxAgeCC=15
allocate(sys%rebatesys%AgeRngl(2))
sys%rebatesys%AgeRngl(1)=0
sys%rebatesys%AgeRngl(2)=1
allocate(sys%rebatesys%AgeRngu(2))
sys%rebatesys%AgeRngu(1)=0
sys%rebatesys%AgeRngu(2)=18
allocate(sys%rebatesys%AddKid(2))
sys%rebatesys%AddKid(1)=48.9500000000000_dp
sys%rebatesys%AddKid(2)=38.5000000000000_dp
 
!ctaxben
sys%ctaxben%taper=0.200000000000000_dp
 
!ccben
sys%ccben%dopolltax=.false.
sys%ccben%taper=0.150000000000000_dp
sys%ccben%PropElig=0.800000000000000_dp
sys%ccben%MinAmt=0.500000000000000_dp
sys%ccben%CCrate=0.000000000000000E+000_dp
 
!extra
sys%extra%fsminappamt=.false.
sys%extra%matgrant=.false.
sys%extra%prices=20031201
 
!.f90 FORTAX system; END-OF-FILE
 
