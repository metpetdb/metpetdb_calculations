	Program GTBforMetPetDB

!	This is a simple main calling routine to test the various subroutines for the different calibrations
!	It will be replaced by various UIs in the final version
	implicit none
	integer*4 itype,iCalib,iError,i
	character*16 rxnname
	real*8 Pbars,Tc
	real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet,Fe3Garnet
	real*8 SiBiotite,AlBiotite,TiBiotite,Fe3Biotite,MgBiotite,FeBiotite,MnBiotite,KBiotite
	real*8 SiMuscovite,AlMuscovite,TiMuscovite,Fe3Muscovite,MgMuscovite,FeMuscovite,MnMuscovite,KMuscovite
	real*8 FeChl,MgChl
	real*8 FeMus,MgMus
	real*8 FeIlm,MnIlm
	real*8 FeOpx,MgOpx
	real*8 FeOlivine,MgOlivine
	real*8 FeTour,MgTour
	real*8 FeCord,MgCord
	real*8 FeCpx,MgCpx
	real*8 SiHbl,AlHbl,Fe3Hbl,TiHbl,MgHbl,FeHbl,MnHbl,CaHbl,NaHbl,KHbl
	real*8 NaPlag,CaPlag,KPlag
	real*8 mpbars,msimuscovite,fpbars


1	continue
	write(*,*)'GTB for MetPetDB'
	write(*,*)' 0 = quit'
	write(*,*)' Thermometers...'
	write(*,*)' 1 = Garnet-Biotite Fe-Mg exchange'
	write(*,*)' 2 = Garnet-Chlorite Fe-Mg exchange'
	write(*,*)' 3 = Garnet-Hornblende Fe-Mg exchange'
	write(*,*)' 4 = Garnet-Phengite Fe-Mg exchange'
	write(*,*)' 5 = Garnet-Ilmenite Fe-Mn exchange'
	write(*,*)' 6 = Garnet-Orthopyroxene Fe-Mg exchange'
	write(*,*)' 7 = Garnet-Olivine Fe-Mg exchange'
	write(*,*)' 8 = Garnet-Tourmaline Fe-Mg exchange'
	write(*,*)' 9 = Biotite-Tourmaline Fe-Mg exchange'
	write(*,*)'10 = Garnet-Cordierite Fe-Mg exchange'
	write(*,*)'11 = Garnet-Clinopyroxene Fe-Mg exchange'
	write(*,*)'12 = Hornblende - Plagioclase Thermometers'
	write(*,*)'13 = Muscovite-Biotite Tschermak exchange thermometer'
	write(*,*)'---------------------------------'
	write(*,*)' Barometers...'
	write(*,*)' '	
	write(*,*)' 101 = GASP (garnet-plagioclase-Al2Si05-quartz)'
	write(*,*)'Input type of thermobarometer or barometer'
	read(*,*)itype
	if(itype.eq.0)stop
	if(itype.eq.1)then
10		continue
		write(*,*)' 0 = return'
		WRITE(*,*)' 1 = Ferry and Spear (1978)'
		WRITE(*,*)' 2 = Hodges and Spear (1982)'
		WRITE(*,*)' 3 = Ganguly and Saxena 1 (1984; sym)'
		WRITE(*,*)' 4 = Ganguly and Saxena 2 (1984; asym)'
		WRITE(*,*)' 5 = Perchuk and Lavrent''eva (1984)'
		WRITE(*,*)' 61 = Indares and Martignole (1985) - model 1'
		WRITE(*,*)' 62 = Indares and Martignole (1985) - model 2'
		WRITE(*,*)' 7 = Ferry and Spear with Berman (1990) Grt'
		WRITE(*,*)' 8 = Patio Douce et al. (1993)'
		WRITE(*,*)' 9 = Holdaway et al. (1997)'
		WRITE(*,*)'10 = Gessmann et al. (1997)'
		WRITE(*,*)'11 = Kleemann and Reinhardt (1994)'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Garnet__Hodges_and_Spear_(1982)_Mt.	GARN	102.180	3.034	2.028	.000	.000	.270	2.224	.324	.074	.000	.000	.000		
!_78B_Biotite_____Hodges_and_Spear_(1982)	BIOT	94.280	5.489	3.519	.163	.000	2.262	2.320	.015	.018	.082	1.524	2.000		

		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		Fe3Garnet = 0.0
		SiBiotite = 5.489
		AlBiotite = 3.519
		TiBiotite = 0.163
		Fe3Biotite = 0.0
		MgBiotite = 2.262
		FeBiotite = 2.320
		MnBiotite = 0.015

		Pbars = 1000
		rxnname='Garnet-Biotite Fe-Mg exchange'
		do 11 i = 1,10			! this will calculate 10 P-T points
	Call GarBio_Fe_Mg(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,Fe3Garnet,
     &    SiBiotite,AlBiotite,TiBiotite,Fe3Biotite,MgBiotite,FeBiotite,MnBiotite,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 10
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
11		continue

		go to 10
		endif
	if(itype.eq.2)then
20		CONTINUE
		WRITE(*,*)'Please choose which calibration'
		WRITE(*,*)'You wish to apply'
		WRITE(*,*)'-1 = Plot all calibrations with a single data set'
		WRITE(*,*)' 0 = Return to Rxn menu'
		WRITE(*,*)' 1 = Dickenson and Hewitt (1986)'
		WRITE(*,*)'      (Modified in Laird, 1988)'
		WRITE(*,*)' 2 = D + H w/Py-Gr Correction'
		WRITE(*,*)' 3 = D + H w/Gan-Sax Correction'
		WRITE(*,*)' 4 = D + H w/Berman Correction'
		read(*,*)icalib
		if(iCalib.eq.0)go to 1
		rxnname='Garnet-chlorite Fe-Mg exchange'

!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Garnet__Hodges_and_Spear_(1982)_Mt.	GARN	102.180	3.034	2.028	.000	.000	.270	2.224	.324	.074	.000	.000	.000		
!TM-416_Chl_Spot_				Chlori	87.04	2.6428	2.8150	0.0036	0.0	2.1276	2.3104	0.0034	0.0046	0.0646	0.0106	 4. 
		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		MgChl = 2.1276
		FeChl = 2.3104
		Pbars = 1000
		do 21 i = 1,10			! this will calculate 10 P-T points
		Call GarChl_Fe_Mg(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,
     &    MgChl,FeChl,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 20
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
21		continue
		go to 20
		endif



	if(itype.eq.3)then
30		continue
		write(*,*)' 0 = return'
		WRITE(*,*)' 1 = Graham and Powell (1984)'
		WRITE(*,*)' 2 = Perchuk, et al. (1985)'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Garnet__Hodges_and_Spear_(1982)_Mt.	GARN	102.180	3.034	2.028	.000	.000	.270	2.224	.324	.074	.000	.000	.000		
!SC-160__Hbl					Hbld	97.320	6.213	2.232	.308	.000	2.478	1.785	.017	1.858	.604	.342	.000
      rxnname='Garnet-Hornblende Fe-Mg exchange' 

		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		MgHbl = 2.478
		FeHbl = 1.785
		Pbars = 1000
		do 31 i = 1,10			! this will calculate 10 P-T points
		Call GarHbl_Fe_Mg(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,
     &    MgHbl,FeHbl,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 30
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
31		continue
		go to 30
		endif

	if(itype.eq.4)then
40		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
		WRITE(*,*)' 1 = Krogh and Raheim (1978)'
		WRITE(*,*)' 2 = Green and Hellman (1982) - High Ca rock (>2.0 wt%)'
		WRITE(*,*)' 3 = Green and Hellman (1982) - Low Ca(<2.0 wt%), Hi Mg rock '
		WRITE(*,*)' 4 = Green and Hellman (1982) - Low Ca(<2.0 wt%), Low Mg rock '
		WRITE(*,*)' 5 = Hynes and Forest (1988)'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Garnet__Hodges_and_Spear_(1982)_Mt.	GARN	102.180	3.034	2.028	.000	.000	.270	2.224	.324	.074	.000	.000	.000		
!_78B_Muscovite___Hodges_and_Spear_(1982)	MUSCO	95.120	6.241	5.552	.043	.000	.093	.081	.000	.010	.453	1.389	2.000	.000	.000	.000	.000	.000	.000	

      rxnname='Garnet-phengite Fe-Mg exchange'

		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		MgMuscovite = 2.478
		FeMuscovite = 1.785
		Pbars = 1000
		do 41 i = 1,10			! this will calculate 10 P-T points
		Call GarPhen_Fe_Mg(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,
     &    MgMuscovite,FeMuscovite,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 40
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
41		continue
		go to 40
		endif

	if(itype.eq.5)then
50		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
		WRITE(*,*)' 1 = Pownceby et al. (1987)'
		WRITE(*,*)' 2 = Gan-Sax Garnet correction'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Garnet__Hodges_and_Spear_(1982)_Mt.	GARN	102.180	3.034	2.028	.000	.000	.270	2.224	.324	.074	.000	.000	.000		
!Moose_Ilmenite					Ilmeni	98.667	0.0029	0.0014	0.9684	0.0558	0.0066	0.8806	0.0805	0.0035	0.0	0.0	0.0	0.0	0	0	0	0	0	0	0	0	0	0	0	
      		rxnname='Garnet-ilmenite Fe-Mn exchange'
		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		FeIlm = 0.8806
		MnIlm = 0.0805
		Pbars = 1000
		do 51 i = 1,10			! this will calculate 10 P-T points
		Call GarIlm_Fe_Mn(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,
     &    FeIlm,MnIlm,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 50
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
51		continue
		go to 50
		endif

	if(itype.eq.6)then
60		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
		WRITE(*,*)' 1 = Sen and Bhattacharya (1984)'
		WRITE(*,*)' 2 = Harley (1984)'
		WRITE(*,*)' 3 = Lee and Ganguly (1988)'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Garnet__Hodges_and_Spear_(1982)_Mt.	GARN	102.180	3.034	2.028	.000	.000	.270	2.224	.324	.074	.000	.000	.000		
!SC-160__OPX_(Integrated)			Opx	100.540	1.936	.095	.001	.000	1.218	.731	.015	.018	.000	.000	.000	.000	.000	.000	.000	.000	.000	
		rxnname='Garnet-Orthopyroxene Fe-Mg'
		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		FeOpx = 0.731
		MgOpx = 1.218
		Pbars = 1000
		do 61 i = 1,10			! this will calculate 10 P-T points
		Call GarOpx_Fe_Mg(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,
     &    FeOpx,MgOpx,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 60
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
61		continue
		go to 60
		endif


	if(itype.eq.7)then
70		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
		WRITE(*,*)'1 = O''Neil and Wood (1979)'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Garnet__Hodges_and_Spear_(1982)_Mt.	GARN	102.180	3.034	2.028	.000	.000	.270	2.224	.324	.074	.000	.000	.000		
!Olivine					Olivine	100.540	1.0	0.0	0.0	0.0	1.5	0.5	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
		rxnname='Garnet-Olivine Fe-Mg'
		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		FeOlivine = 0.5
		MgOlivine = 1.5
		Pbars = 1000
		do 71 i = 1,10			! this will calculate 10 P-T points
		Call GarOlivine_Fe_Mg(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,
     &    FeOlivine,MgOlivine,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 70
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
71		continue
		go to 70
		endif

!	write(*,*)' 8 = Garnet-Tourmaline Fe-Mg exchange'
	if(itype.eq.8)then
80		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
      WRITE(*,*)' 1 = Kohn (1987): Colopietro + Friberg (1987)'
      WRITE(*,*)'     with Ferry + Spear (1978) garnet'
      WRITE(*,*)' 2 = Kohn (1987): Colopietro + Friberg (1987)'
      WRITE(*,*)'     with Hodges + Spear (1982) garnet'
      WRITE(*,*)' 3 = Kohn (1987): Colopietro + Friberg (1987)'
      WRITE(*,*)'     with Ganguly + Saxena (1984) garnet'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Garnet__Hodges_and_Spear_(1982)_Mt.	GARN	102.180	3.034	2.028	.000	.000	.270	2.224	.324	.074	.000	.000	.000		
!Tourmaline					Tourm	100.540	1.0	0.0	0.0	0.0	1.5	0.5	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
      rxnname='GARNET-TOURMALINE Fe-Mg'
		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		FeTour = 0.5
		MgTour = 1.5
		Pbars = 1000
		do 81 i = 1,10			! this will calculate 10 P-T points
		Call GarTourmaline_Fe_Mg(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,
     &    FeTour,MgTour,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 80
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
81		continue
		go to 80
		endif

!	write(*,*)' 9 = Biotite-Tourmaline Fe-Mg exchange'
	if(itype.eq.9)then
90		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
      WRITE(*,*)' 1 = Colopietro and Friberg (1987)'
      WRITE(*,*)' 2 = Colo and Fri (1987) + Delta V'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Biotite_____Hodges_and_Spear_(1982)	BIOT	94.280	5.489	3.519	.163	.000	2.262	2.320	.015	.018	.082	1.524	2.000		
!Tourmaline					Tourm	100.540	1.0	0.0	0.0	0.0	1.5	0.5	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
      rxnname='TOURMALINE-BIOTITE Fe-Mg exchange'
		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		MgBiotite = 2.262
		FeBiotite = 2.320
		FeTour = 0.5
		MgTour = 1.5
		Pbars = 1000
		do 91 i = 1,10			! this will calculate 10 P-T points
		Call BiotiteTourmaline_Fe_Mg(
     &    FeBiotite,MgBiotite,
     &    FeTour,MgTour,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 90
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
91		continue
		go to 90
		endif

!	write(*,*)'10 = Garnet-Cordierite Fe-Mg exchange'
	if(itype.eq.10)then
100		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
      WRITE(*,*)' 1 = Nichols, Berry and Green (1992 CMP)'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Garnet__Hodges_and_Spear_(1982)_Mt.	GARN	102.180	3.034	2.028	.000	.000	.270	2.224	.324	.074	.000	.000	.000		
! Cordierite			8              Cord	98.0	.000	.000	5	18	.0 	.000	.8	.2	0.0	.0	.000	.000	1.000	
            rxnname='Garnet-Cordierite Fe-Mg exchange' 
		MgGarnet = 0.270
		FeGarnet = 2.224
		MnGarnet = 0.324
		CaGarnet = 0.074
		FeCord = 0.2
		MgCord = .8
		Pbars = 1000
		do 101 i = 1,10			! this will calculate 10 P-T points
		Call GarCord_Fe_Mg(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,
     &    FeCord,MgCord,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 100
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
101		continue
		go to 100
		endif

!	write(*,*)'11 = Garnet-Clinopyroxene Fe-Mg exchange'
	if(itype.eq.11)then
110		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
      WRITE(*,*)' 1 = Ellis and Green (1979)'
      WRITE(*,*)' 2 = Powell (1985)'
      WRITE(*,*)' 3 = Pattison and Newton (1989) (Xgrs>0.15)'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!SC-160__Garnet			3		Garn	101.230	2.991	1.959	.006	.000	.991	1.508	.079	.488	.000	.000	.000	
! SC-160__CPX_(Integrated)	2		CPx	99.860	1.905	.199	.012	.000	.683	.308	.006	.820	.101	.000	.000	
      rxnname='Garnet-Clinopyroxene Fe-Mg exchange'
		MgGarnet = 0.991
		FeGarnet = 1.508
		MnGarnet = 0.079
		CaGarnet = 0.488
		Fe3Garnet = 0.
		FeCpx = 0.308
		MgCpx = .683
		Pbars = 1000
		do 111 i = 1,10			! this will calculate 10 P-T points
		Call GarCpx_Fe_Mg(
     &    FeGarnet,MgGarnet,MnGarnet,CaGarnet,Fe3Garnet,
     &    FeCpx,MgCpx,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 110
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
111		continue
		go to 110
		endif

!	write(*,*)'12 = Hornblende - Plagioclase Na-Ca exchange'
	if(itype.eq.12)then
120		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
		WRITE(*,*)'1 = Holland and Blundy (1994) - Na Alkali site exchange'
		WRITE(*,*)'1 = Holland and Blundy (1994) - Ca-Na M4 exchange'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!SC-160__Hbl			4		Hbld	97.320	6.213	2.232	.308	.000	2.478	1.785	.017	1.858	.604	.342	.000	
!SC-160__PLAG			5		Plag	100.000	2.650	1.350	.004	.000	.000	.000	.000	.350	.650	.000	.000	
      rxnname='Hornblende - Plagioclase Na-Ca exchange'

		SiHbl = 6.213
		AlHbl = 2.232
		TiHbl = .308
		Fe3Hbl = 0
		MgHbl = 2.478
		FeHbl = 1.785
		MnHbl = .017
		CaHbl = 1.858
		NaHbl = .604
		KHbl = .342
		NaPlag = .65
		CaPlag = .35
		KPlag = 0.

		Pbars = 1000
		do 121 i = 1,10			! this will calculate 10 P-T points
		Call HbldPlag_Na_Ca(
     & SiHbl,AlHbl,TiHbl,Fe3Hbl,MgHbl,FeHbl,MnHbl,CaHbl,NaHbl,KHbl,
     & NaPlag,CaPlag,KPlag,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.1)then
			write(*,*)'Keq is less than or = zero - try again with new minerals'
			go to 120
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
121		continue
		go to 120
		endif


!	write(*,*)'13 = Muscovite-Biotite Tschermak exchange thermometer'
	if(itype.eq.13)then
130		continue
		WRITE(*,*)' 0 = Return to Rxn menu'
		write(*,*)' 1 = Hoisch (1989; Am Min - not Implemented'
		write(*,*)' 2 = Wu, Pan & Wang, JMG, 2000 - manuscript'
		write(*,*)'Input choice of model'
		read(*,*)iCalib
		if(iCalib.eq.0)go to 1
		if(iCalib.eq.1)then
			write(*,*)'Not yet implemented'
			go to 130
			endif
!	Test data
!Sample						Min	Wt%tot	CSi	CAl	CTi	CFe3+	CMg	CFe2+	CMn	CCa	CNa	CK	CH2O	
!_78B_Biotite_____Hodges_and_Spear_(1982)	BIOT	94.280	5.489	3.519	.163	.000	2.262	2.320	.015	.018	.082	1.524	2.000	
!_78B_Muscovite___Hodges_and_Spear_(1982)	MUSCO	95.120	6.241	5.552	.043	.000	.093	.081	.000	.010	.453	1.389	2.000	
      rxnname='Muscovite-biotite Mg-tschermak exchange thermometer'

		SiBiotite = 5.489
		AlBiotite = 3.519
		TiBiotite = .163
		Fe3Biotite = 0
		MgBiotite = 2.262
		FeBiotite = 2.320
		MnBiotite = .025
		KBiotite = 1.524
		SiMuscovite = 6.241
		AlMuscovite = 5.552
		TiMuscovite = .043
		Fe3Muscovite = 0
		MgMuscovite = .093
		FeMuscovite = .081
		MnMuscovite = 0
		KMuscovite = 1.389

		Pbars = 1000
		do 131 i = 1,10			! this will calculate 10 P-T points
		Call MuscBiot_Tschermak(
     &    SiBiotite,AlBiotite,TiBiotite,Fe3Biotite,MgBiotite,FeBiotite,MnBiotite,KBiotite,
     &    SiMuscovite,AlMuscovite,TiMuscovite,Fe3Muscovite,MgMuscovite,FeMuscovite,MnMuscovite,KMuscovite,
     &    Pbars,TC,iCalib,iError)
		if(iError.eq.2)then
			write(*,*)'Al6 in biot <= 0:  Try again'
			go to 130
			endif
		write(*,*)Pbars,TC
		Pbars = Pbars + 1000
131		continue
		go to 130
		endif


	end

