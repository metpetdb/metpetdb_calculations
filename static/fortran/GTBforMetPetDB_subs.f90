!================================================================
!      Garnet-Biotite Fe-Mg exchange
!================================================================
      Subroutine GarBio_Fe_Mg( &
      FeGarnet,MgGarnet,MnGarnet,CaGarnet,Fe3Garnet, &
      SiBiotite,AlBiotite,TiBiotite,Fe3Biotite,MgBiotite,FeBiotite,MnBiotite, &
      Pbars,TC,iCalib,iError)
      implicit none
      integer*4 iCalib,iError,igs
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet,Fe3Garnet
      real*8 SiBiotite,AlBiotite,TiBiotite,Fe3Biotite,MgBiotite,FeBiotite,MnBiotite
      real*8 Pbars,TC,TK,Told,chgfac,temp,temp1,K,RlnGFeGMg,D,KD,R
      real*8 v,s,h,eqcon
      real*8 Xprp,Xalm,Xsps,Xgrs
      real*8 tetra,al4b,al6b,xann,xphl,spd,sh,xAlB,xTiB,xAlBpd,xTiBpd,xphlpd,xannpd,xalBh,xTiBh,xannh,xphlh
      real*8 wCa,wMn,wFeMg,wGr,Gprp,Galm,GBt

!f2py intent(in, out) TC

      if(iCalib.eq.9) then
      if(Fe3Garnet.lt.0.001) then
      Fe3Garnet=0.03*FeGarnet
      FeGarnet=0.97*FeGarnet
      chgfac=24./(24. + Fe3Garnet)
      Fe3Garnet = Fe3Garnet*chgfac
      MgGarnet = MgGarnet*chgfac
      FeGarnet = FeGarnet*chgfac
      MnGarnet = MnGarnet*chgfac
      CaGarnet = CaGarnet*chgfac
      endif

      if(Fe3Biotite.lt.0.001) then
      Fe3Biotite=0.116*FeBiotite
      FeBiotite=0.884*FeBiotite
      chgfac=22./(22.+Fe3Biotite)
      if(SiBiotite.gt.4.)chgfac=44./(44.+Fe3Biotite)
      SiBiotite = chgFac*SiBiotite
      AlBiotite = chgFac*AlBiotite
      TiBiotite = chgFac*TiBiotite
      Fe3Biotite = chgFac*Fe3Biotite
      MgBiotite = chgFac*MgBiotite
      FeBiotite = chgFac*FeBiotite
      MnBiotite = chgFac*MnBiotite
      endif
      endif

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      tetra=4
      if (SiBiotite.ge.4) tetra = 8
      al4b=tetra - SiBiotite
      al6b=AlBiotite-al4b
      XANN=FeBiotite/(MgBiotite+FeBiotite)
      XPHL=MgBiotite/(MgBiotite+FeBiotite)
      spd=al6b+TiBiotite+Fe3Biotite+MgBiotite+FeBiotite+MnBiotite

      sh=3.0      
      if(spd.gt.sh)sh=spd
      xalb=al6b/spd
      xtib=TiBiotite/spd
      xalbpd=al6b*2/tetra
      xtibpd=TiBiotite*2/tetra
      xphlpd=(1-xalbpd-xtibpd)*MgBiotite/(MgBiotite+FeBiotite)
      xannpd=(1-xalbpd-xtibpd)*FeBiotite/(MgBiotite+FeBiotite)

      xalbh=al6b*4/(sh*tetra)
      xtibh=TiBiotite*4/(sh*tetra)
      xannh=FeBiotite*4/(sh*tetra)
      xphlh=MgBiotite*4/(sh*tetra)

      TEMP=Xprp*XANN/(XPHL*Xalm)
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif
      V=.057
      S=4.662
      H=12454.

! -------------------------------------------------------
!      Ferry and Spear (1978) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Ferry + Spear (1978)'
      V=.057
      S=4.662
      H=12454.
      EQCON=3*dlog(TEMP)
      TK=(H+Pbars*V)/(S-1.987*EQCON)
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Hodges and Spear (1982) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      calname='Hodges + Spear (1982)'
      h=12454+3300*Xgrs*3
      s=4.662+4.5*Xgrs
      EQCON=3*dlog(TEMP)
      TK=(H+Pbars*V)/(S-1.987*EQCON)
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Ganguly and Saxena 1 and 2 (1984) Calib.
! -------------------------------------------------------
      IF (ICALIB.EQ.3) THEN
      calname='Ganguly + Saxena (1984) symmetric Garnet solution model'

      WCA=3000
      WMN=3000
      WFEMG=(Xalm*2500+Xprp*200)/(Xalm+Xprp)
      EQCON=dlog(TEMP)
      TK=(1175+Pbars*.00945+(WFEMG*(Xalm-Xprp)+WCA*Xgrs+&
      WMN*Xsps)/1.987)/(.782-EQCON)
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Ganguly and Saxena 1 and 2 (1984) Calib.
! -------------------------------------------------------
      IF (ICALIB.EQ.4) THEN
      calname='Ganguly + Saxena (1984) asymmetric Garnet solution mode'

      WCA=3000
      WMN=3000
      WFEMG=(Xalm*2500+Xprp*200)/(Xalm+Xprp)
      TK=775
      igs = 1
      EQCON=dlog(TEMP)
      TK=(1175+Pbars*.00945+(WFEMG*(Xalm-Xprp)+WCA*Xgrs+&
      WMN*Xsps)/1.987)/(.782-EQCON)
      told=TK
5435      told=(told+TK)/2
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,Gprp,1,igs)
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,galm,2,igs)
      TK=(1175+Pbars*.00945)/(0.782-dlog(temp*Gprp/galm))
      if (abs(TK-told).ge.0.25) goto 5435

      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Perchuk and Lavrent'eva (1984) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.5) THEN
      calname='Perchuk + Lavrenteva (1984)'
      TEMP1=1/TEMP
      EQCON=dlog(TEMP1)
      TK=(7843.7+(Pbars-6000)*.04115)/(1.987*EQCON+5.699)
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Indares and Martignole (1985) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.61) THEN
      calname='Indares + Martignole (1985) (Ganguly and Saxena garnet model)'
      TK=(H+Pbars*V+3*(-1590*xalb-7451*xtib)+9000*(Xgrs+Xsps))/&
      (4.662-5.9616*dlog(temp))
      TC=TK-273.15
      endif
! -------------------------------------------------------
!      Indares and Martignole (1985) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.62) THEN
      calname='Indares + Martignole (1985) (Hodges and Spear garnet model)'
      wgr=1800
!      DO 5165 K=1,10
      TK=(H+Pbars*V+3*(-464*xalb-6767*xtib)+3*wgr*Xgrs)/&
      (4.662-5.9616*dlog(temp))
      WGR=(3300-1.5*TK)
!5165      CONTINUE
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Ferry and Spear (1978) with Berman (1990) garnet model
! -------------------------------------------------------
      IF (ICALIB.EQ.7) THEN
!9007 continue
      calname='Ferry + Spear (1978) w/Berman (1990) Garnet solution model'
      igs=4
      EQCON=3*dlog(TEMP)
      v=.057
      h=12454.+9900*Xgrs
      s=4.662-4.5*Xgrs
      TK=(h+pbars*v)/(s-1.987*eqcon)

      H = 11865./(3.*1.987)
      V = .057/(3.*1.987)
      S = 4.662/(3.*1.987)
      told=TK
5175      told=(told+TK)/2
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,Gprp,1,igs)
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,galm,2,igs)
      TK = (H + Pbars*V)/(S - dlog(temp*Gprp/galm)) !one atom basis
      if(abs(TK-told).ge..25)goto 5175
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Patio Douce et al.
!      Coded by F.S. Spear (7/95) based on 1993 Am. Min paper
! -------------------------------------------------------
      IF (ICALIB.EQ.8) THEN
      calname='Patio Douce et al.(1993 Am Min).'
      v=.057
      h=12454. + 9900*Xgrs
      s=4.662 - 4.5*Xgrs
      TK=(h+pbars*v)/(s-1.987*3*dlog(temp))
      igs=4      ! Berman garnet activity model

      H = 11865.*4.184      !calculations done in joules/3 cations
      V = .057*4.184
      S = 4.662*4.184
      EQCON=3*dlog(Xprp/Xalm)+3.*dlog(xannpd/xphlpd)
      told=TK
6185 told=(told+TK)/2
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,Gprp,1,igs)
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,galm,2,igs)
      RlnGFeGMg = (xalbpd*(57400-54700)+XTibpd*(65100-75100))/told
      D = S - (8.3144*(eqcon + 3.*dlog(Gprp/galm)) + 3.*RlnGFeGMg)
      TK = (H + V*Pbars)/D
      if(abs(TK-told).ge..25)goto 6185
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Holdaway et al.
! -------------------------------------------------------
      IF (ICALIB.EQ.9) THEN
      calname='Holdaway et al. (1997)'
      igs=5
      Temp=(Xprp*Xannh)/(Xalm*Xphlh)
      v=.057
      h=12454.+9900*Xgrs
      s=4.662-4.5*Xgrs
      TK=(h+pbars*v)/(s-1.987*3*dlog(temp))
      told=TK
5195      told=(told+TK)/2
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,Gprp,1,igs)
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,galm,2,igs)
      gbt=exp(((40719.-30.*told)*(Xannh-Xphlh) - &
      (210190.-245.40*told)*XAlbh - &
      (310990.-370.39*told)*Xtibh)/(3.*8.3144*told))
      TK=(1682.+Pbars*.01247)/(0.41494-dlog(temp*Gprp/(galm*gbt)))
      if(abs(TK-told).ge..025)goto 5195
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Gessman et al.
! -------------------------------------------------------
      IF (ICALIB.EQ.10) THEN
      calname='Gessman et al. (1997)'
      igs=4
      v=.057
      h=12454.+9900*Xgrs
      s=4.662-4.5*Xgrs
      TK=(h+pbars*v)/(s-1.987*3*dlog(temp))
      told=TK
5205      told=(told+TK)/2
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,Gprp,1,igs)
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,galm,2,igs)
      TK=(-50905-0.236*Pbars+5403*(xphlh-xannh)+60532*xalbh)/ &
        (-17.18+3.*8.3144*dlog(temp*Gprp/galm))
      if(abs(TK-told).ge..025)goto 5205
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Kleemann and Reinhardt (1994)
! -------------------------------------------------------
      IF (ICALIB.EQ.11) THEN
      calname='Kleemann and Reinhardt (1994)(Uses Berman garnet model'

      KD = Xprp*XANN/(XPHL*Xalm)
      v=.057
      h=12454.+9900*Xgrs
      s=4.662-4.5*Xgrs
      EQCON=3*dlog(KD)
      tk=(h+pbars*v)/(s-1.987*eqcon)
      R = 8.3144
      V = 0.108      !j/bar
      told = tk
5975      told=(told+tk)/2.
      igs=4      !Calculate using Berman's garnet model
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,Gprp,1,igs)
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,galm,2,igs)
      TK = (20253. + 77785.*XAlb - 18138.*XTib + (V)*Pbars)/(10.66 - R*dlog(KD) - R*dlog(Gprp/galm) + 94.1*XAlb - 11.7*XTib)
      if(abs(tk-told).ge..25)goto 5975
      TC=TK-273.15
      ENDIF

      END


!================================================================
!      Garnet-Chlorite Fe-Mg exchange
!================================================================
      Subroutine GarChl_Fe_Mg( &
      FeGarnet,MgGarnet,MnGarnet,CaGarnet, &
      MgChl,FeChl, &
      Pbars,TC,iCalib,iError)
      implicit none
      integer*4 iCalib,iError,igs
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet
      real*8 FeChl,MgChl
      real*8 Pbars,TC,TK,Told,temp
      real*8 v,s,h,eqcon
      real*8 Xprp,Xalm,Xsps,Xgrs
      real*8 XFeC,XMgC
      real*8 Gprp,Galm

!f2py intent(in, out) TC

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      XFEC=FeChl
      XMGC=MgChl
      TEMP=(Xprp*XFEC)/(XMGC*Xalm)
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      Dickenson and Hewitt (1986) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Dickenson + Hewitt (1986) Modified in Laird (1988)'
      EQCON=15.*dlog(TEMP)
      TK=(55841+0.212*Pbars)/(10.76-1.987*EQCON)
      TC=TK -273.15
      ENDIF
! -------------------------------------------------------
!      D + H (1986) Calib. W\Py-Gr correction in garnet
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      calname='Dickenson + Hewitt (1986) as in Laird (1988) w/Hodges + Spear (1982) Grt'
      H=55841.+15*3300*Xgrs
      S=10.76+15*1.5*Xgrs
      V=.212
      EQCON=15*dlog(TEMP)
      TK=(H+Pbars*V)/(S-1.987*EQCON)
      TC=TK-273.15
      ENDIF
!----------------------------------------------------------------------
!      D + H Calibration w/Gan + Sax Garnet Correction
!----------------------------------------------------------------------
      IF (ICALIB.EQ.3) THEN
      calname='Dickenson + Hewitt (1986) as in Laird (1988) w/Ganguly + Saxena (1984) Grt'
      H=55841./(15*1.987)-914.
      S=10.76/(15*1.987)
      V=.212/(15*1.987)
      told=775
5135      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,Gprp,1,1)
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,galm,2,1)
      TK=(H+Pbars*V)/(S-dlog(temp*Gprp/galm))
      if(abs(TK-told).ge.1.)then
      told=(TK+told)/2
      go to 5135
      endif
      TC=TK-273.15
      ENDIF
!----------------------------------------------------------------------
!      D + H Calibration w/Berman Garnet Correction
!----------------------------------------------------------------------
      IF (ICALIB.EQ.4) THEN
      calname='Dickenson + Hewitt (1986) as in Laird (1988) w/Berman (1990) Grt'
      igs=4
      EQCON=3*dlog(TEMP)
      H=55841.+15*3300*Xgrs
      S=10.76+15*1.5*Xgrs
      V=.212
      TK=(H+Pbars*V)/(S-1.987*EQCON)
      v=.212/(15*1.987)
      h=55841/(15*1.987)-111
      s=10.76/(15*1.987)

      told=TK
5445      told=(told+TK)/2
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,Gprp,1,igs)
      call gs(told,Pbars,Xprp,Xalm,Xsps,Xgrs,galm,2,igs)
      TK=(h+Pbars*v)/(s-dlog(temp*Gprp/galm))
      if(abs(TK-told).ge..25)goto 5445
      TC=TK-273.15
      ENDIF
      END


!================================================================
!      Garnet-Hornblende Fe-Mg exchange
!================================================================
      Subroutine GarHbl_Fe_Mg(&
      FeGarnet,MgGarnet,MnGarnet,CaGarnet,&
      MgHbl,FeHbl,&
      Pbars,TC,iCalib,iError)
      implicit none
      integer*4 iCalib,iError
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet
      real*8 FeHbl,MgHbl
      real*8 Pbars,TC,TK,temp
      real*8 Xprp,Xalm,Xsps,Xgrs
      real*8 XFeH,XMgH

!f2py intent(in, out) TC

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      XFEH=FeHbl/(FeHbl+MgHbl)
      XMgH=MgHbl/(FeHbl+MgHbl)
      TEMP=(Xalm*XMGH)/(Xprp*XFEH)
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      Graham and Powell (1984) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      TK=(2880.+3280.*Xgrs)/(dlog(TEMP)+2.426)
      TC=TK-273.15
      calname='Graham and Powell (1982)'
      ENDIF
! -------------------------------------------------------
!      Perchuk, et al (1985) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      TK=3330/(dlog(TEMP)+2.333)
      TC=TK-273.15
      calname='Perchuk, et al. (1985)'
      ENDIF
      END


!================================================================
!      Garnet-Phengite Fe-Mg exchange
!================================================================
      Subroutine GarPhen_Fe_Mg(&
      FeGarnet,MgGarnet,MnGarnet,CaGarnet,&
      MgMus,FeMus,&
      Pbars,TC,iCalib,iError)
      implicit none
      integer*4 iCalib,iError,j
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet
      real*8 FeMus,MgMus
      real*8 Pbars,TC,TK,temp
      real*8 Xprp,Xalm,Xsps,Xgrs
      real*8 FeMgRat,WFeMg

!f2py intent(in, out) TC

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      TEMP=(Xalm*MgMus/(FeMus*Xprp))
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      Krogh and Raheim (1978) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Krogh + Raheim (1978)'

      TK=(3685+0.0771*Pbars)/(dlog(TEMP)+3.52)
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Green and Hellman (1982) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      calname='Green + Hellman (1982) - High Ca rock (>2.0 wt%)'
      TK=(5170+0.036*Pbars)/(dlog(TEMP)+4.17)
      TC=TK-273.15
      ENDIF
      IF (ICALIB.EQ.3) THEN
      calname='Green + Hellman (1982) - Low Ca(<2.0 wt%), Hi Mg rock'
      TK=(5560.+0.036*Pbars)/(dlog(TEMP)+4.65)
      TC=TK-273.15
      ENDIF
      IF (ICALIB.EQ.4) THEN
      calname='Green + Hellman (1982) - Low Ca(<2.0 wt%), Low Mg rock'
      TK = (5680.+0.036*Pbars)/(dlog(TEMP)+4.48)
      TC=TK-273.15
      ENDIF
! -------------------------------------------------------
!      Hynes and Forest (1988) Calibration (J. Met. Geol)
! -------------------------------------------------------
      IF (ICALIB.EQ.5) THEN
      calname='Hynes + Forest (1988)'
      femgrat=Xalm/(Xalm+Xprp)
      wfemg=200.*(1-femgrat)+2500.*femgrat
      TK=825
      do 5430 j=1,10
      TK=4790/(dlog(temp)+(0.8*wfemg-wfemg*(Xalm-Xprp)-&
      3000*Xsps)/(1.987*TK) - 2.978*Xgrs*844./TK +&
      5.906*((Xgrs*844/TK)**2) + 4.13)
5430        continue
      TC=TK-273.15
      Endif
      END


!================================================================
!      Garnet-Ilmenite Fe-Mn exchange
!================================================================
      Subroutine GarIlm_Fe_Mn(&
      FeGarnet,MgGarnet,MnGarnet,CaGarnet,&
      FeIlm,MnIlm,&
      Pbars,TC,iCalib,iError)
      implicit none
      integer*4 iCalib,iError,j
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet
      real*8 FeIlm,MnIlm,XFeI,XMnI
      real*8 Pbars,TC,TK,temp
      real*8 Xprp,Xalm,Xsps,Xgrs
      real*8 galm,gsps

!f2py intent(in, out) TC

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      XFEI=FeIlm
      XMNI=MnIlm
      TEMP=(Xsps*XFEI/(XMNI*Xalm))
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif
! -------------------------------------------------------
!      Pownceby et al. (1987) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Pownceby et al. (1987)'

      TK=(-4089+420*(2*XMNI-1)+77*(2*Xsps-1))/&
      (-1.987*dlog(TEMP)-1.44)
      TC = TK-273.15
      ENDIF
! -------------------------------------------------------
!      P. et al. (1987) Calib. W/Gan and Sax Gar Corr.
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      calname='Pownceby et al. (1987) w/ Ganguly + Saxena (1984) Garnet'
      TK=775
      do 5125 j=1,20
      call gs(TK,Pbars,Xprp,Xalm,Xsps,Xgrs,galm,2,1)
      call gs(TK,Pbars,Xprp,Xalm,Xsps,Xgrs,gsps,3,1)
      TK=(-4089+420*(2*XMNI-1))/&
      (-1.987*dlog(TEMP*gsps/galm)-1.44)
5125      CONTINUE
      TC = TK-273.15
      ENDIF
      END


!================================================================
!      Garnet-Orthopyroxene Fe-Mg exchange thermometer 
!================================================================
      Subroutine GarOpx_Fe_Mg(&
      FeGarnet,MgGarnet,MnGarnet,CaGarnet,&
      FeOpx,MgOpx,&
      Pbars,TC,iCalib,iError)
      implicit none
      integer*4 iCalib,iError
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet
      real*8 FeOpx,MgOpx,XFeP,XMgP
      real*8 Pbars,TC,TK,temp
      real*8 Xprp,Xalm,Xsps,Xgrs

!f2py intent(in, out) TC

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      XFEP=FeOpx
      XMGP=MgOpx
      TEMP=(Xalm/Xprp)*(XMGP/XFEP)
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      Sen and Bhattacharya (1984) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Sen + Bhattacharya (1984)'
      TK=(2713.+.022*Pbars+3300.*Xgrs+195.*(Xalm-Xprp))/&
      (1.987*dlog(TEMP)+0.787+1.5*Xgrs)
      TC = TK-273.15
      ENDIF
! -------------------------------------------------------
!      Harley (1984) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      calname='Harley (1984)'
      TK = (3740+(Xgrs)*1400+.02286*Pbars)/(1.987*dlog(TEMP)+1.96)
      TC = TK-273.15
      ENDIF
! -------------------------------------------------------
!      Lee and Ganguly (1988) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.3) THEN
      calname='Lee and Ganguly (1988) Eq''n 11.2 (w/Cp)'
      TK=(1971+.01191*Pbars+3000*(Xgrs)/1.987)/(dlog(temp)+0.96)
      TC = TK-273.15

      ENDIF
! -------------------------------------------------------
!      Bhattacharya, Krishnakumar, Raith and Sen (1991) Calibration
! -------------------------------------------------------
!      IF (ICALIB.EQ.4) THEN
!9004 continue
!      Pbars=PMIN-500
!      i=0
!      ATERM=-1220*Xalm*Xprp - 441.*Xgrs*(Xprp-Xalm) - 136.*Xprp**2 +&
!      746.*Xalm**2
!
!      TEMP=(XFeP/XMgP)/(Xalm/Xprp) !KD
!
!5710  Pbars=Pbars+500
!      IF (Pbars-100.GE.PMAX)GOTO 5711
!      i=i+1
!      T=(1611.+0.021*Pbars+906.*Xgrs+ATERM+477.*(2*XMgP-1.))/&
!      (-dlog(TEMP)+0.796)
!      
!      TC = TK-273.15
!      Tplot(i)=T
!      PresPlot(i)=Pbars/1000.
!      GO TO 5710
!5711  continue
!      call plotTP(i)
!      if(i.eq.0)go to 1
!      ENDIF
      END


!================================================================
!      Garnet-Olivine Fe-Mg thermometer
!================================================================
      Subroutine GarOlivine_Fe_Mg(&
      FeGarnet,MgGarnet,MnGarnet,CaGarnet,&
      FeOlivine,MgOlivine,&
      Pbars,TC,iCalib,iError)
      implicit none
      integer*4 iCalib,iError,j
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet
      real*8 FeOlivine,MgOlivine,MnOlivine,XFeOl,XMgOl
      real*8 Pbars,TC,TK,temp
      real*8 Xprp,Xalm,Xsps,Xgrs
      real*8 wfmo,wfmg,wgr,H,S,pk,DV

!f2py intent(in, out) TC

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      XFEOL=FeOlivine/(FeOlivine+MgOlivine+MnOlivine)
      XFEOL=MgOlivine/(FeOlivine+MgOlivine+MnOlivine)

      TEMP=(Xalm/Xprp)*(XMGOL/XFEOL)
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      O'Neil and Wood (1979) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='O''Neil + Wood (1979)'
      wfmo=990.
      wfmg=195.
      wgr=2676.
      H=-10750
      S=-4.26
      pk=Pbars/1000
      DV=0
      Do 5615 j=1,5
      TK=(902+DV+(xmgol-xfeol)*(498+1.51*(pk-30))-&
      98*(Xprp-Xalm)+1347*Xgrs)/(dlog(temp)+0.357)
      DV=462.5*(1.0191+(TK-1073))*2.87e-5*(pk-2.63e-4*(pk**2)&
      -29.76)+262.4*(1.0292+(TK-1073)*4.5e-5)*(pk-3.9e-4*&
      (pk**2)-29.65)-454*(1.02+(TK-1073)*2.84e-5)*(pk-2.36e-4*&
      (pk**2)-29.79)-278.3*(1.0234+(TK-1073)*2.3e-5)*(pk -&
      4.5e-4*(pk**2)-29.6)
5615      continue
      TC = TK-273.15
      ENDIF
      END


!================================================================
! ----- GARNET-TOURMALINE Fe-Mg ----------------------------------------------
!================================================================
      Subroutine GarTourmaline_Fe_Mg(&
      FeGarnet,MgGarnet,MnGarnet,CaGarnet,&
      FeTour,MgTour,&
      Pbars,TC,iCalib,iError)

      implicit none
      integer*4 iCalib,iError,j
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet
      real*8 FeTour,MgTour,XFeT,XMgT
      real*8 Pbars,TC,TK,temp
      real*8 Xprp,Xalm,Xsps,Xgrs
      real*8 galm,gprp,H,S,V,eqcon,R

!f2py intent(in, out) TC

      V=.0070
      S=3.738
      H=1040.

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      XFET=FeTour
      XMGT=MgTour
      TEMP=(XMGT*Xalm)/(Xprp*XFET)
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      Kohn (1987) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Kohn:Colopietro+Friberg (1987) Ferry+Spear (1978) garnet'
      EQCON=dlog(TEMP)
      TK=(H + V*Pbars)/(S - EQCON) 
      TC = TK - 273.15
      ENDIF
! -------------------------------------------------------
!      Kohn (1987) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      calname='Kohn:Colopietro+Friberg (1987) Hodges+Spear(1982) garnet'
      h=h-3300*Xgrs
      s=s-1.5*Xgrs
      EQCON=dlog(TEMP)
      TK=(H + V*Pbars)/(S - EQCON)
      TC = TK-273.15
      ENDIF
! -------------------------------------------------------
!      Ganguly and Saxena (1984) Calib.
! -------------------------------------------------------
      IF (ICALIB.EQ.3) THEN
      calname='Kohn:Colopietro+Friberg(1987) Ganguly+Saxena(1984)garnet'
      R=1.987
      H=1040.+914.
      TK = 700      ! an initial guess
      do 5135 j=1,15
      call gs(TK,Pbars,Xprp,Xalm,Xgrs,Xsps,gprp,1,1)
      call gs(TK,Pbars,Xprp,Xalm,Xgrs,Xsps,galm,2,1)
      TK=(H+Pbars*V)/(S-dlog(temp*galm/gprp))
5135      continue
      TC = TK-273.15
      ENDIF
      END


!================================================================
! ----- Biotite-TOURMALINE Fe-Mg ----------------------------------------------
!================================================================
      Subroutine BiotiteTourmaline_Fe_Mg(&
      FeBiotite,MgBiotite,&
      FeTour,MgTour,&
      Pbars,TC,iCalib,iError)

      implicit none
      integer*4 iCalib,iError
      character*256 calname
      real*8 FeBiotite,MgBiotite,Xann,XPhl
      real*8 FeTour,MgTour,XFeT,XMgT
      real*8 Pbars,TC,TK,temp
      real*8 H,S,V,eqcon

!f2py intent(in, out) TC

      XFET=FeTour
      XMGT=MgTour

      XANN=FeBiotite
      XPHL=MgBiotite
      TEMP=(XMGT*XANN/(XPHL*XFET))
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      Colopietro and Friberg (1987) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Colopietro + Friberg (1987)'
      H=-3150
      S=4.52
      EQCON=dlog(TEMP)
      TK = H/(EQCON-S) 
      TC = TK - 273.15
      ENDIF
! -------------------------------------------------------
!      Modified Colopietro and Friberg (1987) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      calname='Colopietro + Friberg (1987) with delta V of reaction'
      H=3100.
      S=4.52
      V=.01661
      EQCON=dlog(TEMP)
      TK = (H + V*Pbars)/(S - EQCON)
      TC = TK - 273.15
      ENDIF
      END


!================================================================
! ----- GARNET-Cordierite Fe-Mg ----------------------------------------------
!================================================================
      Subroutine GarCord_Fe_Mg(&
      FeGarnet,MgGarnet,MnGarnet,CaGarnet,&
      FeCord,MgCord,&
      Pbars,TC,iCalib,iError)

      implicit none
      integer*4 iCalib,iError
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet
      real*8 FeCord,MgCord,XFeCrd,XMgCrd
      real*8 Pbars,TC,TK,temp
      real*8 Xprp,Xalm,Xsps,Xgrs
      real*8 R

!f2py intent(in, out) TC

      R = 8.3144

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      XFeCrd=FeCord/(FeCord + MgCord)
      XMgCrd=MgCord/(FeCord + MgCord)
      TEMP=((Xalm*XMGCrd)/(Xprp*XFECrd))**6
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      Nichols, Berry and Green (1992 CMP) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Nichols, Berry and Green (1992 CMP)'
      TK = (113116. + 630.*Pbars/1000.)/(31.87 + R*dlog(temp))
      TC = TK - 273.15
      ENDIF
      END


!================================================================
! ----- GARNET-Clinopyroxene Fe-Mg ----------------------------------------------
!================================================================
      Subroutine GarCpx_Fe_Mg(&
      FeGarnet,MgGarnet,MnGarnet,CaGarnet,Fe3Garnet,&
      FeCpx,MgCpx,&
      Pbars,TC,iCalib,iError)

      implicit none
      integer*4 iCalib,iError,j
      character*256 calname
      real*8 FeGarnet,MgGarnet,MnGarnet,CaGarnet,Fe3Garnet
      real*8 FeCpx,MgCpx,XFeP,XMgP
      real*8 Pbars,TC,TK,temp
      real*8 Xprp,Xalm,Xsps,Xgrs,XCa,XMg,Xand
      real*8 ptt2,ptb2,ptt3,ptb3,ptt4,ptb4,ptt5,ptb5,t2,t3,t4,t5
      real*8 pattn2(8),pattn3(8),pattn4(8),pattn5(8)

!f2py intent(in, out) TC

      data pattn2/3.606,-5.172,2.317,.1742,26370,-32460,11050,1012/
      data pattn3/15.87,-20.3,7.468,-.1479,43210,-53230,18120,776/
      data pattn4/14.64,-18.72,6.94,-.2583,44900,-55250,18820,712/
      data pattn5/9.408,-12.37,4.775,-.2331,38840,-47880,16300,859/

      Xprp = MgGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xalm = FeGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xsps = MnGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)
      Xgrs = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet) - 0.5*Fe3Garnet
      Xand = 0.5*Fe3Garnet
      XCa = CaGarnet/(FeGarnet+MgGarnet+MnGarnet+CaGarnet)

      XFEP = FeCpx
      XMGP = MgCpx
      TEMP=(Xalm/Xprp)*(XMGP/XFEP)
      iError = 0
      if (temp.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      Ellis and Green (1979) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Ellis and Green (1979)'
      TK = (3104*Xgrs+3030+10.86*Pbars/1000)/(dlog(TEMP)+1.9034)
      TC = TK-273.15
      ENDIF
! -------------------------------------------------------
!      Powell (1985) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      calname='Powell (1985) Calibration'
      TK = (3140*Xgrs+2790+10.00*Pbars/1000)/(dlog(TEMP)+1.7350)
      TC = TK-273.15
      ENDIF
! -------------------------------------------------------
!      Pattison and Newton (1989) Calibration
! -------------------------------------------------------
      IF (ICALIB.EQ.3) THEN
      calname='Pattison and Newton (1989) Calibration'
      xmg=Xprp/(Xprp+Xalm)
      ptt2=0
      ptb2=0
      ptt3=0
      ptb3=0
      ptt4=0
      ptb4=0
      ptt5=0
      ptb5=0
      do 5635 j=1,4
      ptb2=ptb2+pattn2(j)*(xmg**(4-j))
      ptt2=ptt2+pattn2(4+j)*(xmg**(4-j))
      ptb3=ptb3+pattn3(j)*(xmg**(4-j))
      ptt3=ptt3+pattn3(4+j)*(xmg**(4-j))
      ptb4=ptb4+pattn4(j)*(xmg**(4-j))
      ptt4=ptt4+pattn4(4+j)*(xmg**(4-j))
      ptb5=ptb5+pattn5(j)*(xmg**(4-j))
      ptt5=ptt5+pattn5(4+j)*(xmg**(4-j))
5635      continue

      t2=ptt2/(ptb2+dlog(temp))
      t3=ptt3/(ptb3+dlog(temp))
      t4=ptt4/(ptb4+dlog(temp))
      t5=ptt5/(ptb5+dlog(temp))
      if (xca.le.0.2) TK=t2
      if(xca.gt.0.2.and.xca.le.0.3) TK=((xca-0.2)/0.1)*t3+((0.3-xca)/0.1)*t2
      if(xca.gt.0.3.and.xca.le.0.4) TK=((xca-0.3)/0.1)*t4+((0.4-xca)/0.1)*t3
      if(xca.gt.0.4.and.xca.le.0.5) TK=((xca-0.4)/0.1)*t5+((0.5-xca)/0.1)*t4
      if(xca.ge.0.5) TK=t5

!      if (ical2.eq.2) then
!      TK=(-4125-16793*xca+12742*(Xprp-Xalm)+
!      & 3033*(2*xmgp*(xfep**2)-xmgp**2+2*xfep*(xmgp**2))+
!      & 11243.*(xfep**2-2*xmgp*(xfep**2)-2*xfep*(xmgp**2)))/
!      & (-1.6258-8.3144*dlog(temp))
!      endif
!      if (ical2.eq.3) then
!
!      WCAFE=coeff3
!      coeff3=(2*Xalm*(Xgrs**2)+Xprp*Xgrs*Xalm+Xalm*Xgrs/2+Xprp*Xgrs/2
!      & -Xgrs*Xalm*(Xgrs-Xalm))
!
!      WFECA=coeff4
!
!      coeff4=(Xgrs**2-2*Xalm*(Xgrs**2)+Xprp*Xgrs/2-Xprp*Xgrs*Xalm+
!      & Xalm*Xgrs/2+Xgrs*Xalm*(Xgrs-Xalm)+Xsps*Xgrs/2-Xalm*Xsps*Xgrs)
!      TK=(-479.3+12030.5*(Xprp-Xalm)-33418.6*coeff3-9767.7*coeff4+
!      & 1910.7*Xgrs+
!      & 6112.6*(2*xmgp*(xfep**2)-xmgp**2+2*xfep*(xmgp**2))+
!      & 9124.3*(xfep**2-2*xmgp*(xfep**2)-2*xfep*(xmgp**2)))/
!      & (-0.30864-8.3144*dlog(temp))
!      endif
      TC = TK - 273.15 + 5.5*((Pbars/1000)-15.)
      ENDIF
      END


!================================================================
!      Hornblende-Plagioclase from Holland and Blundy (1994)
!================================================================
      Subroutine HbldPlag_Na_Ca(&
      SiHbl,AlHbl,TiHbl,Fe3Hbl,MgHbl,FeHbl,MnHbl,CaHbl,NaHbl,KHbl,&
      NaPlag,CaPlag,KPlag,&
      Pbars,TC,iCalib,iError)

      implicit none
      integer*4 iCalib,iError
      character*256 calname
      real*8 NaPlag,CaPlag,KPlag,Xan,Xab
      real*8 SiHbl,AlHbl,Fe3Hbl,TiHbl,MgHbl,FeHbl,MnHbl,CaHbl,NaHbl,KHbl
      real*8 XSiT1,XAlT1,XAlM2,SUM1,XCaM4,XNaM4,XNaAlk,XKAlk,XVacAlk
      real*8 H,S,V,A1,A2,A5,B1,B3,B5,B8,YAmph,YAmph1,YPl,EQcon,temp1,temp2
      real*8 Pbars,TC,TK

!f2py intent(in, out) TC

      XSiT1=(SiHbl-4.0)/4.0
      XAlT1=(8.0-AlHbl)/4.0
      XAlM2=(AlHbl+SiHbl-8.0)/2.0
      SUM1 = SiHbl + AlHbl + TiHbl + Fe3Hbl + MgHbl + FeHbl + MnHbl + CaHbl
      XCaM4 = CaHbl/2.0
      XNaM4 = (15.0-SUM1)/2.0
      XNaAlk = SUM1 + NaHbl-15.0
      XKAlk = KHbl
      XVacAlk = 1.0-XKAlk-XNaAlk

      XAn = CaPlag/(CaPlag+NaPlag+KPlag)
      XAb = NaPlag/(CaPlag+NaPlag+KPlag)
      TEMP1=(27.*XVacAlk*XSiT1*XAb)/(256.*XNaAlk*XAlT1)
      TEMP2=(27.*XNaM4*XSiT1*XAn)/(64.*XCaM4*XAlT1*XAb)
      iError = 0
      if (temp1.le.0.or.temp2.le.0)then
      iError = 1
      return
      endif

! -------------------------------------------------------
!      Holland + Blundy (1994) Calibration - this one uses the alkali site concentration
! -------------------------------------------------------
      IF (ICALIB.EQ.1) THEN
      calname='Holland+Blundy(1994) hornblende-plagioclase alkali site'
      H=-76950
      S=-65.0
      V=0.79
      A1=39400
      A2=22400
      YAmph1=XNaAlk*A1+XKAlk*A2
      if(XAb.ge.0.5) YPl=0.0
      if(XAb.lt.0.5) YPl=12000.*(1.0-XAb)**2-3000.0
      EQCON=dlog(27.*XVacAlk*XSiT1*XAb/&
      (256.*XNaAlk*XAlT1))
      A5=41500-Pbars*2.89
      YAmph=YAmph1+XAlM2*A5
      TK = (H+V*Pbars+YPl+YAmph)/(S-8.3144*EQCON)
      TC = TK -273.15
      endif

! -------------------------------------------------------
!      Holland + Blundy (1994) Calibration - this one uses Ca-Na in M4 concentration
! -------------------------------------------------------
      IF (ICALIB.EQ.2) THEN
      calname='Holland+Blundy(1994) hornblende-plagioclase Ca-Na in M4'
      H=78440
      S=72.1
      B1=9400.
      B3=-33600.
      B8=78500.
      YAmph1=XNaAlk*B1+XNaM4*B3+XAlT1*B8
      if(XAb.ge.0.5) YPl=3000.
      if(XAb.lt.0.5) YPl=12000.*(2.*XAb-1)+3000.0
      EQCON=dlog(27.*XNaM4*XSiT1*XAn/&
      (64.*XCaM4*XAlT1*XAb))
      B5=-66800+Pbars*2.92
      YAmph=YAmph1+XAlM2*B5
      TK = (H+YPl+YAmph)/(S-8.3144*EQCON)
      TC = TK -273.15
      ENDIF
      END


!================================================================
!      Muscovite-biotite Mg-tschermak exchange thermometer
!================================================================
      Subroutine MuscBiot_Tschermak(&
      SiBiotite,AlBiotite,TiBiotite,Fe3Biotite,MgBiotite,FeBiotite,MnBiotite,Kbiotite,&
      SiMuscovite,AlMuscovite,TiMuscovite,Fe3Muscovite,MgMuscovite,FeMuscovite,MnMuscovite,KMuscovite,&
      Pbars,TC,iCalib,iError)

      implicit none

      integer*4 iCalib,iError
      character*256 calname
      real*8 SiBiotite,AlBiotite,TiBiotite,Fe3Biotite,MgBiotite,FeBiotite,MnBiotite,KBiotite
      real*8 SiMuscovite,AlMuscovite,TiMuscovite,Fe3Muscovite,MgMuscovite,FeMuscovite,MnMuscovite,KMuscovite
      real*8 Xphl,Xann,Xeast,XTibi,XMg,XFe2,XTi,XAl6,XK,XMgms,Xms,XKms,XAl6ms,Xcel
      real*8 Pbars,TC,TK,KdT

!f2py intent(in, out) TC
      
      IF(SiBiotite.LE.4.0)then      !normalized to 11
      XMg = MgBiotite/3.
      XFe2 = (FeBiotite + Fe3Biotite*0.87)/3.
      XTi = TiBiotite/3.
      XAl6 = (SiBiotite + AlBiotite -4.)/3.
      XK = KBiotite/1.
      else      !normalized to 22
      XMg = MgBiotite/6.
      XFe2 = (FeBiotite + Fe3Biotite*0.87)/6.
      XTi = TiBiotite/6.
      XAl6 = (SiBiotite + AlBiotite -4.)/6.
      XK = KBiotite/2.
      endif
      
      Xphl = XK*XMg**3
      Xann = XK*XFe2**3
      Xeast = 6.75*XK*XMg**2*XAl6
      XTibi = XK*XTi**3

      IF(SiMuscovite.LE.4.0)then      !normalized to 11 
      XAl6ms = (SiMuscovite + AlMuscovite-4.)/2.
      XKms = KMuscovite/1.
      XMgms = MgMuscovite/2.
      else
      XAl6ms = (SiMuscovite + AlMuscovite-4.)/4.
      XKms = KMuscovite/2.
      XMgms = MgMuscovite/4.
      endif      

      Xms = XKms*XAl6ms**2
      Xcel = 4.*XKms*XMgms*XAl6ms

      iError = 0 
      if(Xeast.le.0)then
      iError = 2
      return
      endif

! -----------------------------------------------------------
!      Wu, Pan and Wang (ms)
! -----------------------------------------------------------
      IF(ICALIB.EQ.2) THEN
      calname='Wu,Pan&Wang, 2000 ms'
      KdT = (Xeast/Xphl)*(Xcel/Xms)

      TK = (803.6709+0.009301*Pbars+159.5364*(Xphl-Xeast)-130.566*Xann+335075.2*XTibi - 16.977*Xms - 26.08148*Xcel*(Xcel-2.))/&
      (1.+0.001394*8.3144*dlog(KdT))

      TC =TK - 273.15
      endif      !end Wu,Pan and Wang calibration

      END


!================================================================
!      Garnet-Plagioclase-Aluminosilicate-Quartz barometer
!================================================================


!================================================================
!      Garnet-Plagioclase-Hornblende-Quartz barometer
!================================================================


!================================================================
!      Garnet Solution model
!================================================================
      SUBROUTINE GS (TK,P,Xpy,Xalm,Xsp,Xgr,Ggar,Imole,imod)
      implicit real*8 (x,w,c,r)
      integer*4 imole,imod
      real*8 TK,P,Xpy,Xalm,Xsp,Xgr,Ggar

!f2py intent(in, out) Ggar

      if (imole.eq.1) then
      x1=xpy
      x2=xalm
      x3=xsp
      x4=xgr
      W12=2500
      W21=200
      W13=3000
      W31=3000
      if (imod.eq.2) then
      w13=2500
      w31=2500
      endif
      W14=1000-1.5*TK
      W41=4047-1.5*TK
      W23=0
      W32=0
      W24=4620-1.5*TK
      W42=-630-1.5*TK
      if (imod.eq.3) then
      w24=23801/4.184-11.36*TK/4.184
      w42=6240/4.184-11.36*TK/4.184
      endif
      W34=0
      W43=0
      if(imod.eq.4) then
      w12=(3720+.06*p)/(4.184*3)
      w21=(230 +.01*p)/(4.184*3)
      w13=0
      w31=0
      w14=(21560-18.79*TK+.1*p)/(4.184*3)
      w41=(69200-18.79*TK+.1*p)/(4.184*3)
      w23=0
      w32=0
      w24=(20320-5.08*TK+.17*p)/(4.184*3)
      w42=(2620 -5.08*TK+.09*p)/(4.184*3)
      w34=0
      w43=0
      endif
      if(imod.eq.5) then
      w12=(22265 -12.4*TK +.05*p)/(4.184*3)
      w21=(-24166 +22.09*TK -.034*p)/(4.184*3)
      w13=(30345- 15.6*TK)/(4.184*3)
      w31=(30345- 15.6*TK)/(4.184*3)
      w14=(14306-2.49*TK+.14*p)/(4.184*3)
      w41=(65182-20.82*TK+.068*p)/(4.184*3)
      w23=1860/(4.184*3)
      w32=1860/(4.184*3)
      w24=(17526 -14.51*TK+.135*p)/(4.184*3)
      w42=(-18113 +15.51*TK+.04*p)/(4.184*3)
      w34=1425/(4.184*3)
      w43=1425/(4.184*3)
      endif
      endif
      if (imole.eq.2) then
      x1=xalm
      x2=xsp
      x3=xgr
      x4=xpy
      W12=0
      W21=0
      W13=4620-1.5*TK
      W31=-630-1.5*TK
      if (imod.eq.3) then
      w13=23801/4.184-11.36*TK/4.184
      w31=6240/4.184-11.36*TK/4.184
      endif
      W14=200
      W41=2500
      W23=0
      W32=0
      W24=3000
      W42=3000
      if (imod.eq.2) then
      w24=2500
      w42=2500
      endif
      W34=4047-1.5*TK
      W43=1000-1.5*TK
      if(imod.eq.4) then
      w41=(3720+.06*p)/(4.184*3)
      w14=(230 +.01*p)/(4.184*3)
      w42=0
      w24=0
      w43=(21560-18.79*TK+.1*p)/(4.184*3)
      w34=(69200-18.79*TK+.1*p)/(4.184*3)
      w12=0
      w21=0
      w13=(20320-5.08*TK+.17*p)/(4.184*3)
      w31=(2620 -5.08*TK+.09*p)/(4.184*3)
      w23=0
      w32=0
      endif
      if(imod.eq.5) then
      w41=(22265 -12.4*TK +.05*p)/(4.184*3)
      w14=(-24166 +22.09*TK -.034*p)/(4.184*3)
      w42=(30345- 15.6*TK)/(4.184*3)
      w24=(30345- 15.6*TK)/(4.184*3)
      w43=(14306-2.49*TK+.14*p)/(4.184*3)
      w34=(65182-20.82*TK+.068*p)/(4.184*3)
      w12=1860/(4.184*3)
      w21=1860/(4.184*3)
      w13=(17526 -14.51*TK+.135*p)/(4.184*3)
      w31=(-18113 +15.51*TK+.04*p)/(4.184*3)
      w23=1425/(4.184*3)
      w32=1425/(4.184*3)
      endif
      endif
      if (imole.eq.3) then
      x1=xsp
      x2=xgr
      x3=xpy
      x4=xalm
      W12=0
      W21=0
      W13=3000
      W31=3000
      if (imod.eq.2) then
      w13=2500
      w31=2500
      endif
      W14=0
      W41=0
      W23=4047-1.5*TK
      W32=1000-1.5*TK
      W24=-630-1.5*TK
      W42=4620-1.5*TK
      if (imod.eq.3) then
      w42=23801/4.184-11.36*TK/4.184
      w24=6240/4.184-11.36*TK/4.184
      endif
      W34=2500
      W43=200
      if(imod.eq.4) then
      w34=(3720+.06*p)/(4.184*3)
      w43=(230 +.01*p)/(4.184*3)
      w31=0
      w13=0
      w32=(21560-18.79*TK+.1*p)/(4.184*3)
      w23=(69200-18.79*TK+.1*p)/(4.184*3)
      w41=0
      w14=0
      w42=(20320-5.08*TK+.17*p)/(4.184*3)
      w24=(2620 -5.08*TK+.09*p)/(4.184*3)
      w12=0
      w21=0
      endif
      if(imod.eq.5) then
      w34=(22265 -12.4*TK +.05*p)/(4.184*3)
      w43=(-24166 +22.09*TK -.034*p)/(4.184*3)
      w31=(30345- 15.6*TK)/(4.184*3)
      w13=(30345- 15.6*TK)/(4.184*3)
      w32=(14306-2.49*TK+.14*p)/(4.184*3)
      w23=(65182-20.82*TK+.068*p)/(4.184*3)
      w41=1860/(4.184*3)
      w14=1860/(4.184*3)
      w42=(17526 -14.51*TK+.135*p)/(4.184*3)
      w24=(-18113 +15.51*TK+.04*p)/(4.184*3)
      w12=1425/(4.184*3)
      w21=1425/(4.184*3)
      endif
      endif
      if (imole.eq.4) then
      x1=xgr
      x2=xpy
      x3=xalm
      x4=xsp
      W12=4047-1.5*TK
      W21=1000-1.5*TK
      W13=-630-1.5*TK
      W31=4620-1.5*TK
      if (imod.eq.3) then
      w31=23801/4.184-11.36*TK/4.184
      w13=6240/4.184-11.36*TK/4.184
      endif
      W14=0
      W41=0
      W23=2500
      W32=200
      W24=3000
      W42=3000
      if (imod.eq.2) then
      w24=2500
      w42=2500
      endif
      W34=0
      W43=0
      if(imod.eq.4) then
      w23=(3720+.06*p)/(4.184*3)
      w32=(230 +.01*p)/(4.184*3)
      w24=0
      w42=0
      w21=(21560-18.79*TK+.1*p)/(4.184*3)
      w12=(69200-18.79*TK+.1*p)/(4.184*3)
      w34=0
      w43=0
      w31=(20320-5.08*TK+.17*p)/(4.184*3)
      w13=(2620 -5.08*TK+.09*p)/(4.184*3)
      w41=0
      w14=0
      endif
      if(imod.eq.5) then
      w23=(22265 -12.4*TK +.05*p)/(4.184*3)
      w32=(-24166 +22.09*TK -.034*p)/(4.184*3)
      w24=(30345- 15.6*TK)/(4.184*3)
      w42=(30345- 15.6*TK)/(4.184*3)
      w21=(14306-2.49*TK+.14*p)/(4.184*3)
      w12=(65182-20.82*TK+.068*p)/(4.184*3)
      w34=1860/(4.184*3)
      w43=1860/(4.184*3)
      w31=(17526 -14.51*TK+.135*p)/(4.184*3)
      w13=(-18113 +15.51*TK+.04*p)/(4.184*3)
      w41=1425/(4.184*3)
      w14=1425/(4.184*3)
      endif
      endif

      C123=0.5*(W21-W12+W13-W31+W32-W23)
      C124=0.5*(W21-W12+W14-W41+W42-W24)
      C134=0.5*(W31-W13+W14-W41+W43-W34)
      C234=0.5*(W32-W23+W24-W42+W43-W34)
      if(imod.eq.4) then
      C123=0.
      C124=0.
      C134=0.
      C234=0.
      endif
      if(imod.eq.5) then
      if(imole.eq.1)C124=C124-7110/(4.184*3)
      if(imole.eq.2)C124=C134-7110/(4.184*3)
      if(imole.eq.3)C124=C234-7110/(4.184*3)
      if(imole.eq.4)C123=C123-7110/(4.184*3)
      endif
      RTLG1=(X2**2)*(W12+2*X1*(W21-W12))+(X3**2)*(W13+2*X1*&
      (W31-W13))+(X4**2)*(W14+2*X1*(W41-W14))-X2*X3*(1-2*X1)*&
      C123-X4*X2*(1-2*X1)*C124-X4*X3*(1-2*X1)*C134+X2*X3*0.5*&
      (W21+W12+W31+W13-W23-W32)+X4*X2*0.5*(W41+W14+W21+W12-&
      W24-W42)+X4*X3*0.5*(W41+W14+W31+W13-W34-W43)+&
      X1*X2*X3*(W21-W12+W31-W13)+X1*X2*X4*(W41-W14+W21-W12)+&
      X1*X3*X4*(W41-W14+W31-W13)+X2*X3*(X2-X3)*(W23-W32)+&
      X4*X2*(X2-X4)*(W24-W42)+X4*X3*(X3-X4)*(W34-W43)+&
      2*X2*X3*X4*(C234+W42-W24)
      if(imod.eq.4)RTLG1=RTLG1-2*x2*x3*x4*(w42-w24)
      Ggar=EXP(RTLG1/(1.987*TK))
      RETURN
      END
