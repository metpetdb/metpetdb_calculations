C File GTBbarebones.f
      SUBROUTINE GARBIO(FeMgGarnet,FeMgBiotite,Pbars,TC)
      implicit none
      real*8 FeMgGarnet,FeMgBiotite,Pbars,TC,TK,EQcon,H,V,S,temp
      temp = ((1-FeMgGarnet)/FeMgGarnet)/((1-FeMgBiotite)/FeMgBiotite)
!f2py intent(in, out) TC
      V=.057
      S=4.662
      H=12454.
      EQCON=3*DLOG(TEMP)
      TK=(H+Pbars*V)/(S-1.987*EQCON)
      TC=TK-273.15
      END
