sUBROUTINE PRIV(GIN,HOUT,OGR,PN,HIN,PIN,POUT,ZAT,PZAT,PK,PECH,HOM)
    REAL(8), dimension(1) :: ZAT,PZAT
    REAL(8), dimension(3) :: OGR, po
    REAL(8), dimension(5) :: pk
    REAL(8) :: HOM,PECH,PN,PIN,HIN,POUT,PECH1,HOUT,PN1,GIN
    INTEGER :: I

    10 FORMAT(/5X,'PRIV ',I3,25X,'TYPÅàHA èàTATEãúHOÉO HACOCA' &
        //33X,'BXOÑHõE èAPAMETPõ'//5X,'    PN=',F16.8, &
        2X,'   PIN=',F16.8,2X,'   HIN=',F16.8,2X,'  POUT=', &
        F16.8/5X,'  PECH=',F16.8,2X,'   HOM',F16.8// &
        5X,'PK: 1)QCYX =',F16.8,8X,'ZAT(1)=QBL  =',F16.8/10X, &
        '2)QMEX =',F16.8,7X,'PZAT(1)=POGBL=',F16.8/10X, &
        '3)BLMAX=',F16.8/10X,'4)TIP  =',F16.8/10X, &
        '5)A    =',F16.8/)
    11 FORMAT(//,33X,'BõXOÑHõE èAPAMETPõ',//,10X, &
        'HOUT=',F16.8,5X,'GIN=',F16.8,/ &
        10X,'OGR:1)PIN-POUT=',F16.8,/ &
        15X,'2)GIN=',F16.8,/15X,'3)BLMAX-BL2=',F16.8)
    12 FORMAT(//'èAPAMETPõ èOÑèPOÉPAMMõ OTSK BHYTPà èOÑèPOÉPAMMõ PRIV'/)
    i=int(hom+0.1)
    if (pech.eq.0.) goto 1
    write(*,10)i,pn,pin,hin,pout,pech,hom,pk(1),zat(1),pk(2),pzat(1),pk(3),pk(4),pk(5)
    1 pech1=0.
    if ((pech.eq.2.).or.(pech.eq.102.)) goto 3
    2 call otsk(hout,pn1,po,ogr,hin,pin,1.0,pout,zat,pzat,pk,1.0,pech1,hom)
    gin=pn/pn1
    ogr(2)=gin
    if (pech.eq.0.) goto 4
    write(*,11)hout,gin,ogr
    goto 4
    3 pech1=pech
    write(*,12)
    goto 2
    4 continue
    return
end

subroutine rass(gpout,hpout,gwout,ppout,hwout,ogr,gin,hin,pwout,pech,hom)
    real(8),dimension(1) :: ogr(1)
    real(8) :: hom,pech,gin,hin,pwout,ppout,hwout,hpout,gpout,gwout
    integer :: i
    10 FORMAT(/5X,'RASS ',I3,25X,'PACòàPàTEãú PEHAÜA'//33X,'BXOÑHõE èAPAMETPõ'// &
        5X,'GIN=',F16.8,2X,'HIN=',F16.8,2X, &
        'PWOUT=',F16.8/)
    20 FORMAT(/33X,'BõXOÑHõE èAPAMETPõ'// &
        5X,'GPOUT=',F16.8,2X,'HPOUT=',F16.8,2X, &
        'GWOUT=',F16.8,2X,'PPOUT=',F16.8,2X,'HWOUT=',F16.8// &
        40X,'OÉPAHàóEHàE'//40X,'OGR(1)=HIN-HPOUT=',F16.8/)
    i=int(hom+0.1)
    if (pech.eq.0.) goto 1
    write(*,10)i,gin,hin,pwout
    1 continue
    ppout=pwout
    call ipn1(hwout,pwout)
    call ipn2(hpout,ppout)
    gpout=gin*(hin-hwout)/(hpout-hwout)
    gwout=gin-gpout
    ogr(1)=hin-hpout
    if (pech.eq.0.) goto 2
    write(*,20)gpout,hpout,gwout,ppout,hwout,ogr(1)
    2 continue
    return
end

subroutine nask(hwout,pn,hwin,pwin,pwout,gwin,pk,pech,hom)
    real(8), dimension(2) :: pk
    real(8) :: pech,hom,hwin,pwin,pwout,gwin,v,pn,hwout
    integer :: i
    10 FORMAT(/5X,'NASK ',I3,25X,'H A C O C'//&
        33X,'BXOÑHõE èAPAMETPõ'//&
        5X,'HWIN=',F16.8,2X,'PWIN=',F16.8,2X,&
        'PWOUT=',F16.8,2X,'GWIN=',F16.8/&
        5X,'PK(1)=KPDM=',F16.8,2X,'PK(2)=KPDOI=',&
        F16.8,2X,'PECH=',F16.8,2X,'HOM=',F16.8/)
    20 FORMAT(/33X,'BõXOÑHõE èAPAMETPõ'//5X,'HWOUT=',F16.8,2X,'PN=',F16.8/)
    if (pech.eq.0.) goto 1
    i=int(hom+0.1)
    write(*,10)i,hwin,pwin,pwout,gwin,pk,pech,hom
    1 call vpi(v,pwin,hwin)
    pn=v*gwin*(pwout-pwin)*98.1/pk(1)/pk(2)
    hwout=hwin+v*(pwout-pwin)*98.1/pk(1)/4.186
    if (pech.eq.0.) goto 2
    write(*,20)hwout,pn
    2 continue
    return
end

subroutine deaer(gpin,gwout,hwout,dp,ogr,gwin,&
        gkin,ppin,pwin,pkin,pwout,hpin,hwin,hkin,ake,hom,pech)
    real(8), dimension(3) :: ogr
    real(8) hom,hwout,pwout,gpin,gkin,hkin,gwin,hwin,ake,hpin,ppin,pkin,dp,pwin,gwout,pech
    integer :: i
    i=int(hom+0.1)
    call ipn1(hwout,pwout)
    gpin=(gkin*(hwout-hkin)+gwin*(hwout-hwin))/(ake*(hpin-hwout))
    ogr(1)=gpin
    ogr(2)=ppin-pwout
    ogr(3)=ppin-pkin
    dp=pwin-pwout
    gwout=gwin+gkin+gpin
    1 FORMAT(/5X,'DEAER ',I3,30X,'Ñ E A ù P A T O P'/&
        /30X,'BXOÑHõE èAPAMETPõ'/5X,'GWIN=',F16.8,5X,'GKIN=',F16.8,&
        5X,'PPIN=',F16.8,5X,'PWIN=',F16.8/5X,'PKIN=',F16.8,4X,&
        'PWOUT=',F16.8,5X,'HPIN=',F16.8,5X,'HWIN=',F16.8/&
        5X,'HKIN=',F16.8,4X,'AKE=',F16.8/)
    2 FORMAT(/30X,'BõXOÑHõE èAPAMETPõ'/5X,'GPIN=',F16.8,4X,&
        'HWOUT=',F16.8,&
        'GWOUT=',F16.8,7X,'DP=',F16.8//30X,'OÉPAHàóEHàü'/5X,&
        'OGR(1)=GPIN=',F16.8,3X,'OGR(2)=PRIN-PWOUT=',F16.8,3X,&
        'OGR(3)=PPIN-PKIN=',F16.8/)
    if (pech.eq.0.) goto 3
    write(*,1)i,gwin,gkin,ppin,pwin,pkin,pwout,hpin,hwin,hkin,ake
    write(*,2)gpin,hwout,gwout,dp,ogr
    3 continue
    return
end

subroutine privn(pnpr,pnkb,pngt)
    real(8) :: pnpr,pnkb,pngt
    pnpr=pnkb-pngt
    if (pnpr.le.0.) pnpr=0.3
    return
end


