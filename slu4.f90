subroutine slu4(y1,y,x,x1)
    real(8), dimension(4) :: y,x
    real(8) f10,f8,f9,y1,x1,f1,f2,f3,f4,f5,f6,f7,c0,c1,c2,c3,c4,c5,c6,c7,a0,a1,a2,a3
    c0=y(1)
    c1=(y(2)-c0)/(x(2)-x(1))
    f1=(x(1)**2.-x(2)**2.)/(x(2)-x(1))
    f2=(x(1)**3.-x(2)**3.)/(x(2)-x(1))
    c2=c0-c1*x(1)
    f3=(f1*x(1)+x(1)**2)
    f10=x(1)**3.+f2*x(1)
    c3=c2+c1*x(3)
    f4=(f1*x(3)+x(3)**2.-f3)
    f5=f2*x(3)-f10+x(3)**3.
    c4=(y(3)-c3)/f4
    f6=f5/f4
    c5=c2-f3*c4
    f7=f3*f6-f10
    c6=c1+f1*c4
    f8=f2-f1*f6
    c7=c5+c6*x(4)+c4*x(4)**2.
    f9=f7+f8*x(4)-f6*x(4)**2.+x(4)**3.
    a3=(y(4)-c7)/f9
    a2=c4-f6*a3
    a1=c6+f8*a3
    a0=c5+f7*a3
    y1=a0+a1*x1+a2*x1**2.+a3*x1**3.
    return
end




