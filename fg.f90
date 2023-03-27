subroutine fg(y1,x1,x,dx,n,y)
    integer :: n,k
    real(8), dimension(n) :: y
    real(8), dimension(4) :: z,f
    real(8) :: p,x1,dx,x,s,y1

    p=x1+dx*2.
    if (x.ge.p) goto 10
    z(1)=x1
    z(2)=x1+dx
    z(3)=x1+dx*2.
    z(4)=x1+dx*3.
    f(1)=y(1)
    f(2)=y(2)
    f(3)=y(3)
    f(4)=y(4)
    goto 30
    10 continue
    s=x1+dx*(n-3)
    if(x.le.s) goto 20
    z(4)=x1+dx*(n-1)
    z(3)=x1+dx*(n-2)
    z(2)=x1+dx*(n-3)
    z(1)=x1+dx*(n-4)
    f(1)=y(n-3)
    f(2)=y(n-2)
    f(3)=y(n-1)
    f(4)=y(n)
    goto 30
    20 continue
    k=int((x-x1)/dx)
    z(1)=x1+dx*(k-1)
    z(2)=z(1)+dx
    z(3)=z(2)+dx
    z(4)=z(3)+dx
    f(1)=y(k)
    f(2)=y(k+1)
    f(3)=y(k+2)
    f(4)=y(k+3)
    30 continue
    call slu4(y1,f,z,x)
    return
end
