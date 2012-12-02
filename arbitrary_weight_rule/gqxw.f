c     Abscissas and weights of Gaussian Quadrature
c     produced by Mathematica code: AWGQ
cccccc
c     for int_a^b{f(z(x))W(x)dx}
c     argument    z(x) = x
c     weight      W(x) = 1/(0.5 + x)
c     lower limit a = 0
c     upper limit b = 1
c     GQ order n<=200
cccccc
      subroutine gqxw(x,w,n)
c     x: abscissas
c     w: weights
      implicit real*8(a-h,o-z)
      dimension x(200),w(200)
c
      if(n.eq.4) then
        x(1)=6.0374746877431588D-2
        w(1)=2.7420521378251518D-1
        x(2)=3.0224715711266843D-1
        w(2)=3.9347814617278917D-1
        x(3)=6.4536186039717539D-1
        w(3)=2.9771836550998488D-1
        x(4)=9.2391363736933210D-1
        w(4)=1.3321056320282046D-1
        return
      end if
c
      stop 'gqxw.f, n=', n
      end
