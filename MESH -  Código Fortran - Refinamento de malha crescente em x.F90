program exemplo

!Declaração das variáveis a serem usadas no programa
implicit real*8 (a-h,o-z)
!common /geom/a,b,dy,dx             !Estas três linhas indicam as variáveis que serão comuns a todas subrotinas (se existirem)
!common /pontos/l1,l2,l3,m1,m2,m3
!common /dados/T1,T2
parameter (ni=1000,nj=1000)	       
dimension T(ni,nj),x(ni),y(nj) 

open (10,file='tecplot.dat')

!colocando as constantes do problema no programa
a=.1d0                    !comprimento da aleta
b=.05d0                    !largura da aleta
l1=102                    !a aleta sera dividida em (l1)-2 colunas
m1=52                    !a aleta sera dividida em (m1)-2 linhas
!T1=500d0                  !condicao de contorno
!T2=200d0                  !condicao de contorno

!cálculo da malha
l2=l1-1
l3=l1-2
m2=m1-1
m3=m1-2
!dx=(a/l3)            !malha uniforme - criando as colunas com o espacamento dx, malha igualmente espacada
dy=(b/m3)             !malha uniforme - criando as linhas com o espacamento dy, malha igualmente espacada
alp=3.0d0              !malha não-uniforme - tangente hiperbólica

x(1)=0
!x(2)=dx/2            !malha uniforme 
!x(l1)=a

!mais refinada em x=l1
do i=2,l1  !3,l2
	x(i)=a*((dtanh(alp*((i-2.d0)/(l1-2.d0))))/(dtanh(alp)))
enddo

!mais refinada em x=0 
!do i=2,l2  !3,l2
!	x(i)=x(l1)-(a*((dtanh(alp*((i-2.d0)/(l1-2.d0))))/(dtanh(alp))))
!enddo

y(1)=0
y(2)=dy/2
y(m1)=b
do j=3,m2
	y(j)=y(j-1)+dy
enddo
!fim malha

!cálculo dos coeficientes aw(i,j),ae(i,j),an(i,j),as(i,j),ap(i,j) 
!definição do campo inicial de temperaturas para solução 
!definição das temperaturas prescritas nas fronteiras
!cálculo das temperaturas no interior do domínio (usar como critério de convergência 1E-6, comparando qin e qout (ex.: erro=(qin-qout)/qin))

!!!!campo fictício de temperatura somente para demonstração em aula!!!!
do j=1,m1
  T(1,j)=47112184.558106D0*y(j)**6.D0-47581472.306763D0*y(j)**5.D0+16446382.403565D0*y(j)**4.D0-1771045.730858D0*y(j)**3.D0-168905.394280D0*y(j)**2.D0+40155.801959D0*y(j)+300.D0
  !T(1,j)=498.88112d0-908.15851d0*y(j)-2913.7529d0*y(j)**2
enddo

do i=2,l1
  do j=1,m1
    T(i,j)=(T(1,j)-300)*(1-3*((x(i)/a)**2)+2*((x(i)/a)**3))+300
	!T(i,j)=T(i-1,j)
  enddo
enddo
	 
!Plotagem no Tecplot
  WRITE(10,70)
  WRITE(10,80) L1,M1
  LA=L1+1
  DO J=1,M1
    DO II=L1,1,-1
       I=LA-II
	   WRITE(10,90) X(I),Y(J),T(I,J)
    ENDDO
  ENDDO

  70 FORMAT(1X,'Variables = X, Y, T')
  80 FORMAT(1X,'Zone T = "Zone-One", I=',I3,' J=',I3,' F=Point')
  90 FORMAT(2F10.5,F10.1,1P1E15.5)


close (10)

end



