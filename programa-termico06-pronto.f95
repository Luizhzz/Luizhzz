

PROGRAM teste_matriz  !nome do programa.

!----------INICIO INTERFACE----------------!
INTERFACE							!obrigatorio interface para passar uma matriz da subrotina para o programa.
  SUBROUTINE digita_matriz (matriz_A,m,n,K_sub,L_sub,T_A,T_CHUT )			!nome da subrotina
    INTEGER :: m, n						! intent(in) --> dados de entrada
    real*8 ::K_sub,L_sub,T_A,T_CHUT
    REAL*4, ALLOCATABLE :: matriz_A(:,:)  ! intent(out) --> dados de saida
  END SUBROUTINE digita_matriz
END INTERFACE
!----------FIM INTERFACE----------------!

!---- inicio declaração de variáveis------!

real*8 ::K,L,Temp_A,Temp_B,T_CHUT
REAL*8:: IT_ALTO,IT_BAIXO,ERRO
INTEGER :: m, n	
REAL*4, ALLOCATABLE :: matriz_A(:,:)  !matriz de tamanho variavel com dados tipo real*4

!----FIM declaração de variáveis------!

print*, "DIGITE O COMPRIMENTO DA PECA(m) :"
read(*,*) COMPRIMENTO
print*, "temperatura na extremidade T(a):"
read(*,*) TEMP_A
print*, "temperatura na outra extremidade T(b):"
read(*,*) TEMP_B
print*, "Condutibilidade termica do material:"
read(*,*) CONDUT_TERM
print*, "quantidadde de divisões:"
read(*,*) TAMANHO_MALHA

m = TAMANHO_MALHA
n = 1 
K= CONDUT_TERM
L= COMPRIMENTO
Temp_A= TEMP_A
Temp_B= TEMP_B


ALLOCATE (matriz_A(m+2, n))		!separando na memoria local com tamanho correto para caber a matriz.

T_CHUT=Temp_A
! print*, m,n,k,l,Temp_A,T_CHUT
CALL digita_matriz(matriz_A, m, n,K,L,Temp_A,T_CHUT)	!comando para executar a subrotina de nome digita_matriz 

i=1
  DO j = 1, m+2     ! Imprime cada linha da matriz em linhas separadas na tela separados por ";"
      WRITE(*, '(F12.6, A)', ADVANCE='NO') matriz_A(j, i), '; '
	  END DO
	  WRITE(*,*)


T_CHUT=Temp_B
!print*, m,n,k,l,Temp_A,T_CHUT
CALL digita_matriz(matriz_A, m, n,K,L,Temp_A,T_CHUT)	!comando para executar a subrotina d
i=1  
  DO j = 1, m+2     ! Imprime cada linha da matriz em linhas separadas na tela separados por ";"
      WRITE(*, '(F10.4, A)', ADVANCE='NO') matriz_A(j, i), '; '
	  END DO
    WRITE(*,*)

IT_BAIXO= Temp_A
IT_ALTO= Temp_B
!	T_CHUT = (IT_ALTO + IT_BAIXO)/2
	ERRO = (IT_ALTO + IT_BAIXO)/IT_ALTO
	
	print*, "erro de" ,ERRO
DO WHILE (ERRO.GT.0.000000000001)
T_CHUT = (IT_ALTO + IT_BAIXO)/2
ERRO=(IT_ALTO - IT_BAIXO)/ IT_ALTO
ERRO = ABS(ERRO)
PRINT*,"novo erro" ,ERRO
!print*, m,n,k,l,Temp_A,T_CHUT
CALL digita_matriz(matriz_A, m, n,K,L,Temp_A,T_CHUT)	!comando para executar a subrotina d
i=1  
  DO j = 1, m+2     ! Imprime cada linha da matriz em linhas separadas na tela separados por ";"
      WRITE(*, '(F14.6, A)', ADVANCE='NO') matriz_A(j, i), '; '
	  END DO
    WRITE(*,*)

IF (matriz_A(m+2,1).GE.TEMP_B) THEN
		IT_ALTO= T_CHUT
		PRINT*, "rotina72", IT_ALTO
		ELSE
		IT_BAIXO=T_CHUT
		PRINT*,"rotina74", IT_BAIXO
END IF

END DO

ERRO=(IT_ALTO - IT_BAIXO)/ IT_ALTO
ERRO = ABS(ERRO)
PRINT*, "O erro do calculo foi de:" ,ERRO
END PROGRAM teste_matriz	! fim do programa principal.


!------------------------------------------------------!
SUBROUTINE digita_matriz(matriz_A, m, n, K_sub,L_sub,T_A, T_CHUT)	

 IMPLICIT NONE
!---- inicio declaração de variáveis locais ------!
REAL*8 ::K_sub,L_sub,T_A,T_CHUT
REAL*4 ::A_se, A_sw, A_e, A_w, A_p
 INTEGER :: m, n, i, j
 REAL*4, ALLOCATABLE :: matriz_A(:,:)
!---- FIM declaração de variáveis locais ------! 

A_se = K_sub/L_sub/2		!meia distancia apra calcular a temperatura na superficie
A_sw = K_sub/L_sub/2		!meia distancia apra calcular a temperatura na superficie
A_e = K_sub/L_sub			!coeficiente á direita
A_w = K_sub/L_sub			!!coeficiente á esquerda
A_p = A_e + A_w		!coeficiente no ponto

j = n 

!print*, m,n,K_sub,L_sub,T_A,T_CHUT

do i=1,1
matriz_A(i,j)= T_A 		!temperatura lado esquerdo. parede Temp_A
!print*, matriz_A(1,1)
end do

do i=2,2
matriz_A(i,j)= T_CHUT		!primeira estimativa de temperatura (chute da [T1])
!print*, matriz_A(2,1)
end do

do i=3,3
matriz_A(i,j)= (A_p*matriz_A(i-1,j)-A_sw*matriz_A(i-2,j))/A_e  !calculo da T2
!print*, matriz_A(3,1)
end do

do i=4,m+2
matriz_A(i,j)= (A_p*matriz_A(i-1,j)-A_e*matriz_A(i-2,j))/A_e   !resultado chutado da T_B 
!print*, matriz_A(i,1)
end do
          
          
RETURN

END SUBROUTINE digita_matriz

SUBROUTINE matriz_pela_esquerda (matriz_A, m, n, K_sub,L_sub,T_B, T_CHUT)

 IMPLICIT NONE
!---- inicio declaração de variáveis locais ------!
REAL*8 ::K_sub,L_sub,T_B,T_CHUT
REAL*4 ::A_se, A_sw, A_e, A_w, A_p
 INTEGER :: m, n, i, j, x
 REAL*4, ALLOCATABLE :: matriz_A(:,:)
!---- FIM declaração de variáveis locais ------! 

x=m
A_se = K_sub/(L_sub/2)		!meia distancia apra calcular a temperatura na superficie
A_sw = K_sub/(L_sub/2)		!meia distancia apra calcular a temperatura na superficie
A_e = K_sub/L_sub			!coeficiente á direita
A_w = K_sub/L_sub			!!coeficiente á esquerda
A_p = A_e + A_w		!coeficiente no ponto

j = n 

!print*, m,n,K_sub,L_sub,T_A,T_CHUT

do i=m+2,m+2
matriz_A(i,j)= T_B		!temperatura lado esquerdo. parede Temp_A
print*,"posicao",m+2 , matriz_A(7,1)
end do

do i=m+1,m+1
matriz_A(i,j)= T_CHUT		!primeira estimativa de temperatura (chute da [T1])
print*, "posicao",m+1 ,matriz_A(6,1)
end do

do i=m,m
matriz_A(i,j)=((A_e+A_se)*matriz_A(m+1,j)-A_se*matriz_A(m+2,j))/A_w  !calculo da T2
 print*, "posicao",m ,matriz_A(5,1)
 end do

i=m-1
do while (i.GT.1)
matriz_A(i,j)=(A_p*matriz_A(m,j)-A_e*matriz_A(m+1,j))/A_e !calculo da T2
print*, "posicao",m-1 ,matriz_A(m-1,1)
 m=m-1
  i=i-1
end do

do i=1,1

matriz_A(i,j)=((A_se+A_e)*matriz_A(m,j)-A_e*matriz_A(m+1,j))/(A_se) !calculo da T2

print*, "posicao",m-1 ,matriz_A(1,1)
 end do
 
m=x

STOP


RETURN
END SUBROTINE matriz_pela_esquerda