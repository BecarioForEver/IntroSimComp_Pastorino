module mod1
use ziggurat
implicit none
integer :: iostat
integer, allocatable :: M(:,:)  
integer :: N, J, int_1, int_2, kB = 1, ntemp, w	!! Desde archivo
real(kind=8), dimension(:), allocatable :: T			!! Desde archivo

!Luego de 'contains' puedo definir ruitnas y funciones
contains        
	                 
	subroutine leer_param()
	integer :: line = 5
	real(kind=8) :: val
	
  		! Open the file for reading
  		open(unit=10, file='./Inputs/Param.dat', status='old', iostat=iostat)
    	! Check for file open errors
    	if (iostat /= 0) then
    		write(*, *) 'Error opening the file'
    		stop
    	end if
    	! Lee parametros 
    	do line = 1, 4
    		read(10, *) val
    		select case(line)
      			case(1)
        			N = val
      			case(2)
        			J = val
        		case(3)
        			int_1 = val
      			case(4)
        			int_2 = val
      		end select
  		end do
    	! Check for read errors
    	if (iostat /= 0) then
      		write(*, *) 'Error reading the number from the file'
        	stop
    	end if
    	! Close the file
    	close(10)
    	
!    	open (unit=10, file='./Inputs/Temp_short.dat', status='old', action='read')
    	open (unit=10, file='./Inputs/Temp.dat', status='old', action='read')
      	read(10, *) ntemp
      	allocate(T(ntemp))
      	read(10,*) T
      	write(*,*) T
      	close(10)
      	
      	allocate(M(0:N+1,0:N+1))
      	
	end subroutine

	subroutine Random_M()
	!! Rutina que inicializa matriz aleatoria
		integer :: row, col !locales
		real(kind=8) :: x
		
		! Genera la matriz aleatoria de 1 o -1
  		do row = 1, N
  	  		do col = 1, N
  	  			x = uni()
  	    		if (x< 0.5) then
  	      			M(row, col) = 1
  	    		else
  	      			M(row, col) = -1
  	    		end if
  	  		end do
  		end do
  		
  		!! Condiciones periodicas de contorno
  		M(0,:) = M(N,:)
  		M(N+1,:) = M(1,:)
  		M(:,0) = M(:,N)
  		M(:,N+1) = M(:,1)
  		
	end subroutine Random_M
	
	subroutine print_M()
		!! Imprime la matriz de Ising
		!! Usado para verificar que la inicializacion funcionaba
		integer :: row, col !locales
		
		write(*, *) 'Matriz Ising:'
  		do row = 0, N+1
    		do col = 0, N+1
    			write(*, '(I2, 2X)', advance='no') M(row, col)
    		end do
    			write(*, *) ! Salto de línea después de cada fila
  		end do
  	end subroutine print_M
  	
  	subroutine Delta_E()
  		!! esta subrutina ejecuta MC
  		integer :: row, col, dE, Sk_mu, z
  		real(kind=8) :: r
  		
  		do z = 1, int_2
  			!! Elegir un spin al azar
  			row = int(1+uni()*N)
  	  		col = int(1+uni()*N)
  	  		!! plantear Su -> -Sk
  			Sk_mu = M(row,col)
  			!! Calc delta_E
  			dE = 2 * J * Sk_mu * (M(row-1,col)+M(row+1,col)+M(row,col-1)+M(row,col+1))
  			!! ¿Aceptamos?
  			if(dE < 0) then
  				M(row,col) = Sk_mu * (-1)
  				!!Actualizar CC. ¿Se puede optimizar?
  				M(0,:) = M(N,:)
  				M(N+1,:) = M(1,:)
  				M(:,0) = M(:,N)
  				M(:,N+1) = M(:,1)
  			else
  				r = uni()
  				if(r < exp((-1.0/(kB*T(w))) * real(dE))) then
  					!! damos vuelta al spin
  					M(row,col) = Sk_mu*(-1)
  					!!Actualizar CC. ¿Se puede optimizar?
  						M(0,:) = M(N,:)
  						M(N+1,:) = M(1,:)
  						M(:,0) = M(:,N)
  						M(:,N+1) = M(:,1)
  				end if
  			end if
		end do 
	end subroutine Delta_E

	subroutine calc_E()
  		!! esta subrutina calcula E 
		integer :: row, col
		integer :: E !! Energia
		E = 0
  		do row = 1, N
    		do col = 1, N
      			E = E + (M(row,col) * (M(row-1,col) + M(row+1,col) + M(row,col-1) + M(row,col+1)))
      		end do
  		end do
  		write(2,*) E*(-J)
	end subroutine calc_E
	
	subroutine calc_M()
  		!! esta subrutina calcula M
		integer :: mag !! suceptibilidad magnetica
  		mag = sum(M(1:N,1:N))
  		write(3,*) mag
	end subroutine calc_M
	
end module
