!-----------------------------------------------------------------
! Año: 2023
! Curso: Introducción a la simulación computacional
! Docente: Claudio Pastorino
! URL: https://www.tandar.cnea.gov.ar/~pastorin/cursos/intro_sims/
!-----------------------------------------------------------------
program simple 
    use ziggurat
    use mod1 ! uso el mod1
    implicit none
    logical :: es
    integer :: seed, i


![NO TOCAR] Inicializa generador de número random

    inquire(file='seed.dat',exist=es)
    if(es) then
        open(unit=10,file='seed.dat',status='old')
        read(10,*) seed
        close(10)
        print *,"  * Leyendo semilla de archivo seed.dat"
    else
        seed = 24583490
    end if

    call zigset(seed)
![FIN NO TOCAR]    

!! 
!! EDITAR AQUI 
!! 

!! INICIALIZAR EL SISTEMA
	!! Lee N del input.dat
	call leer_param
  	print *,'N:',N,'J:',J,'Temp:',T,'int_1:',int_1,'int_2:',int_2
  		
  	!! Genera matriz aleatoria
  	call Random_M
    !! Imprime la matriz generada en forma de matriz
  	!! Solo para verificacion
  	!! call print_M
  	
  	!! archivos de salida E y M Para postprocesado
	open(unit=2, file='./Outputs/output_E.dat', status='replace')
	open(unit=3, file='./Outputs/output_M.dat', status='replace')
	
	!! Ciclo principal MC
	do w = 1, ntemp
		!! Genera matriz aleatoria
		!! call Random_M
		do i = 1, int_1		!! pasos MC es int_1*int_2
			call Delta_E
			!! Cuentas al final del ciclo
			call calc_E		!! escribe archivo output_E.dat
			call calc_M 	!! escribe archivo output_M.dat
		end do
	end do
	
	!! Cerrar archivos output
	close(2)
	close(3)
	
	!! Imprime la ultima matriz
	call print_M
	
!! 
!! FIN FIN edicion
!! 
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
        write(10,*) seed
        close(10)
![FIN no Tocar]        


end program simple
