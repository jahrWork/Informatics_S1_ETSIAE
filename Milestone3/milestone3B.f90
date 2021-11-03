module milestone3B 
    
    implicit none 
    
    contains 
    
subroutine real_and_integers  

     real  :: x ! equivalent to real (kind=4) :: x 
     integer :: i 
     

     x = 123.456
     i = 123
     
     write(*,*) " x is real =", x  
     write(*,*) " i is integer =", i  


end subroutine     
    
subroutine loss_of_precision 

     real  :: x, y  
     real (kind=4) :: eps 
     

     x = 1 
     eps = 1e-7   ! <- assign 
     y = x + eps 
     
     write(*,*) " y= 1 + eps =", y 


end subroutine 


subroutine do_while_loop 

     real :: Error 
     real :: SN
     real :: an ! general term of series 
     integer :: n 
    
    
    n = 1
    SN = 0
    Error = 10 
    
    do while (  Error > 1e-3 ) 
        
                          ! 1 is a integer constant 
                          ! 1. or 1.0 is a real constant 
        
         an = 1. / 2**n        ! exact solution sum = 1
    !    an = 1 / real(n)**2   ! exact solution sum = pi**2/6 
        
        SN = SN + an      ! SN <- SN + an 
        
       Error = abs( 1 - SN )   ! | 1 - SN | absolute value 
   !   Error = abs( PI**2/6 - SN )   ! | 1 - SN | absolute value 
        
        write(*,*) " n = ", n, " Error =", Error, " an =", an 
        
        n = n + 1 
        
    end do 
    
    write(*,*) " SN = ", SN  
    

end subroutine 

    
    
    
end module 