module Taylor_expansion
    
    use dislin 
    implicit none 
   
     
  ! f: R  --> R   
     interface 
     function f_R_R(x) result(f)
        real, intent(in) :: x 
        real :: f 
      end function 
    end interface 
    
    
  ! f: R x N --> R  
    interface 
     function f_RxN_R(x, k) result(f)
        real, intent(in) :: x 
        integer, intent(in) :: k 
        real :: f 
      end function 
    end interface 
    
    real :: xmin_, xmax_ 
    
    contains 

function Taylor( fk, x0, x, M ) result(S)
 procedure (f_RxN_R) :: fk               ! kth derivative of f(x) 
 real, intent(in) :: x0, x               ! x0 origen of Tayler series
                                         ! Taylor is evaluated at x 
 integer, optional, intent(in) :: M      ! polynomial degree (M+1 terms)  
 real :: S                               ! S is the evaluation of Taylor series at x 
 
      integer :: k    ! index of Taylor series 
      real :: ak      ! general term of the series 
      integer :: Mmax ! max number of terms 
      real :: S1      ! M terms of Taylor series   
      logical :: add  ! if true then, sum  
      
      
      if (present(M)) then 
                           Mmax = M 
      else 
                           Mmax = 1000
      end if 
      
       S = 0 ;  k = 0 
       add = .true. 
       
       do while (add) 
           ak = fk(x0, k) / gamma( real(k+1) ) 
           S1 = S 
           S = S + ak * ( x - x0 )**k    
         ! sum if S(k) /= S(k-1) and k < Mmax
           add = (S /= S1 .or. abs(ak) == 0) .and. (k < Mmax)
           k = k + 1
       end do 
       
   
end function 
    
    
subroutine plot_ini( xmin, xmax,  ymin, ymax ) 
    real, intent(in) ::  xmin, xmax, ymin, ymax 

    
    call metafl("xwin")
    CALL PAGE (4000, 4000)
    call scrmod("reverse")
    call disini  
    call graf(xmin, xmax, xmin, (xmax-xmin)/10, ymin, ymax, ymin, (ymax-ymin)/10) 
   
    xmin_ = xmin 
    xmax_ = xmax 
    
end subroutine    
    
    
subroutine plotf( f ) 
 procedure (f_R_R) :: f 
 
 
    integer, parameter :: M = 200  
    integer :: i 
    real :: x(0:M), y(0:M)    
    
      x = [ ( xmin_ + (xmax_-xmin_)*i/real(M), i=0, M) ] 
      
      y = [ ( f( x(i) ), i=0, M) ]
      
      call curve(x, y, M+1) 
      
    
    
end subroutine 



    
end module
    