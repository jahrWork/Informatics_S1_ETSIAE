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

function Taylor( fk, x0, x, M ) result(T)
 procedure (f_RxN_R) :: fk               ! kth derivative of f(x) 
 real, intent(in) :: x0, x               ! x0 origen of Tayler series
                                         ! Taylor is evaluated at x 
 integer, optional, intent(in) :: M      ! polynomial degree (M+1 terms)  
 real :: T                               ! T is the evaluation of Tayloer series at x 
 
      integer :: k    ! index of Taylor series 
      real :: ak      ! general term of the series 
      integer :: Mmax ! max number of terms 
      logical :: add  ! if true then, sum 
      real :: Tk      ! kth term of Taylor series 
      real :: Tk1     ! k-1th term of Taylor series 
      real :: eps     ! Tolerance 
      
      
      if (present(M)) then 
                           Mmax = M 
      else 
                           Mmax = 1000
      end if 
      
       T = 0 ; eps = 1e-5; k = 0 
       add = .true. 
       
       do while (add) 
           ak = fk(x0, k) / gamma( real(k+1) ) 
           Tk = ak * ( x - x0 )**k  
           T = T + Tk   
         ! sum if Tk is greater eps and 
         ! sum if number of terms is smaller than Mmax   
           add = (abs(Tk) > eps .or. abs(Tk1) > eps) .and. (k < Mmax)
           k = k + 1
           Tk1 = Tk 
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
    