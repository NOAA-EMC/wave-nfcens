!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      module ens_common
!
!     D. Cao 2008
!      Multiscale model ensemble
!
!     Revised by Vera Gerald, June 2011: including new grib2 output interface
!     Revised by J-Henrique Alves, June 2011: cleanup, removed pdsgds sbr 
!                       (grib2 parameter definitions now all made in grbit2.f)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
! Y. Y. Chao,  20110620 
! From original by HS Chen, 2004
! For common scales and arrays.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
!
      implicit none
!
      integer,parameter :: nx=360,ny=181,nxy=nx*ny
      integer,parameter :: lcgrib=4*nxy
      real,parameter :: x0=0.00,xend=359.00,dx=1.00
      real,parameter :: y0=-90.00,yend=90.00,dy=1.00
      integer,parameter :: iu11=11,iu12=12
      integer,parameter :: iu51=51,iu52=52,iu53=53,iu54=54,iu59=59
      real,parameter :: defv=1.0e10,tolr=1.0e-8,zero=0.0
!
      character :: id_para*5
      integer :: kpds(25),kgds(22)
      integer :: ymdc,fhr,nnme,nnsc,in_para
      integer :: idumy(nx,ny),istop
      real    :: dumy1d(nxy)
!
! buoys.
!
      integer :: nnb,readbuoy=1
      real,allocatable    :: xbuoy(:),ybuoy(:),rbuoy(:,:)
      integer,allocatable :: ixbuoy(:,:),jybuoy(:,:)
      character(len=5),allocatable :: idbuoy(:)
!
!
      character(len=13) :: fname
      logical*1 :: lbms2d(nx,ny),lbms1d(nxy)
      real      :: fsum(nx,ny),fmean(nx,ny),fspre(nx,ny)
      character,allocatable        :: id_member(:)*2
      character(len=7),allocatable :: id_file(:)
      real,allocatable             :: f(:,:,:),fprob(:,:,:),scale(:)
!
      real :: gmean,gspre
      real,allocatable :: g(:),gprob(:)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
      endmodule ens_common
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
      PROGRAM nfcomb_main
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MAIN PROGRAM:  ensemble    conduct ensemble statistics.
!   PRGMMR: H.S. CHEN        ORG: W/MP21     DATE: 04-05-20
!
! ABSTRACT: conduct ensemble statistics.
!
! PROGRAM HISTORY LOG:
!   04-05-20  H.S. Chen      Origination and implement at NCEP.
!
! USAGE:
!**   Interface.
!     ----------
!       The program nfcomb_main runs independently and must run after the
!       wave model.
!
!     Input File: 
!     ------------
!      nfcomb_input  - Provides date, parameter definitions, ensemble member
!                      names and numbers, probability level and input data
!                      file names (data_NN)
!     Output File: 
!     -------------
!      mean_out    - grib2 file containing combined ensemble mean
!      spread_out  - grib2 file containing combined ensemble spread
!      probab_out  - grib2 file containing combined ensemble probabilities of
!                    Hs exceeding a given value
!
!     Method.
!     -------
!       Interpolation at the closest grid point.       
!
!     Externals.
!     ----------
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  CRAY-C90
!
!$$$
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
! 
      use ens_common
!
      integer :: ierr,i,j,k,l,m,n,i1,i2,j1,j2,iret,leve
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
!     CALL W3TAGB('nww2ec  ',0097,0027,0075,'NP21   ')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
!
      call read_buoy
!
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      open (unit=iu51,file='station_out')
!
      call baopen(iu52,'mean_out',iret)
      write(*,'("** After baopen for mean, iret = ",i5)') iret
      call baopen(iu53,'spread_out',iret)
      write(*,'("** After baopen for spread, iret = ",i5)') iret
      call baopen(iu54,'probab_out',iret)
      write(*,'("** After baopen for probab, iret = ",i5)') iret
!
      open (unit=iu59,file='test_out')
!
      read(*,*) ymdc,fhr,id_para,in_para
      write(iu59,'(i10.10,2x,i3.3,2x,a5,i7)') ymdc,fhr,id_para,in_para 
      read(*,*) nnme
      write(iu59,'(i4)') nnme
      allocate( id_member(nnme) )
      read(*,*) id_member(:)
      write(iu59,'(20(1x,a2))') id_member(:)
      read(*,*) nnsc
      write(iu59,'(i4)') nnsc
!    
      allocate( scale(nnsc) )
      scale(:) = 0.0
        read(*,*) scale(:)
        write(iu59,'(10f8.3)') scale(:)
!
      allocate( id_file(nnme) )
      read(*,*) id_file(:)
      write(iu59,'(10(1x,a7))') id_file(:)
!
      allocate( f(nx,ny,nnme) )
      allocate( fprob(nx,ny,nnsc) )
!
      allocate( g(nnme) )
      allocate( gprob(nnsc) )
!
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      f(:,:,:) = 0.0
      do m=1,nnme
!
        fname=id_file(m)
        write(iu59,*) fname
!
        open (iu11,file=fname,access='SEQUENTIAL',form='UNFORMATTED')
!
        write(iu59,'("*** read f(m), m =",i4)') m
        read (iu11,end=12,err=91,iostat=ierr)     &
             ((f(i,j,m),i=1,nx),j=ny,1,-1)
!
        where( f(:,:,m).ge.defv ) f(:,:,m) = zero
!
   12   continue
        close(iu11)
      enddo
!
! Calculate ensemble statistics.
!
      call ens_statistics
!
! Output.
!
! Set up bitmap, lbms2d(:,:) and lbms1d(:), for missing data indicator; 
! true for data, false for no data.
!
      lbms2d(:,:) = .true.
      where( fmean(:,:).le.zero ) lbms2d(:,:) = .false.
!
      ij=1
        do j=1,ny
        do i=1,nx
          lbms1d(ij)=lbms2d(i,j)
          ij=ij+1
        enddo
      enddo
!
! ensemble mean.
!
      dumy1d(:)=0.0
      ij=1
      do j=1,ny
        do i=1,nx
          dumy1d(ij)=fmean(i,j)
          ij=ij+1
        enddo
      enddo
!
      call grbit2 (iu52,nx,ny,lcgrib,lbms1d,dumy1d,iret,ymdc,fhr, &
                                     1,0,0,0,nnme)
      if( iret.ne.0 ) then
        write(*,'("**** ERR in grbit2, ymdc,fhr,in_para,id_para: ")')
        write(*,'(1x,i10.10,1x,i3.3,i5,1x,a5)') ymdc,fhr,in_para,id_para
      endif
!
! ensemble spread.
!
      dumy1d(:)=0.0
      ij=1
        do j=1,ny
        do i=1,nx
          dumy1d(ij)=fspre(i,j)
          ij=ij+1
        enddo
      enddo
!
      call grbit2 (iu53,nx,ny,lcgrib,lbms1d,dumy1d,iret,ymdc,fhr, &
                                     2,0,0,0,nnme)
      if( iret.ne.0 ) then
        write(*,'("**** ERR in grbit2, ymdc,fhr,in_para,id_para: ")')
        write(*,'(1x,i10.10,1x,i3.3,i5,1x,a5)') ymdc,fhr,in_para,id_para
      endif
!
! ensemble probability.
!
      do l=1,nnsc
!
        dumy1d(:)=0.0
        ij=1
        do j=1,ny
          do i=1,nx
            dumy1d(ij)=fprob(i,j,l)
            ij=ij+1
          enddo
        enddo
!
        leve = nint(100.*scale(l))
        call grbit2 (iu54,nx,ny,lcgrib,lbms1d,dumy1d,iret,ymdc,fhr, &
                                     3,nnsc,l,leve,nnme)
        if( iret.ne.0 ) then
          write(*,'("** ERR in grbit2, ymdc,fhr,in_para,id_para,leve:")')
          write(*,'(1x,i10.10,1x,i3.3,i5,1x,a5,i7)')   &
                  ymdc,fhr,in_para,id_para,leve
        endif
      enddo
!
! interploation for stations.
!
      if( nnb.gt.0 ) then
        do l=1,nnb
          i1 = ixbuoy(l,1)
          i2 = ixbuoy(l,2)
          j1 = jybuoy(l,1)
          j2 = jybuoy(l,2)
          do k=1,nnme
            g(k) = rbuoy(l,1)*f(i1,j1,k) + rbuoy(l,2)*f(i2,j1,k) +   &
                   rbuoy(l,3)*f(i2,j2,k) + rbuoy(l,4)*f(i1,j2,k)
          enddo
!
          call ens_stat1
!
          write(iu51,'(i10.10,1x,i3.3,2(1x,a5),2f8.2)')    &
               ymdc,fhr,id_para,idbuoy(l),xbuoy(l),ybuoy(l)
          write(iu51,'(13f6.2,8f5.2)')    &
               (g(m),m=1,nnme),gmean,gspre,(gprob(k),k=1,nnsc)
        enddo
      endif
!
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
   91 continue
!
!  deallocate.
!
      if( nnb.gt.0 ) then
        deallocate( idbuoy,xbuoy,ybuoy,ixbuoy,jybuoy,rbuoy )
      endif
      deallocate( id_member )
      deallocate( id_file,scale,fprob,f )
      deallocate( g,gprob )
!
      close(iu51)
!
      call baclose(iu52,iret)
      write(*,'("** After baclose for mean, iret= ",i5)') iret
      call baclose(iu53,iret)
      write(*,'("** After baclose for spread, iret= ",i5)') iret
      call baclose(iu54,iret)
      write(*,'("** After baclose for probab, iret= ",i5)') iret
      close(iu59)
!
      STOP
      END
!
!
      subroutine read_buoy
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --++ + + +
!  read buoy id, lan, lat from fix buoy location list.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --++ + + +
!
      use ens_common
!
!  local
!
      character :: cdumy*1,cbid*5,cbtype*5,cbown*16
      integer   :: lon,lat,loc,nnb1,k,m,i1,i2,j1,j2
      real      :: anem,rx1,ry1
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!  calculate ratios only readbuoy.ge.1 .
!
      if( readbuoy.lt.1) goto 50
!
      open (iu11,file='buoy_file.data')
      open (iu51,file='buoy_ratio.data')
!
      nnb=0
   14 continue
        read (iu11,18,end=20) cdumy,cbid,cbtype,cbown,lat,lon,anem,loc
        if( cdumy.eq.'9' ) goto 20
        if( cdumy.eq.'c' ) goto 14
        nnb = nnb + 1
        goto 14
   18 format(a1,1x,a5,3x,a5,2x,a16,1x,2i4,f6.1,i2)
!
   20 continue
      if( nnb.lt.1 ) then
        write(*,'(" **** nnb=",i5)') nnb
        stop
      endif
!
      allocate( xbuoy(nnb) ) 
      allocate( ybuoy(nnb) ) 
      allocate( ixbuoy(nnb,2) ) 
      allocate( jybuoy(nnb,2) ) 
      allocate( rbuoy(nnb,4) ) 
      allocate( idbuoy(nnb) ) 
!
      rewind iu11
      nnb1=0
   24 continue
        read (iu11,18,end=30) cdumy,cbid,cbtype,cbown,lat,lon,anem,loc
        if( cdumy.eq.'9' ) goto 30
        if( cdumy.eq.'c' ) goto 24
        nnb1 = nnb1 + 1
        idbuoy(nnb1) = cbid
        ybuoy(nnb1) = 0.1*lat
        xbuoy(nnb1) = 360.0 - 0.1*lon
        goto 24
!
   30 continue
      write(*,'(" *** end of iu11 file *** ")')
      if( nnb1.ne.nnb ) then
        write(*,'(" **** nnb1=",i4," is not equal to nnb=",i4)') nnb1,nnb
        stop 1
      endif
!
      close( iu11)
!
! interpolation ratios.
!
      if( nnb.ge.1 ) then
        do k=1,nnb
          i1 = int((xbuoy(k)-x0)/dx) + 1
          i2 = i1 + 1
          if( i2.gt.nx ) i2=i2-nx
          rx1 = (xbuoy(k)-x0)/dx - float(i1-1)
          j1 = int((ybuoy(k)-y0)/dy) + 1
          j2 = j1 + 1
          if( j2.gt.ny ) j2=j2
          ry1 = (ybuoy(k)-y0)/dy - float(j1-1)
          ixbuoy(k,1)=i1
          ixbuoy(k,2)=i2
          jybuoy(k,1)=j1
          jybuoy(k,2)=j2
          rbuoy(k,1) = (1.0-rx1)*(1.0-ry1)
          rbuoy(k,2) =      rx1 *(1.0-ry1)
          rbuoy(k,3) =      rx1 *     ry1 
          rbuoy(k,4) = (1.0-rx1)*     ry1 
        enddo
      else
        write(*,'(" **** no buoy used ****")')
      endif
!
      write(iu51,'(i10)') nnb
      if( nnb.ge.1 ) then
        do k=1,nnb
          write(iu51,40)  k,idbuoy(k),xbuoy(k),ybuoy(k),   &
            (ixbuoy(k,m),m=1,2),(jybuoy(k,m),m=1,2),       &
            (rbuoy(k,m),m=1,4)
        enddo
      endif
   40 format(i5,1x,a5,2f8.2,4i5,4f8.4)  
!
      close( iu11 )
      close( iu51 )
!
      return
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
! read only, distribution ratios already exist.
!
   50 continue
      open (iu11,file='buoy_ratio.data')
      read (iu11,'(i10)') nnb
!!      write(*,'(i10)') nnb
!
      if( nnb.ge.1 ) then
!
        allocate( xbuoy(nnb) ) 
        allocate( ybuoy(nnb) ) 
        allocate( ixbuoy(nnb,2) ) 
        allocate( jybuoy(nnb,2) ) 
        allocate( rbuoy(nnb,4) ) 
        allocate( idbuoy(nnb) ) 
!
        do k=1,nnb
          read (iu11,40)  m,idbuoy(m),xbuoy(m),ybuoy(m),   &
            (ixbuoy(m,n),n=1,2),(jybuoy(m,n),n=1,2),       &
            (rbuoy(m,n),n=1,4)
        enddo
      else
        write(*,'("**** no buoy, no buoy arraysallocation ****")')
      endif
!
      close( iu11 )
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
! end subroutine read_buoy.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      return
      end
!
!
      subroutine ens_statistics
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
! Calculate ensemble statistics.
!   input : nx   = number of x points.
!           ny   = number of y points.
!           me   = number of ensemble members.
!           nnsc = number of scales for probability.
!           f(nx,ny,me)  = field array.
!           fsum(nx,ny)  = only used for calculating sum.
!           fscale(nnsc) = scale array.
!   output: fmean(nx,ny)      = ensemble mean array.
!           fspre(nx,ny)      = ensemble spread array.
!           fprob(nx,ny,nnsc) = ensemble probability.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      use ens_common
!
!  local
!
      integer :: m,n,i,j,k
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
!  mean.
!
      fsum(:,:) = 0.0
      do m=1,nnme
        fsum(:,:) = fsum(:,:) + f(:,:,m)
      enddo
      fmean(:,:) = fsum(:,:)/real(nnme)
!
!  spread - deviation.
!
      fspre(:,:) = 0.0
      do m=1,nnme
        fspre(:,:) = fspre(:,:) + (f(:,:,m)-fmean(:,:))**2
      enddo
      fspre(:,:) = sqrt( fspre(:,:)/real(nnme) )
!
!  probability.
!
     fprob(:,:,:) = 0.0
      do n=1,nnsc
        do m=1,nnme
          where( f(:,:,m).ge.scale(n) ) fprob(:,:,n)=fprob(:,:,n)+1.
        enddo     
        fprob(:,:,n) = fprob(:,:,n)/real(nnme)
      enddo     
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      return
      end
!
!
      subroutine ens_stat1
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
! Calculate ensemble statistics only at (one) station.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      use ens_common
!
!  local
!
      integer :: i,j,k
      real    :: gsum
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
!  mean.
!
      gsum = 0.0
      do i=1,nnme
        gsum = gsum + g(i)
      enddo
      gmean = gsum/real(nnme)
!
!  spread - deviation.
!
      gspre = 0.0
      do i=1,nnme
        gspre = gspre + (g(i)-gmean)**2
      enddo
      gspre = sqrt( gspre/real(nnme) )
!
!  probability.
!
     gprob(:) = 0.0
      do i=1,nnsc
        do j=1,nnme
          if( g(j).ge.scale(i) ) gprob(i)=gprob(i)+1.
        enddo     
        gprob(i) = gprob(i)/real(nnme)
      enddo     
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      return
      end
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
