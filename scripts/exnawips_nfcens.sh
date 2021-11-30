###################################################################
#### "----------------------------------------------------"
#### "exnawips - convert NCEP GRIB files into GEMPAK Grids"
#### "----------------------------------------------------"
#### "History: Mar 2000 - First implementation of this new script."
#### "Sept 2011 - First implementation of this new script based on"
#### "               /nwprod/scripts/exnawips.sh.sms"
#### " March 2020- Modified for GEFSv12.0"
# March-2020 RPadilla & JHAlves                                     
#####################################################################

set -xa
# Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x



cd $DATA

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

cpyfil=gds
garea=dset
gbtbls=
maxgrd=4999
kxky=
grdarea=
proj=
output=T
pdsext=no

maxtries=15
fhcnt=$fstart

cat ${COMIN}/what_wave${RUN}_used.${cycle} | grep FNMOC
fused=$?
if [ ${fused} = '0' ]
then
   nmemb=`seq -w 0 40`
else
   nmemb=`seq -w 0 20`
   set +x
   echo "**********************************************"
   echo "******WARNING !! NO FNMOC MEMBERS FOUND*******"
   echo "* CREATING GEMPAK GRIDS FOR NCEP MEMBERS ONLY*"
   echo "**********************************************"
   [[ "$LOUD" = YES ]] && set -x
fi

for memb in $nmemb mean probab spread
do
  GRIBIN=${COMIN}/HTSGW_${memb}.${cycle}.grib2

  if [ ${memb} == 'mean' -o ${memb} == 'probab' -o ${memb} == 'spread' ]
  then
     GEMGRD=htsgw_${memb}_${PDY}${cyc}
  else
     GEMGRD=htsgw_${memb}m_${PDY}${cyc}
     if [ ${memb} -lt 21 ]
     then
       g2tbls=g2varswmo2.tbl
     else
       g2tbls=g2varswmo5.tbl
     fi
  fi

  NAGRIB=nagrib2

  GRIBIN_chk=$GRIBIN


## Commented the lines below as Z doesnt work in WCOSS2 and fhr and fhr3 are
## not used in this script
#  if [ $fhcnt -ge 100 ] ; then
#    typeset -Z3 fhr
#  else
#    typeset -Z2 fhr
#  fi
#  fhr=$fhcnt

#  fhr3=$fhcnt
#  typeset -Z3 fhr3

  icnt=1
  while [ $icnt -lt 1000 ]
  do
    if [ -r $GRIBIN_chk ] ; then
      break
    else
      let "icnt=icnt+1"
      sleep 20
    fi
    if [ $icnt -ge $maxtries ]
    then
      msg="ABORTING after 5 minutes of waiting for $GRIBIN."
      postmsg "$msg"
      export err=2 ; err_chk
    fi
  done

  cp $GRIBIN grib_${memb}

  startmsg

  $NAGRIB << EOF
   GBFILE   = grib_${memb}
   INDXFL   = 
   GDOUTF   = $GEMGRD
   PROJ     = $proj
   GRDAREA  = $grdarea
   KXKY     = $kxky
   MAXGRD   = $maxgrd
   CPYFIL   = $cpyfil
   GAREA    = $garea
   OUTPUT   = $output
   GBTBLS   = $gbtbls
   G2TBLS   = $g2tbls
   GBDIAG   = 
   PDSEXT   = $pdsext
  l
  r
EOF
  export err=$?;pgm=$NAGRIB;err_chk

  #####################################################
  # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
  # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
  # FOR THIS CASE HERE.
  #####################################################
  ls -l $GEMGRD
  export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

  if [ "$NAGRIB" = "nagrib2" ] ; then
    gpend
  fi

  if [ $SENDCOM = "YES" ] ; then
     cpfs $GEMGRD $COMOUT/$GEMGRD
     if [ $SENDDBN = "YES" ] ; then
         $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
           $COMOUT/$GEMGRD
     else
       set +x
       echo "##### DBN_ALERT is: MODEL ${DBN_ALERT_TYPE} $job $COMOUT/$GEMGRD#####"
       [[ "$LOUD" = YES ]] && set -x
     fi
  fi
  rm grib_${memb}
  let fhcnt=fhcnt+finc
done

#####################################################################
# GOOD RUN
set +x
echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE IBM"
echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE IBM"
echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE IBM"
[[ "$LOUD" = YES ]] && set -x
#####################################################################
msg='Job completed normally.'
echo $msg
postmsg "$jlogfile" "$msg"
############################### END OF SCRIPT #######################
