#!/bin/ksh
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# This script is the ensemble postprocessor for the global ens wave model.  It +
# packs ensemble mean and spread in grib form.  It runs in serial mode and in  +
# its own directory. The output are copied to the output directory.            +
#                                                                              +
# For non-fatal errors output is witten to the ens.log file.                   +
#                                                                              +
# Feb, 2008                                                                    +
#                                                                              +
#                            ************************                          +
# Modified for NCEP and FNMOC ensemble wave models combined in grib2 form.     +
# June 2011                                                                    +
# March-2019 RPadilla & JHAlves                                                +
#                   - Merging wave scripts to global workflow for GEFS         +
#                                                                              +
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# 0.  Preparations
# 0.a Basic modes of operation
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#

#0.  Preparations
# 0.a Basic modes of operation

  set -x
  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x


  postmsg "$jlogfile" "HAS BEGUN on `hostname`"

  msg="Starting WAVES_NFCENS SCRIPT for $runID"
  postmsg "$jlogfile" "$msg"
  set +x
  echo ' '
  echo '                   ******************************************'
  echo '                   *** WAVE ENSEMBLE POSTPROCESSOR SCRIPT ***'
  echo '                   ******************************************'
  echo ' '
  echo "Starting at : `date`"
  [[ "$LOUD" = YES ]] && set -x

#
# 0.b Date and time stuff
#
  export YMD=$PDY
  export YMDH=${PDY}${cyc}
  export tcycz=t${cyc}z

  export grdID='glo_30m'

#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# 1. Get Buoy files.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#
# buoy input files exist?
#
  if [ -f $FIXwave/wave_ens_buoy.data ] ; then
    cp  $FIXwave/wave_ens_buoy.data  buoy_file.data
    set +x
    echo " $FIXwave/wave_ens_buoy.data copied to buoy_file.data."
    [[ "$LOUD" = YES ]] && set -x
  else
    msg="ABNORMAL EXIT: ERR in coping ens_buoy_file.data."
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '***************************************************** '
    echo "*** ERROR : No $FIXwave/wave_mens.data  copied. *** "
    echo '***************************************************** '
    echo ' '
    echo $msg
    [[ "$LOUD" = YES ]] && set -x
    echo "$FIXwave/wave_ens_buoy.data  missing." >> $ensemb_log
    err=1;export err;err_chk
  fi
#
  if [ -f $FIXwave/wave_ens_buoy.ratio ] ; then
    cp  $FIXwave/wave_ens_buoy.ratio  buoy_ratio.data
    set +x
    echo " $FIXwave/wave_ens_buoy.ratio copied to buoy_ratio.data."
    [[ "$LOUD" = YES ]] && set -x
  else
    msg="ABNORMAL EXIT: ERR in coping ens_buoy_file.data."
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '***************************************************** '
    echo "*** ERROR : No $FIXwave/wave_ens_buoy.ratio  copied.*** "
    echo '***************************************************** '
    echo ' '
    echo $msg
    [[ "$LOUD" = YES ]] && set -x
    echo "$FIXwave/wave_ens_buoy.ratio  missing." >> $ensemb_log
    err=1;export err;err_chk
  fi
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#
    end_hour=${FHMAX_WAV}
    set +x
    echo "end_hour=${END_HOUR}"
    [[ "$LOUD" = YES ]] && set -x
    dh=$FHOUT_WAV

   para='HTSGW'  

#Beaufort Wind Scale, from Force 2 t 10.  And corresponding wave heights.
  uscale='3.60 5.65 8.74 11.31 14.39 17.48 21.07 24.67'
  hscale='0.60 1.00 2.00  3.00  4.00  5.50  7.00  9.00'
  tpscale='5.0 7.0 9.0 11.0 13.0 15.0 17.0 19.0'
  tmscale=''
  tsscale=''
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# 2. Get NCEP files.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#
# get data for specified members

#XXX  membn='00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20' 
  membn='00'
  blnkspc=' '
  for i in $(seq -f "%02g" 1 $npert)
  do 
    export membn=${membn}${blnkspc}${i}
  done

  nmembn=`echo ${membn} | wc -w`

  for me in $membn
  do
#
# get data for specified forecast hour for specified parameter.
    fhour=0
    while [ "$fhour" -le "$end_hour" ]
    do
      if [ $fhour -eq 0 ] ; then
        ihr='anl'
        hhh='000'
      elif [ $fhour -lt 10 ] ; then
        ihr=${fhour}' hour'
        hhh='00'$fhour
      elif [ $fhour -lt 100 ] ; then
        ihr=${fhour}' hour'
        hhh='0'$fhour
      elif [ $fhour -ge 100 ] ; then
        ihr=${fhour}' hour'
        hhh=$fhour
      fi

      ftype="p$me"
      if [ "$me" == "00" ]; then
        ftype="c00"
      fi
      file2proc=${COMIN}/${model}.${cycle}.${ftype}.${gridID}.f${hhh}.grib2
      if [ -f $file2proc ] ; then
# Strip grib2 file onto old 1x1 resolution at 6h intervals
        $WGRIB2 -lola 0:360:1 -90:181:1 $me.f${hhh}.grib2 grib \
                          $file2proc -match HTSGW  1> out 2>&1

      else
        msg="ABNORMAL EXIT: ERR in coping $lnkfile."
        postmsg "$jlogfile" "$msg"
        set +x
        echo '******************************************************* '
        echo "*** FATAL ERROR : No $file2proc copied ** "
        echo '******************************************************* '
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        echo "$file2proc missing." >> $ensemb_log
        err=1;export err;err_chk
      fi

      for ip in $para
      do
         $WGRIB2 $me.f${hhh}.grib2 -s | grep ":${ip}:surface:${ihr}" | $WGRIB2 -i $me.f${hhh}.grib2  \
            -grib ${ip}_${me}.${hhh}.grib2 > wgrib.out 2>&1 

#       ls -la ${ip}_${me}.${hhh}.grib2
        ok1=$?
        if [ $ok1 -ne 0 ] ; then
          set +x
          echo " *** ERROR : ip=$ip, im=$me, ok1=$ok1"
          [[ "$LOUD" = YES ]] && set -x
          exit
        fi
#       echo " got from NCEP's ${me}.t${cyc}z: ${ip}_${me}.${hhh}.grib2."
         $WGRIB2 ${ip}_${me}.${hhh}.grib2 -append -grib  ${ip}_${me}.t${cyc}z.grib2
      done

      if [ $fhour -ge $FHMAX_HF_WAV ]; then
        inc=$FHOUT_WAV
      else
        inc=$FHOUT_HF_WAV
      fi
      let fhour=fhour+inc

    done
    rm -f $me.grib2
  done
  nlast=$me
  set +x
  echo "last NCEP member is $nlast"
  [[ "$LOUD" = YES ]] && set -x

#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# 3. Get FNMOC files.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#

   chkFNMOC=YES
   for chkmemb in $(seq 1 $me)
   do
      if [ $chkmemb -lt 10 ]
      then
         chkmemb='0'${chkmemb}
      fi
      for chkhr in $(seq 0 $dh $end_hour)
      do
         if [ $chkhr -lt 10 ]
         then
            chkhr='00'${chkhr}
         elif [ $chkhr -lt 100 ]
         then
            chkhr='0'${chkhr}
         fi
         chkfile=$COMFN/${fnmID}_et0${chkmemb}.${chkhr}.${YMDH}
         if [ ! -f ${chkfile} ]
         then
            chkFNMOC=NO
         fi
      done
   done

#  chkfile=$COMFN/${fnmID}_et0${me}.${end_hour}.${YMDH}
  foundFN='no'

    
  if [ ${chkFNMOC} = 'NO' -o ${RUN_FNMOC} = 'NO' ]
  then
# FNMOC file for last forecast time is not available, go without FNMOC data
    membf=0
    nmembf=0

  else
# FNMOC file for last forecast time found
    foundFN='yes' 

    #membf='01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20'
    
    membf='01'
    blnkspc=' '
    for i in $(seq -f "%02g" 2 $npert)
    do 
      export membf=${membf}${blnkspc}${i}
    done

    nmembf=`echo ${membf} | wc -w`
#
# forecast hour loop.
#
    fhour=0
#
    while [ "$fhour" -le "$end_hour" ]
    do
#
      if [ $fhour -eq 0 ] ; then
        ihr='anl'
        hhh='000'
      elif [ $fhour -lt 10 ] ; then
        ihr=${fhour}' hour'
        hhh='00'$fhour
      elif [ $fhour -lt 100 ] ; then
        ihr=${fhour}' hour'
        hhh='0'$fhour
      elif [ $fhour -ge 100 ] ; then
        ihr=${fhour}' hour'
        hhh=$fhour
      fi
#
# member loop
#
      for me in $membf
      do
        
        ie=` expr $nlast + $me `
        #let ie=nlast+me+1
        if [ ${ie} -lt 10 ]
        then
          ie='0'${ie}
        fi

        filefnmoc0p5=$COMFN/${fnmID}_et0${me}.${hhh}.${YMDH}
        if [ -f $filefnmoc0p5 ] ; then
          $WGRIB2 -lola 0:360:1 -90:181:1 ${ie}.${hhh}.grib2 grib \
                           $filefnmoc0p5 -match HTSGW  1> out 2>&1
#         Using copygb2 (But we use $WGRIB2 for consitence with ncep data)
#grid="0 6 0 0 0 0 0 0 360 181 0 -1 -90000000 0 48 90000000 359000000 1000000 1000000 64"
#$COPYGB2 -g "${grid}" -i0 -x  "${filefnmoc0p5}" ${ie}.${hhh}.grib2

        else
           msg="ABNORMAL EXIT: ERR in coping $filefnmoc0p5 "
           postmsg "$jlogfile" "$msg"
           set +x
           echo ' '
           echo '******************************************************* '
           echo "*** ERR : No $filefnmoc0p5 copied. *** "
           echo '******************************************************* '
           echo ' '
           echo $msg
           [[ "$LOUD" = YES ]] && set -x
           echo "$filefnmoc0p5 missing." >> $ensemb_log
           err=1;export err;err_chk
        fi
#       Parameter loop
        for ip in $para
        do
          infile=${ie}.${hhh}.grib2  
          $WGRIB2 $infile -s | grep "${ip}:surface:${ihr}" | $WGRIB2 -i $infile -grib  ${ip}_${ie}.${hhh}.grib2
          ok1=$?
          if [ $ok1 -ne 0 ] ; then
            set +x
            echo " *** ERROR : ip=$ip, me=${me}, ok1=$ok1"
            [[ "$LOUD" = YES ]] && set -x
            exit
          fi
#         echo " got from FNMOC's et0${me}.${hhh}.${YMDH}: ${ip}_${ie}.${hhh}.grib2."
          $WGRIB2 ${ip}_${ie}.${hhh}.grib2 -append -grib  ${ip}_${ie}.t${cyc}z.grib2
        done
      done


      if [ $fhour -ge $FHMAX_HF_WAV ]; then
        inc=$FHOUT_WAV
      else
        inc=$FHOUT_HF_WAV
      fi
      let fhour=fhour+inc

      #XXXfhour=`expr $fhour + $dh`
    done
  fi
#
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# 4. Output to what_{modID}_used
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#
  if [ "$foundFN" = 'no' ]
  then
    msg="COULD NOT FIND FNMOC ENSEMBLES DATA, USING ONLY NCEP ENSEMBLES"
    postmsg "$jlogfile" "$msg"

    set $setoff

    export maillist='ncep.list.spa-helpdesk@noaa.gov,sdm@noaa,ncep.sos@noaa.gov,Henrique.Alves@noaa.gov'
    echo ' ' > warning
    echo '******************************************************' >> warning
    echo '*** WARNING !! COULD NOT FIND FNMOC ENSEMBLES DATA ***' >> warning
    echo '***         (USING ONLY NCEP ENSEMBLES)            ***' >> warning
    echo '******************************************************' >> warning
    echo ' ' >> warning
    echo 'NCEP-FNMOC COMBINED WAVE ENSEMBLE WILL RUN WITH ONLY NCEP MEMBERS' >> warning
    echo "$modID $grdID prep $date $cycle : FNMOC file(s) missing (using only NCEP)" >> $wavelog
   
    #XXX cat warning |mail.py -v -s "Missing FNMOC Ensemble Data For $PDY t${cyc}z" $maillist
    

  fi

  echo "NCEP ${nmembn}" > what_wave${modID}_used.t${cyc}z
  if [ "$foundFN" == 'yes' ]
  then
    echo "FNMOC ${nmembf}" >> what_wave${modID}_used.t${cyc}z
  fi
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# 5. Compute mean, spread and probability of exceedence 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#
# set $seton

  rm -f mean.t${cyc}z.grib spread.t${cyc}z.grib probab.t${cyc}z.grib

  nmemb=`expr ${nmembn} + ${nmembf}`
  nmembm1=${nmemb}
#
# Create list of combined ensemble member numbers (starting from 00 = NCEP control run)
#
  memb=`seq -w 0 ${nmembm1}`
#
# forecast hour loop.
#
  fhour=0
#
  while [ "$fhour" -le "$end_hour" ]
  do
#
    if [ $fhour -eq 0 ] ; then
      ihr='anl'
      hhh='000'
    elif [ $fhour -lt 10 ] ; then
      ihr=${fhour}' hour'
      hhh='00'$fhour
    elif [ $fhour -lt 100 ] ; then
      ihr=${fhour}' hour'
      hhh='0'$fhour
    elif [ $fhour -ge 100 ] ; then
      ihr=${fhour}' hour'
      hhh=$fhour
    fi
#
    for ip in $para
    do
#
      scale='     '
      case $ip in
        HTSGW)   scale=$hscale ; id_para='10,0,3'  ;;
        *)       scale=$tsscale; id_para='   '  ;;
      esac
#
      rm -f fname_input data_* 
#
      echo $YMDH $hhh $ip $id_para  > fname_input
      echo ${nmemb}                 >> fname_input
      echo $memb                    >> fname_input
      echo $scale | wc -w           >> fname_input
      echo $scale                   >> fname_input
#
#      for im in $memb
      nme=0
      while [ ${nme} -lt ${nmemb} ]
      do

      im=${nme}
      if [ ${nme} -lt 10 ]
      then
        im='0'${nme}
      fi
 
        infile=${ip}_${im}.${hhh}.grib2
           $WGRIB2 $infile -s \
           | grep "${ip}:surface:${ihr}" | \
           $WGRIB2 -i $infile -bin data_${im}
#       ls -la data_${im}
        ok1=$?
        if [ $ok1 -ne 0 ] ; then
          set +x
          echo " *** ERROR : ip=$ip, im=$im, ok1=$ok1"
          [[ "$LOUD" = YES ]] && set -x
          exit
        fi
        echo data_$im       >> fname_input
        nme=`expr ${nme} + 1`
      done
#
# execute ensemb.x
# and creat grib2 files
#
      rm -f station_out mean_out spread_out probab_out test_out
#
      $EXECwave/nfcombwave_ensemble  < fname_input >>$pgmout 2>&1
#
      cat station_out >> ${ip}_station.t${cyc}z.text

      cp mean_out    ${ip}_mean.$hhh.grib2
      cp spread_out  ${ip}_spread.$hhh.grib2
      cp probab_out  ${ip}_probab.$hhh.grib2
      $WGRIB2  mean_out  -append -grib ${ip}_mean.t${cyc}z.grib2
      $WGRIB2  spread_out -append -grib ${ip}_spread.t${cyc}z.grib2
      $WGRIB2  probab_out -append -grib ${ip}_probab.t${cyc}z.grib2
#
    done

      if [ $fhour -ge $FHMAX_HF_WAV ]; then
        inc=$FHOUT_WAV
      else
        inc=$FHOUT_HF_WAV
      fi
      let fhour=fhour+inc
    #XXXfhour=`expr $fhour + $dh`
  done
#
  mv  *station.t${cyc}z.text $COMOUT/.
  mv  *.t${cyc}z.grib2    $COMOUT/. 

#
  gzip $COMOUT/*station.t${cyc}z.text
#
  cp what_wave${modID}_used.${cycle} ${COMOUT}/.
#
  if [ "$SENDDBN" = 'YES' ]
  then
    for dbmemb in `seq -w 0 $((${nmembm1}-1))`
    do
       $DBNROOT/bin/dbn_alert MODEL NFC_WAVEENS $job $COMOUT/HTSGW_${dbmemb}.t${cyc}z.grib2
    done
       $DBNROOT/bin/dbn_alert MODEL NFC_WAVEENS $job $COMOUT/HTSGW_mean.t${cyc}z.grib2
       $DBNROOT/bin/dbn_alert MODEL NFC_WAVEENS $job $COMOUT/HTSGW_spread.t${cyc}z.grib2
       $DBNROOT/bin/dbn_alert MODEL NFC_WAVEENS $job $COMOUT/HTSGW_probab.t${cyc}z.grib2
       $DBNROOT/bin/dbn_alert MODEL NFC_WAVEENS $job $COMOUT/HTSGW_station.t${cyc}z.text.gz
       $DBNROOT/bin/dbn_alert MODEL NFC_WAVEENS $job $COMOUT/what_wave${modID}_used.${cycle}
  fi
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# END
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#
