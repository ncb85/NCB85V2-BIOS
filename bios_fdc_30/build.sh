# LINUX version of build script for NCB85 BIOS
#
ASPATH=$HOME/Programs/as/asl-1.42-222
ASL=$ASPATH/asl
P2H=$ASPATH/p2hex
PROJPATH=$HOME/Programs/NCB85/BIOS/NCB85V2-BIOS/bios_fdc_30
FILENAME=ncb85bios

build_target()
{
  FDTYPE=$1
  PARAM2=$2
  if [ -n "$PARAM2" ]; then
    EXTRA=${PARAM2}
  else
    EXTRA=0
  fi
  echo "f" ${FDTYPE} "p" ${PARAM2} "e" ${EXTRA} 
  ${ASL} -cpu 8080 -L -OLIST ${PROJPATH}/${FILENAME}_${FDTYPE}${PARAM2}.lst -D Floppy=${FDTYPE},Extra=${EXTRA} -i ${PROJPATH} ${PROJPATH}/${FILENAME}.asm

  ${P2H} ${PROJPATH}/${FILENAME}.p ${PROJPATH}/${FILENAME}_${FDTYPE}${PARAM2}.ihx -k -r 0x-0x1FFF -R 57344 -F Intel -i 0 >/dev/null 

  sort -k1.8,1.9 -k1.4,1.7 ${FILENAME}_${FDTYPE}${PARAM2}.ihx > ${FILENAME}_${FDTYPE}${PARAM2}.hex

  rm ${FILENAME}_${FDTYPE}${PARAM2}.ihx
}

# FDTYPEs 360k,720k,1.2MB,1.44MB,1.0MB IBM
# optional second parameter 50 denotes an extra 8" C: SS/DD drive, capacity 500kB
# build_target 360
# build_target 720
build_target 120
# build_target 120 50
build_target 120 120
# build_target 144
# build_target 144 50
# build_target 144 144
# build_target 100



