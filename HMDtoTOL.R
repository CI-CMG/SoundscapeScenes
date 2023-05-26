# HMD to TOL

# read in HMD file (1-day) ####

# MANTA HMD DATA ####
dirHMD = "J:\\SanctSound_AcousticScene" # ?? create loop through all manta directories ??
inDir  = choose.dir(default = dirHMD , caption = "Site directory with HMD csv files" ) # GR01_01
inHMD =  list.files(path = inDir, pattern = "MinRes.csv", full.names = T,recursive = T)
st = sapply(strsplit(basename( inHMD [1] ), "_"), "[[", 1) #site name
dpl = sapply(strsplit(basename( inHMD[1] ), "_"), "[[", 2) # deployment name

Pss =  read.csv(inHMD[2]) # 1-day of minute files (60*24)

# existing code examples
# PAMGuide
#PAMGuide <- function(...,atype='PSD',plottype='Both',envi='Air',calib=0,
                    # ctype = 'TS', Si=-159, Mh=-36, G=0, vADC=1.414, r=50, N=Fs, winname='Hann',
                    # lcut=Fs/N,hcut=Fs/2, timestring="",
                    # outdir=dirname(fullfile),outwrite=0,disppar=1,welch="",chunksize="",linlog = "Log")
  
# INPUT PSD parameters ####
Fs = 48000
N = 48000
lcut = Fs/N
hcut = Fs/2
envi = 'Wat'

M = length(xgrid[1,]) #PSD matrix

## System sensitivity if provided
if (calib == 1){
  if (ctype == 'EE') {		#'EE' = end-to-end calibration
    S <- Si}
  if (ctype == 'RC') {		#'RC' = recorder calibration with separate transducer sensitivity defined by Mh
    S <- Si + Mh}
  if (ctype == 'TS') {		#'TS' = manufacturer's specifications
    S <- Mh + G + 20*log10(1/vADC);		#EQUATION 4
    # insert option to calculate across a frequencies???
  } 
  
  if (disppar == 1){cat('System sensitivity correction factor, S = ',sprintf('%.01f',S),' dB\n')}
} else {S <- 0}				#if not calibrated (calib == 0), S is zero

## Reference pressure for envrioment= dB calculation
if (envi == 'Wat'){ pref = 1 ;  aid = aid+200}


#Compute frequencies of DFT bins
f = floor(Fs/2)*seq(1/(N/2),1,len=N/2)
flow = which(f >= lcut)[1]			#find index of lcut frequency
fhigh = max(which(f <= hcut))		#find index of hcut frequency
f = f[flow:fhigh]					      #limit frequencies to user-defined range
nf = length(f)					       	#number of frequency bins

# Compute TOL from DFT data
if (atype == 'TOL') {
  if (lcut < 25){ lcut = 25 } #limit TOL analysis to > 25 Hz
    
  #Generate 1/3-octave freqeuncies
  lobandf = floor( log10(lcut) )	  #lowest power of 10 for TOL computation
  hibandf = ceiling( log10(hcut) )	#highest power of 10 for TOL computation
  nband = 10*(hibandf-lobandf)+1	  #number of 1/3-octave bands
  fc = matrix(0,nband)			       #initialise 1/3-octave frequency vector
  fc[1] <- 10^lobandf
  
  #Calculate centre frequencies (corresponds to EQUATION 13)
  for (i in 2:nband) {  fc[i] = fc[i-1]*10^0.1 }	
    fc = fc[which(fc >= lcut)[1]:max(which(fc <= hcut))]
  
  nfc = length(fc)				#number of 1/3-octave bands
  
  #Calculate boundary frequencies of each band (EQUATIONS 14-15)
  fb = fc*10^-0.05			     	#lower bounds of 1/3-octave bands
  fb[nfc+1] = fc[nfc]*10^0.05	#upper bound of highest band
  if (max(fb) > hcut) {			  #if upper bound exceeds highest
    nfc = nfc-1				        # frequency in DFT, remove
    fc = fc[1:nfc] }
  
  #Calculate TOLs (corresponds to EQUATION 16)
 
   P13 <- matrix(nrow = M, ncol = nfc)
  for (i in 1: nfc) {
    fli = which(f >= fb[i])[1]
    fui = max(which(f < fb[i+1]))
    for (k in 1:M) {
      fcl = sum( Pss[fli:fui,k] )
      P13[k,i] = fcl
    }
  }
  a <- t( 10*log10(P13/(pref^2)) ) - S
}


# Triton- soundscape metrics
# others?

# center frequencies

# read in files to process

# output directory