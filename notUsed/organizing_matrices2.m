%organizing_matrices for clustering
%goal: create TPWS file

%% STEP 1: read in the time data
% step 1A: drag in OMTT (which is the time vector-- save new var as string array. ignore 1st row

% step 1B: convert to matlab serial dates
MTT=dbISO8601toSerialDate(OMTT); %change the thing in parenthesis to whatever variable is saved as

%% STEP 2: set MSP to Lr,Sp, or Or Matrix-- MSP is your spectra
%step 2A: drag in the matrix (which was created after RRPCA from R code)
LNSP=OmatrixLJ21p; %change this to whatever your matrix is and save as MSP

% step 2B: clean up MSP 
MSP=LNSP(2:end,1:38); %change the last value to whatever the last column is (depending on if going up to 1000 or 5000 hz)
MSP=table2array(MSP); %change table to array
% i elimated first line of code-- just remember this is 70 to 5000 hz, in 5hz chunks

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% STEP 3: create MSN and MPP (these are vectors that we just fanegled to make the clustering tool work-- but are meaningless)
% step 3A: MPP is all ones, and the same size as MTT (time vector)
MPP = ones(size(MTT));
% step 3B: MSN same vertical dimension as spectra (MSP) and 2 columns wide
MSN = ones(size(MSP,1),2);

%% STEP 4: 
%now save f
f=60:20:800 %creates the frequency range by 20 hz (change middle and end values based on what you have in matrix)

% save f, MPP, MSN, MSP, MTT all into a TPWS file and go onto the first
%clustering step

%% if you want to export csv files:

%% export csv files
csvwrite('MPP.csv',MPP)
%%
csvwrite('MTT.csv',MTT)
%%
csvwrite('MSP.csv',MSP)
%%
csvwrite('MSN.csv',MSN)

%% visualize matrix as "spectrogram like thing over a period of a week"
% 1.  first load in the matrix. ex: "OMatrixMB0202" and save it as a data
% matrix
% 2. seperate out week of interest
week=OmatrixMB0202(3278:3914,3:248) %week of interest, 110-5000 hz
figure 1
image(week,[]);


%% SKIPPPPP step 2Co: ONLY DO IF JUST WANT UP TO 1010 HZ RATHER THAN 5000 Hz
%SKIPPPPPP!!!!!
MSP=MSP(1:end,1:46);
f=110:20:1010 %creates the frequency range by 20 hz (change middle and end values based on what you have in matrix)

%for just up to 1010 hz:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%