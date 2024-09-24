clear all
close all

inDir = "F:\SoundCoop\hmd_downloadedGCP\AU_CH01\"; 
outDir = 'G:\.shortcut-targets-by-id\1QAlmQwj6IS-J6Gw2PRNQR6jz_4qA5CYZ\SoundCoop_AcousticScene\ClusterAnalysis\A_inputTPWS';
if ~isdir(outDir)
    mkdir(outDir)
end
dirList = dir(inDir);

fminHz = 10;
fmaxHz = 2000;
minQuality = 2; % Made this 2 to include unverified, but could be 1 to only include "good".
iDnum = 1;

%%
for iDir = 8:8 % 3:length(dirList)
    if strcmp(dirList(iDir).name,'.') || strcmp(dirList(iDir).name,'..')
        continue
    end
    inFileList = dir(fullfile(dirList(iDir).folder,dirList(iDir).name,'\*.nc'));
    MSP = []; % initialize empty
MTT = [];
MDEP = {};
    for iFile =1:length(inFileList) % iterate over NC files
        
        thisFile = fullfile(inFileList(iFile).folder,inFileList(iFile).name);
        % ncFInfo = ncinfo(thisFile); %
        f = ncread(thisFile,'frequency');% temporary oversimplification can't assume that this will be the same across deployments,
        myPSD = ncread(thisFile,'psd');
        qualityMat = double(ncread(thisFile,'quality_flag'));
        
        [~,fminIdx] = (min(abs(f-fminHz)));
        [~,fmaxIdx] = (min(abs(f-fmaxHz)));
        f = f(fminIdx:fmaxIdx);
        
        myTime = ((double(ncread(thisFile,'time'))/24/60/60)) + datenum([1970,1,1,0,0,0]);
        
        qualityMat(qualityMat>minQuality)=NaN;
        % save data so we can go back with different quality??
        MSP = [MSP;(myPSD(fminIdx:fmaxIdx,:).*qualityMat(fminIdx:fmaxIdx,:))']; % will have to pad with zeros or truncate if frequency range differs between datasets
        % should we truncate based on quality flag variable
        
        myProject = inFileList(iFile).name; % using folder name as project ID
        
        MTT = [MTT;myTime];
        MDEP = vertcat(MDEP,cellstr(repmat(myProject, size(myTime))));
        % Might be a good idea to compute averages and variance or something
        % Could put other values into other matrices. Time is the overall
        % index
        
        if size(MTT,1)>= 300000
            % file's getting big, save and start a new one.
            
            MSN = ones(size(MTT));
            MPP = ones(size(MTT));
            outFile =  fullfile(outDir,[dirList(iDir).name,'_part',num2str(iDnum),'_test_TPWS1.mat']);
            
            save(outFile,'MTT','MPP','MSP','MSN','MDEP','f','-v7.3');
            % might want to save other identifying info in this file?
            iDnum = iDnum+1;
            
            MSP = []; % initialize empty
            MTT = [];
            MDEP = {};
        end
    end
        
    % badRows = find(sum(MSP,2)==0); might want to prune out nans or other bad
    % spectra here
    MSN = ones(size(MTT,1),2);
    MPP = ones(size(MTT));
    outFile =  fullfile(outDir,[dirList(iDir).name,'_part',num2str(iDnum),'_TPWS1.mat']);
    
    save(outFile,'MTT','MPP','MSP','MSN','MDEP','f','-v7.3');
    % might want to save other identifying info in this file?
    iDnum = 1;
end


