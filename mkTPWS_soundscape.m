inDir = 'E:\Downloads\soundcoop*.nc';
outFile = 'E:\Data\soundscape_test\test_TPWS1.mat';
fList = dir(inDir); 
MSP = []; % initialize empty
MTT = [];
MDEP = {};
for iFile =1:length(fList)% iterate over NC files
    
    thisFile = fullfile(fList(iFile).folder,fList(iFile).name);
    % ncFInfo = ncinfo(thisFile); % 
    f = ncread(thisFile,'frequency');% temporary oversimplification can't assume that this will be the same across deployments, 
    myPSD = ncread(thisFile,'psd');
    
    % a guess at interpreting the time number, close but probably not right:
    myTime = ((double(ncread(thisFile,'time'))/24/60/60)) + datenum([1970,0,00,0,0,0]);
    MSP = [MSP;myPSD']; % will have to pad with zeros or truncate if frequency range differs between datasets
    % should we truncate based on quality flag variable
        
    
    myProject = ncreadatt(thisFile,'/','project'); % really need a deployment ID here, this is temporary solution

    MTT = [MTT;myTime]; 
    MDEP = vertcat(MDEP,cellstr(repmat(myProject, size(myTime))));
    % Might be a good idea to compute averages and variance or something
    % Could put other values into other matrices. Time is the overall
    % index, but 
    
    
end


% badRows = find(sum(MSP,2)==0); might want to prune out nans or other bad
% spectra here
MSN = ones(size(MTT));
MPP = ones(size(MTT));
save(outFile,'MTT','MPP','MSP','MSN','MDEP','f','-v7.3');% might want to save other identifying info in this file?

