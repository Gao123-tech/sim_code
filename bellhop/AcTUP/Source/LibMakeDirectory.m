function Status = LibMakeDirectory(FullPath)
%Makes the specified directory and any required parent directories

%Alec Duncan, May 2002

%Make sure there is a trailing \
if FullPath(end) ~= '\';
    FullPath = [FullPath '\'];
end

SlashInd = findstr('\', FullPath);

NSlash = length(SlashInd);

ISlash = NSlash;

Done = 0;
ExistingPath = FullPath;
NewDir = [];
NewCount = 0;
Status = 1;

%Work backwards until we find an existing directory
while ~Done
    if exist(ExistingPath, 'dir')
        Done = 1;
    else
        NewCount = NewCount+1;
        if ISlash == 1
            IStart = 0;
        else
            IStart = SlashInd(ISlash - 1);
        end
        
        NewDir{NewCount} = ExistingPath(IStart+1:end);
        
        if ISlash == 1
            ExistingPath = [];
            Done = 1;
        else
            ExistingPath = ExistingPath(1:IStart);
            ISlash = ISlash - 1;
        end
    end
    
 end

if NewCount >= 1
   Here = cd;
   if ~isempty(ExistingPath)
       %Only way I could find to detect the root directory of a non-existent disk was that its directory
       %structure has zero length.  For some reason exist() returns TRUE in this case
       DStruct = dir(ExistingPath);   
       if isempty(DStruct)
          Status = 0;
       else
          cd(ExistingPath);
       end
    end
    
    if Status
        for ICount = NewCount:-1:1
            DirName = NewDir{ICount};
            Status = mkdir(DirName(1:end-1));  %Exclude the trailing \ (V5 doesn't like it)
            if Status
                cd(NewDir{ICount});
            end
        end
    end
    cd(Here);
else
   %Nothing new to create, but check that we don't have a non-existent disk
   if ~isempty(ExistingPath)
      %Only way I could find to detect the root directory of a non-existent disk was that its directory
      %structure has zero length.  For some reason exist() returns TRUE in this case
      DStruct = dir(ExistingPath);   
      if isempty(DStruct)
          Status = 0;
      end
   end
end

    

    
    