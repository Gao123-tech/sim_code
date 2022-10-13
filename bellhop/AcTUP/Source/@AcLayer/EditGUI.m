function Obj = EditGUI(Obj)
%Allows interactive editing of an acoustic layer object
Prompt1 = {...
      'Layer name', ...
      'Is layer a bottom half-space (y/n)', ...
      'RMS roughness of top interface of layer (m)', ...
      'Read acoustic data from file (y/n)' ...
  };

Prompt2 = {...
		'Vector of Z coordinates (m, positive down, measured from top of layer)', ...
		'Vector of compressional sound speeds (m/s)', ...
		'Vector of densities (kg.m^-3)', ...
		'Vector of shear sound speeds (m/s).  Use [] for all Cs = 0.', ...
		'Vector of compressional wave absorption (dB/wavelength).  Use [] for all Ap=0.', ...
        'Vector of shear wave absorption (dB/wavelength).  Use [] for all As=0.' ...
    };

Done = 0;
Cancel = 0;
NewObj = Obj;
while ~Done
   if isempty(NewObj.Z)
		Defaults = {...
   	   '', ...
	      'n', ...
   	   '0.0', ...
       'n'
      	};
   else
	    if NewObj.IsHalfSpace
   		    HSpaceStr = 'y';
		else
   		    HSpaceStr = 'n';
		end
		Defaults = {...
            NewObj.Name, ...
	        HSpaceStr, ...
            num2str(NewObj.RMSRough), ...
            'n' ...
      	    };
   end
   
	Ans = inputdlg(Prompt1, 'Edit layer properties', 1, Defaults);

	if isempty(Ans)
        Done = 1;
        Cancel = 1;
    else
   	    NewObj.Name = Ans{1};
   	    NewObj.IsHalfSpace = strcmpi(Ans{2}, 'y');
   	    NewObj.RMSRough = str2num(Ans{3});
        AcDataFromFile = strcmpi(Ans{4}, 'y');
    end
    
    if ~Cancel
        if AcDataFromFile
            NewObj = lReadAcDataFromFile(Obj);
            if isempty(NewObj)
                Done = 1;
                Cancel = 1;
            end
        else
            if isempty(Obj.Z)
		        Defaults = {...
      	            '[0]', ...
            	    '[1500]', ...
      	            '[1024]', ...
   	                '[0]', ...
	                '[0]', ...
   	                '[0]' ...
      	            };
            else
		        Defaults = {...
      	            lVecToStr(NewObj.Z), ...
	                lVecToStr(NewObj.Cp), ...
      	            lVecToStr(NewObj.Rho), ...
   	                lVecToStr(NewObj.Cs), ...
	                lVecToStr(NewObj.Ap), ...
   	                lVecToStr(NewObj.As) ...
      	            };
            end
            Ans = inputdlg(Prompt2, 'Edit acoustic properties', 1, Defaults);
            if isempty(Ans)
                Done = 1;
                Cancel = 1;
            else
        	    NewObj.Z = str2num(Ans{1});
               	NewObj.Cp = str2num(Ans{2});
               	NewObj.Rho = str2num(Ans{3});
        	    NewObj.Cs = str2num(Ans{4});
	            NewObj.Ap = str2num(Ans{5});
   	            NewObj.As = str2num(Ans{6});
            end
        end
        if ~Cancel
   	        [Str, NewObj] = CheckOK(NewObj);
            if strcmp(Str, 'OK')
                Obj = NewObj;
                Done = 1;
            else
                h = warndlg(Str, 'Incompatible acoustic layer parameters:');
                uiwait(h);
            end
        end
      
    end
   
end


   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Str = lVecToStr(Vec)
Str = ['[' num2str(reshape(Vec, 1, length(Vec))) ']'] ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function NewObj = lReadAcDataFromFile(Obj)
%Reads acoustic data from a file.  This is an ascii text file, each row of file has following columns:
%Depth Cp Density Cs Ap  As
%
%where:
%Depth is in metres
%Cp is compressional sound speed in m/s
%Density is density in kg/m^3
%Cs is shear sound speed in m/s
%Ap is compressional wave absorption in dB/wavelength
%As is shear wave absorption in dB/wavelength
%
%You don't have to specify all columns, any after the last specified will be set to defaults: 
% Cp = 1500m/s, Density = 1024m/s, Cs, Ap, As = 0

persistent DfltDir
if isempty(DfltDir)
    DfltDir = 'C:\';
end

NExpected = 6;
Here = pwd;
cd(DfltDir);
[File, Path] = uigetfile('*.*', 'Sound speed file');
cd(Here);

if File == 0
    NewObj = [];
else
    DfltDir = Path;
    InDat = load([Path File], '-ascii');
    
    Sz = size(InDat);
    NPt = Sz(1);
    NCol = Sz(2);
    
    AllDat = zeros(NPt, NExpected);
    AllDat(:, 2) = 1500;
    AllDat(:, 3) = 1024;
    AllDat(:, 1:NCol) = InDat;
    
    NewObj = Obj;
    NewObj.Z = AllDat(:, 1);
    NewObj.Cp = AllDat(:, 2);
    NewObj.Rho = AllDat(:, 3);
    NewObj.Cs = AllDat(:, 4);
	NewObj.Ap = AllDat(:, 5);
   	NewObj.As = AllDat(:, 6);
end
    
        
    

