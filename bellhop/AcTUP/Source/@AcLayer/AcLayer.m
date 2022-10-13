function Obj = AcLayer(LayerData)
%Constructor for acoustic layer class.
%LayerData is a structure containing the parameters for the layer.  Fields are:
%  Name         =  string specifying name of layer
%  IsHalfSpace  =  0 for a normal layer, 1 if the layer is the bottom acousto-elastic halfspace
%  RMSRough     =  RMS roughness of top interface of layer (m)
%The following vectors must be the same length or [] where indicated.  They should be length 1 if IsHalfSpace == 1.
%  Z            =  Vector of Z coordinates (m, positive down, measured from top of layer)
%  Cp           =  Compressional sound speed (m/s)
%  Cs           =  Shear sound speed (m/s).  Use [] for all Cs = 0.
%  Rho          =  density (kg.m^-3).
%  Ap           =  Compressional wave absorption (dB/wavelength).  Use [] for all Ap=0.
%  As           =  Shear wave absorption (dB/wavelength).  Use [] for all As=0.
%
%%Alec J Duncan, 
%Centre for Marine Science and Technology, 
%Curtin University of Technology,
%Kent Street, Bentley, Western Australia
%a.duncan@cmst.curtin.edu.au
%
%May 2002

if nargin==0
   Obj.Name = '';
   Obj.IsHalfSpace = [];
   Obj.RMSRough = [];
   Obj.ZTop = [];
   Obj.Z = [];
   Obj.Cp = [];
   Obj.Cs = [];
   Obj.Rho = [];
   Obj.Ap = [];
   Obj.As = [];
   
   Obj = class(Obj, 'AcLayer');
   
elseif isa(LayerData, 'AcLayer')
   Obj = LayerData;
else
   Obj.Name = LayerData.Name;
   Obj.IsHalfSpace = LayerData.IsHalfSpace;
   Obj.RMSRough = LayerData.RMSRough;
   Obj.ZTop = [];  %This is only included for compatibility with earlier versions and isn't used
   Obj.Z = LayerData.Z;
   Obj.Cp = LayerData.Cp;
   Obj.Cs = LayerData.Cs;
   Obj.Rho = LayerData.Rho;
   Obj.Ap = LayerData.Ap;
   Obj.As = LayerData.As;
   
   Obj = class(Obj, 'AcLayer');
   
   [Str, Obj] = CheckOK(Obj);
   if ~strcmp(Str, 'OK')
      warndlg(Str, 'AcLayer - Incompatible acoustic layer parameters:');
      uiwait(h);

   end
end
